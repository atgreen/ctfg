;;; rwlock.lisp
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2025 Anthony Green

(in-package #:ctfg)

(defstruct rwlock
  ;; internal mutex protecting counters and CVS
  (mutex        (bt:make-lock))
  ;; number of active readers
  (readers      0)
  ;; number of writers waiting to acquire the lock
  (writers-waiting 0)
  ;; true when one thread holds the write lock
  (writer-active  nil)
  ;; condition vars
  (read-cv   (bt:make-condition-variable))
  (write-cv  (bt:make-condition-variable)))

(defun %wait (cv lock) (bt:condition-wait cv lock))

(defun %signal (cv) (bt:condition-notify cv))

(defun %broadcast (cv) (bt2:condition-broadcast cv))

(defun acquire-read-lock (rw)
  (bt:with-lock-held ((rwlock-mutex rw))
    ;; Block while a writer is active **or waiting** (writer priority)
    (loop while (or (rwlock-writer-active rw)
                    (> (rwlock-writers-waiting rw) 0))
          do (%wait (rwlock-read-cv rw) (rwlock-mutex rw)))
    (incf (rwlock-readers rw)))
  (values))

(defun release-read-lock (rw)
  (bt:with-lock-held ((rwlock-mutex rw))
    (decf (rwlock-readers rw))
    ;; If this was the last reader and a writer is queued, wake one writer
    (when (and (zerop (rwlock-readers rw))
               (> (rwlock-writers-waiting rw) 0))
      (%signal (rwlock-write-cv rw))))
  (values))

(defun acquire-write-lock (rw)
  (bt:with-lock-held ((rwlock-mutex rw))
    (incf (rwlock-writers-waiting rw))
    ;; Wait until no other writer is active and no readers are active
    (loop while (or (rwlock-writer-active rw)
                    (> (rwlock-readers rw) 0))
          do (%wait (rwlock-write-cv rw) (rwlock-mutex rw)))
    (decf (rwlock-writers-waiting rw))
    (setf (rwlock-writer-active rw) t))
  (values))

(defun release-write-lock (rw)
  (bt:with-lock-held ((rwlock-mutex rw))
    (setf (rwlock-writer-active rw) nil)
    (cond
      ;; Prefer queued writers, else wake *all* waiting readers
      ((> (rwlock-writers-waiting rw) 0)
       (%signal (rwlock-write-cv rw)))
      (t
       (%broadcast (rwlock-read-cv rw)))))
  (values))

;;; ------------------------------------------------------------------
;;; convenience macros
;;; ------------------------------------------------------------------

(defmacro with-read-lock-held ((rw) &body body)
  `(progn
     (acquire-read-lock ,rw)
     (unwind-protect
          (locally ,@body)
       (release-read-lock ,rw))))

(defmacro with-write-lock-held ((rw) &body body)
  `(progn
     (acquire-write-lock ,rw)
     (unwind-protect
          (locally ,@body)
       (release-write-lock ,rw))))
