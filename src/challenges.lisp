;;; challenges.lisp
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2025 Anthony Green

(in-package :ctfg)

(defvar *all-challenges* nil)

(defstruct challenge
  id title category difficulty points description content flag hints requirements)

(defun read-challenges (challenges.json)
  (let ((clist (list)))
    (let ((raw (cl-json:decode-json-from-string challenges.json)))
      (dolist (challenge raw)
        (push (make-challenge
               :id          (cdr (assoc :id challenge))
               :title       (cdr (assoc :title challenge))
               :category    (cdr (assoc :category challenge))
               :difficulty  (cdr (assoc :difficulty challenge))
               :points      (cdr (assoc :points challenge))
               :description (cdr (assoc :description challenge))
               :content     (cdr (assoc :content challenge))
               :flag        (cdr (assoc :flag challenge))
               :hints       (cdr (assoc :hints challenge))
               :requirements (cdr (assoc :requirements challenge)))
              clist)))
    (log:info (format nil "Read ~A challenges." (length clist)))
    (setf *all-challenges* clist)))

(defun available-challenges (solves)
  "Give a list of solved challenge IDs (SOLVES), return
the list of challenges available."
  (log:info (format nil "SOLVES: ~A" solves))
  (loop for challenge in *all-challenges*
        when (subsetp (challenge-requirements challenge) solves :test #'=)
          collect challenge))
