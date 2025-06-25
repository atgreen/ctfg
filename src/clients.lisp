;;; clients.lisp
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2025 Anthony Green

(in-package :ctfg)

(defvar *websocket-clients* (list))
(defvar *websocket-client-lock* (rwlock:make-rwlock))

(defstruct client
  socket lock)

(defun add-client (client)
  (rwlock:with-write-lock-held *websocket-client-lock*
    (push client *websocket-clients*)
    (log:info "Adding websocket client. Length = ~A" (length *websocket-clients*)))
  t)

(defun remove-client (client)
  (log:info "About to remove websocket client. Length = ~A" (length *websocket-clients*))
  (rwlock:with-write-lock-held *websocket-client-lock*
    (setf *websocket-clients*
          (delete-if (lambda (c)
                       (equal client (client-socket c)))
                     *websocket-clients*)))
  (log:info "Removed websocket client. Length = ~A" (length *websocket-clients*))
  t)

(defun get-client-list ()
  (rwlock:with-read-lock-held *websocket-client-lock*
    (copy-list *websocket-clients*)))
