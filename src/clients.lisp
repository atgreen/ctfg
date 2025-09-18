;;; clients.lisp
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2025 Anthony Green

(in-package :ctfg)

(defvar *websocket-clients* nil)

(defvar *websocket-client-lock*
  (make-rwlock))                     ; many readers, one writer, writer-prio

(defstruct client
  socket
  lock                                   ; per-socket mutex if you need one
  last-pong-ts                           ; last time (get-universal-time) we saw a PONG
  last-ping-ms)                          ; last PING send time (ms since epoch)

;;; ------------------------------------------------------------------
;;; public helpers
;;; ------------------------------------------------------------------

(defun add-client (client)
  (with-write-lock-held (*websocket-client-lock*)
    (push client *websocket-clients*)
    (log:info "Adding websocket client.  Length = ~A"
              (length *websocket-clients*)))
  t)

(defun remove-client (client)
  (log:info "About to remove websocket client.  Length = ~A"
            (length *websocket-clients*))
  (with-write-lock-held (*websocket-client-lock*)
    (setf *websocket-clients*
          (delete client *websocket-clients*
                  :key  #'client-socket
                  :test #'equal)))
  (log:info "Removed websocket client.  Length = ~A"
            (length *websocket-clients*))
  t)

(defun get-client-list ()
  (with-read-lock-held (*websocket-client-lock*)
    ;; return a snapshot so callers can traverse without holding the lock
    (copy-list *websocket-clients*)))
