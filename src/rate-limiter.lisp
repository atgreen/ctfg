;;; rate-limiter.lisp
;;;
;;; SPDX-License-Identifier: MIT
;;; Copyright (C) 2025 Anthony Green

(in-package :ctfg)

;;;; --------------------------------------------------------------------------
;;;;  Token bucket rate limiter implementation
;;;; --------------------------------------------------------------------------

(defstruct rate-limit-bucket
  "Token bucket for rate limiting"
  (tokens 0 :type integer)
  (last-refill 0 :type integer)
  (lock (make-rwlock) :type rwlock))

(defstruct rate-limiter
  "Rate limiter configuration and state"
  (max-tokens 10 :type integer)          ; Max tokens in bucket
  (refill-rate 1 :type number)           ; Tokens per second
  (buckets (lh:make-castable) :type lh:castable)) ; user-key -> bucket

(defun make-rate-limit-key (user endpoint)
  "Create a unique key for user+endpoint combination"
  (format nil "~A:~A" (user-id user) endpoint))

(defun refill-bucket (bucket max-tokens refill-rate now)
  "Refill tokens based on elapsed time. Returns updated token count."
  (let* ((elapsed-ms (- now (rate-limit-bucket-last-refill bucket)))
         (elapsed-secs (/ elapsed-ms 1000.0))
         (new-tokens (floor (* elapsed-secs refill-rate)))
         (total-tokens (min max-tokens 
                           (+ (rate-limit-bucket-tokens bucket) new-tokens))))
    (when (> new-tokens 0)
      (setf (rate-limit-bucket-tokens bucket) total-tokens)
      (setf (rate-limit-bucket-last-refill bucket) now))
    total-tokens))

(defun consume-token-p (limiter user-key)
  "Try to consume a token. Returns T if successful, NIL if rate limited."
  (let* ((now (floor (now-micros) 1000)) ; Convert to milliseconds
         (bucket (or (lh:gethash user-key (rate-limiter-buckets limiter))
                     (let ((new-bucket (make-rate-limit-bucket
                                       :tokens (rate-limiter-max-tokens limiter)
                                       :last-refill now)))
                       (lh:put-if-absent (rate-limiter-buckets limiter) user-key new-bucket)
                       (lh:gethash user-key (rate-limiter-buckets limiter))))))
    (with-write-lock-held ((rate-limit-bucket-lock bucket))
      (let ((tokens (refill-bucket bucket 
                                   (rate-limiter-max-tokens limiter)
                                   (rate-limiter-refill-rate limiter)
                                   now)))
        (if (>= tokens 1)
            (progn
              (decf (rate-limit-bucket-tokens bucket))
              t)
            nil)))))

;;;; --------------------------------------------------------------------------
;;;;  Global rate limiters for different endpoints
;;;; --------------------------------------------------------------------------

(defparameter *submit-rate-limiter*
  (make-rate-limiter :max-tokens 10 :refill-rate 0.5)
  "Rate limiter for flag submissions: 10 attempts, refill 1 every 2 seconds")

(defparameter *hint-rate-limiter*
  (make-rate-limiter :max-tokens 20 :refill-rate 1)
  "Rate limiter for hint purchases: 20 attempts, refill 1 per second")

(defparameter *api-rate-limiter*
  (make-rate-limiter :max-tokens 100 :refill-rate 10)
  "General API rate limiter: 100 requests, refill 10 per second")

(defun check-rate-limit (limiter user endpoint)
  "Check if request is allowed. Returns T if allowed, NIL if rate limited."
  (let ((key (make-rate-limit-key user endpoint)))
    (consume-token-p limiter key)))

;;;; --------------------------------------------------------------------------
;;;;  Cleanup old buckets periodically (prevent memory leak)
;;;; --------------------------------------------------------------------------

(defun cleanup-old-buckets (limiter max-age-ms)
  "Remove buckets that haven't been used in max-age-ms milliseconds"
  (let ((now (floor (now-micros) 1000))
        (keys-to-remove '()))
    ;; Collect old keys
    (lh:maphash (lambda (key bucket)
                  (with-read-lock-held ((rate-limit-bucket-lock bucket))
                    (when (> (- now (rate-limit-bucket-last-refill bucket)) max-age-ms)
                      (push key keys-to-remove))))
                (rate-limiter-buckets limiter))
    ;; Remove old keys
    (dolist (key keys-to-remove)
      (lh:remhash key (rate-limiter-buckets limiter)))
    (length keys-to-remove)))

(defparameter *rate-limit-cleanup-thread* nil)

(defun start-rate-limit-cleanup ()
  "Start background thread to clean up old rate limit buckets"
  (unless (and *rate-limit-cleanup-thread*
               (bt2:thread-alive-p *rate-limit-cleanup-thread*))
    (setf *rate-limit-cleanup-thread*
          (bt2:make-thread
           (lambda ()
             (handler-bind ((error (lambda (c)
                                     (format *error-output* "Error in thread ~A: ~A~%"
                                             (bt:current-thread) c)
                                     (sb-debug:print-backtrace :count 50 :stream *error-output*)
                                     (finish-output *error-output*))))
               (loop
                 (sleep 300) ; Clean up every 5 minutes
                 (handler-case
                     (progn
                       (cleanup-old-buckets *submit-rate-limiter* (* 30 60 1000))
                       (cleanup-old-buckets *hint-rate-limiter* (* 30 60 1000))
                       (cleanup-old-buckets *api-rate-limiter* (* 30 60 1000)))
                   (error (e)
                     (log:error "Rate limit cleanup error: ~A" e))))))
           :name "rate-limit-cleanup"))))

(defun stop-rate-limit-cleanup ()
  "Stop the rate limit cleanup thread"
  (when (and *rate-limit-cleanup-thread*
             (bt2:thread-alive-p *rate-limit-cleanup-thread*))
    (bt2:destroy-thread *rate-limit-cleanup-thread*)
    (setf *rate-limit-cleanup-thread* nil)))