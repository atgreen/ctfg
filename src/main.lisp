;;; main.lisp
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2025 Anthony Green

(in-package #:ctfg)

;; When *developer-mode* is t , enable developer features, such as
;; disabling caching of static content in the browser.
(defvar *developer-mode* nil)

(defun make-app ()
  (let ((p (clingon:make-option :integer :short-name #\p :long-name "port" :key :port
                                         :description "port" :initial-value 8080))
        (s (clingon:make-option :integer :short-name #\s :long-name "slynk-port" :key :slynk-port
                                         :description "slynk-port" :initial-value nil))
        (d (clingon:make-option :flag :short-name #\d :long-name "developer-mode" :key :developer-mode
                                      :description "disable caching" :initial-value nil)))
    (clingon:make-command
     :name    "ctfg"
     :version +version+
     :description "A web application"
     :authors (list "Anthony Green")
     :license "MIT"
     :usage ""
     :options (list p s d)
     :handler (lambda (cmd)
                (let ((port (clingon:getopt cmd :port))
                      (slynk-port (clingon:getopt cmd :slynk-port)))
                  (bt:with-lock-held (*server-lock*)
                    (setf *developer-mode* (clingon:getopt cmd :developer-mode))
                    ;; Create the slynk server.  Allow connections from anywhere.
                    (when slynk-port
                      (slynk:create-server :port slynk-port :interface "0.0.0.0" :dont-close t)
                      (log:info "Started slynk server on port ~A" slynk-port))
                    (start-server port)
                    (log:info "Waiting for connections...")
                    ;; Wait forever.
                    (bt:condition-wait *shutdown-cv* *server-lock*))))
     :examples '(("Run web service on port 9090:"
                  . "ctfg -p 9090")))))

(defun main ()
  "The main entrypoint."
  (handler-case
      (clingon:run (make-app))
    (error (e)
      (format *error-output* "Error: ~A~%" e)
      (uiop:quit 1))))
