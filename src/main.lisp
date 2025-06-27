;;; main.lisp
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2025 Anthony Green

(in-package #:ctfg)

;; When *developer-mode* is t , enable developer features, such as
;; disabling caching of static content in the browser.
(defvar *developer-mode* nil)

(defparameter *challenges-path* nil)
(defparameter *control-cluster* nil)
(defparameter *player-clusters* nil)
(defparameter *dbdir* nil)

(defun make-app ()
  (let ((p (clingon:make-option :integer :short-name #\p :long-name "port" :key :port
                                         :description "port" :initial-value 8080))
        (s (clingon:make-option :integer :short-name #\s :long-name "slynk-port" :key :slynk-port
                                         :description "slynk-port" :initial-value nil))
        (b (clingon:make-option :string :long-name "dbdir" :key :dbdir
                                        :description "database directory" :initial-value "."))
        (d (clingon:make-option :flag :short-name #\d :long-name "developer-mode" :key :developer-mode
                                      :description "enable developer mode" :initial-value nil)))
    (clingon:make-command
     :name    "ctfg"
     :version +version+
     :description "A web application"
     :authors (list "Anthony Green")
     :license "MIT"
     :usage "[challenges json file]"
     :options (list p s b d)
     :handler (lambda (cmd)
                (let* ((positional-args (clingon:command-arguments cmd))
                       (json-path (first positional-args))
                       (port (clingon:getopt cmd :port))
                       (dbdir (clingon:getopt cmd :dbdir))
                       (slynk-port (clingon:getopt cmd :slynk-port)))
                  (setf *challenges-path* json-path)
                  (setf *dbdir* (concatenate 'string dbdir "/"))
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

(defmacro fatal-error (&rest rest)
  `(progn
     (log:error ,@rest)
     (uiop:quit 1)))

(define-condition malformed-game-clusters-yaml (error)
  ())

(defun main ()
  "The main entrypoint."

  ;; Load environment variables if .env exists.
  (let ((.env-pathname (merge-pathnames ".env")))
    (handler-case
        (.env:load-env .env-pathname)
      (file-error (_)
        (declare (ignore _)))
      (.env:malformed-entry (_)
        (declare (ignore _))
        (fatal-error "Malformed entry in ~S" .env-pathname))
      (.env:duplicated-entry (_)
        (declare (ignore _))
        (fatal-error "Duplicated entry in ~S" .env-pathname))))

  (handler-case
      (let ((inventory (cl-yaml:parse (uiop:read-file-string "game-clusters.yaml")
                                      :multi-document-p t)))
        (maphash (lambda (key value)
                   (cond
                     ((string= "control_cluster" key)
                      (setf *control-cluster* value)
                      (log:info "Control Cluster = ~A" value))
                     ((string= "player_clusters" key)
                      (setf *player-clusters* value)
                      (log:info "Player Clusters = ~A" value))
                     (t
                      (error 'malformed-game-clusters-yaml))))
                 (cadr inventory))
        (unless (and *control-cluster* *player-clusters*)
          (error 'malformed-game-clusters-yaml)))
    (malformed-game-clusters-yaml (_)
      (fatal-error "Malformed game-clusters.yaml"))
    (file-error (_)
      (fatal-error "Can't read game-clusters.yaml")))

  (handler-case
      (clingon:run (make-app))
    (error (e)
      (format *error-output* "Error: ~A~%" e)
      (uiop:quit 1))))
