;;; server.lisp
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2025 Anthony Green

(in-package :ctfg)

(version-string:define-version-parameter +version+ :ctfg)

(defvar *db* nil)

;; ----------------------------------------------------------------------------
;; Machinery for managing the execution of the server.
(defvar *shutdown-cv* (bt:make-condition-variable))
(defvar *server-lock* (bt:make-lock))
(defvar *acceptor* nil)

(defun respond-unauthorized ()
  (setf (content-type*) "application/json"
        (return-code*)  401)
  (finish-output)
  (write-string "{\"error\":\"unauthorized\"}" *reply*))

;;; ------------------------------------------------------------
;;; Macro: WITH-AUTHENTICATED-USER
;;; ------------------------------------------------------------
(defmacro with-authenticated-user ((var) &body body)
  "Bind VAR to the username stored in the current session and run BODY.
   If the client has no valid session, immediately return HTTP 401."
  `(let ((,var (hunchentoot:session-value :user)))
     (if ,var
         (progn ,@body)
         (respond-unauthorized))))

;; Easy-routes setup
(defclass my-acceptor (easy-routes:easy-routes-acceptor)
  ())

(defun app-root ()
  (uiop:getcwd))

(defun dev/no-cache-callback (file content-type)
  "Executed right before HANDLE-STATIC-FILE streams FILE.
   We ignore the arguments and just tell the browser not to cache."
  (declare (ignore file content-type))
  (when *developer-mode*
    (hunchentoot:no-cache)))

(defparameter +static-dispatch-table+
  (list
   (hunchentoot:create-folder-dispatcher-and-handler
    "/images/" (fad:pathname-as-directory
                (merge-pathnames "static/images/" (app-root)))
    "application/octet-stream" #'dev/no-cache-callback)
   (hunchentoot:create-folder-dispatcher-and-handler
    "/js/" (fad:pathname-as-directory
            (merge-pathnames "static/js/" (app-root)))
    "application/javascript" #'dev/no-cache-callback)
   (hunchentoot:create-folder-dispatcher-and-handler
    "/css/" (fad:pathname-as-directory
             (merge-pathnames "static/css/" (app-root)))
    "text/css" #'dev/no-cache-callback)))

(defparameter +index.html+ #.(uiop:read-file-string "src/index.html"))
(defparameter +challenges.json+ #.(uiop:read-file-string "src/challenges.json"))

;;; Convenience --------------------------------------------------------------
(defun json-body ()
  "Read the POST body of the current request and return it as a Lisp object."
  (let ((raw (hunchentoot:raw-post-data :force-text t)))
    (when raw
      (cl-json:decode-json-from-string raw))))

(defun respond-json (obj &key (code 200))
  (setf (hunchentoot:content-type*) "application/json"
        (hunchentoot:return-code*) code)
  (cl-json:encode-json-to-string obj))

(defvar *credentials* (make-hash-table :test #'equal))

(defmethod read-credentials ()
  "Parse credentials csv file."
  (let* ((csv (cl-csv:read-csv
               (uiop:read-file-string "credentials.csv")
               :trim-outer-whitespace t)))
    (dolist (row csv)
      (when (not (string= "username" (car row)))
        (setf (gethash (car row) *credentials*) (cadr row))))
    (log:info "Read ~A credentials from credentials.csv" (hash-table-count *credentials*))))

(easy-routes:defroute login ("/api/login" :method :post) ()
  "User login"
  (let* ((body (json-body))
         (username (cdr (assoc :username body)))
         (password (cdr (assoc :password body))))
    (if (and password (equal password (gethash username *credentials*)))
        (progn
          ;; create or reuse session
          (hunchentoot:start-session)
          (let ((user (ensure-user *db* username)))
          (setf (hunchentoot:session-value :user) user)
          (let ((needs-name (null (user-displayname user))))
            (respond-json
             `((:displayname . ,(user-displayname user)) (:needs_name . ,needs-name))))))
        (respond-json '((:error "invalid_credentials")) :code 401))))

(easy-routes:defroute logout ("/api/logout" :method :post) ()
  (with-authenticated-user (user)
    (hunchentoot:remove-session hunchentoot:*session*)
    (setf (hunchentoot:return-code*) hunchentoot:+http-no-content+)
    ""))

(easy-routes:defroute me ("/api/me" :method :get) ()
  "Quick identifier API"
  (let ((user (hunchentoot:session-value :user)))
    (if user
        (respond-json `((:username . ,(user-username user))
                        (:displayname . ,(user-displayname user))
                        (:needs_name . ,(null (user-displayname user)))))
        (respond-json '((:error . "no session")) :code 401))))

(defun user-solved-p (user cid)
  nil)

(defun award-points (user challenge)
  (log:info "award points")
  (let* ((ts (record-flag *db* user challenge))
         (msg (format nil "{ \"displayname\": ~S, \"ts\": ~A, \"challenge\": ~S, \"points\": ~A }"
                      (user-displayname user)
                      (floor ts 1000)
                      (challenge-title challenge)
                      (challenge-points challenge))))
    (log:info msg)
    (push (challenge-id challenge) (gethash (user-id user) *solves-table*))
    (let ((clients nil))
      (rwlock:with-read-lock-held *websocket-client-lock*
        (setf clients (copy-list *websocket-clients*)))
      (dolist (client clients)
        (log:info "sending to " client)
        (ws:write-to-client-text client msg)))))

(easy-routes:defroute set-name ("/api/set-name" :method :post) ()
  "Set your display name"
  (with-authenticated-user (user)
    (let* ((body   (json-body))
           (name   (cdr (assoc :name body))))
      (log:info "Setting displayname for player ~A to ~A: " (user-username user) name)
      (set-displayname *db* user name)
      "")))

(easy-routes:defroute submit ("/api/submit" :method :post) ()
  "Submit a flag"
  (log:info "SUBMIT!")
  (with-authenticated-user (user)
    (log:info "submitting")
    (let* ((body   (json-body))
           (cid    (cdr (assoc :id body)))
           (guess  (string-trim '(#\Space #\Tab #\Newline) (cdr (assoc :flag body))))
           (chal   (find cid *all-challenges* :key #'challenge-id))
           (solved (user-solved-p user cid)))
      (log:info chal)
      (log:info guess)
      (log:info (challenge-flag chal))
      (cond
        ((null chal)
         (respond-json '(:error "unknown_id") :code 400))
        (solved
         (respond-json `(:result "already" :total ,(user-total-points user))))
        ((string= guess (challenge-flag chal))
         (log:info "Correct!")
         (award-points user chal)
         (respond-json `((:result . "correct")
                         (:points . ,(challenge-points chal))
                         (:total . ,(user-total-points user)))))
        (t
         (log:info "Incorrect!")
         (respond-json '((:result . "incorrect"))))))))


(easy-routes:defroute challenges ("/api/challenges" :method :get) ()
  "Challenges"
  (let ((user (hunchentoot:session-value :user)))
    (log:info "Computing challenges for user: " (user-username user))
    (setf (hunchentoot:content-type*) "application/json")
    (let ((solves (gethash (user-id user) *solves-table*)))
      (log:info "Solves for user ~A: ~A" (user-displayname user) solves)
      (let ((challenges (available-challenges solves)))
        (let ((json-data (mapcar (lambda (challenge)
                                   (list (cons "id" (challenge-id challenge))
                                         (cons "title" (challenge-title challenge))
                                         (cons "category" (challenge-category challenge))
                                         (cons "difficulty" (challenge-difficulty challenge))
                                         (cons "points" (challenge-points challenge))
                                         (cons "description" (challenge-description challenge))
                                         (cons "content" (challenge-content challenge))))
                                 challenges)))
          (let ((s (cl-json:encode-json-to-string json-data)))
            ;; (format t "~&~A~%" s)
            s))))))

(easy-routes:defroute index ("/" :method :get) ()
  "Main index page"
  (setf (hunchentoot:content-type*) "text/html")
  +index.html+)

(defclass scorestream-resource (ws:ws-resource)
  ())

(ws:register-global-resource "/scorestream"
                             (make-instance 'scorestream-resource)
                             #'ws::any-origin)
;;                             (ws:origin-prefix "http://127.0.0.1" "http://localhost" "null"))

(defvar *solves-table* (make-hash-table))

(defun send-events (client)
  (let ((events (collect-events-since *db* 0)))
    (dolist (event events)
      (let* ((name-pair (get-user-name-pair (event-user-id event)))
             (event-username (car name-pair))
             (event-displayname (cdr name-pair))
             (dummy (log:info name-pair))
             (msg (format nil "{ \"displayname\": ~S, \"ts\": ~A, \"challenge_id\": ~A, \"challenge\": ~S, \"points\": ~A }"
                          event-displayname
                          (floor (event-ts event) 1000)
                          (event-challenge-id event)
                          (challenge-title (find (event-challenge-id event) *all-challenges* :key #'challenge-id))
                          (event-points event))))
        (push (event-challenge-id event) (gethash (event-user-id event) *solves-table*))
        (ws:write-to-client-text client msg)))))

(defvar *websocket-clients* (list))
(defvar *websocket-client-lock* (rwlock:make-rwlock))

(defmethod ws:resource-client-connected ((res scorestream-resource) client)
  (log:info "got connection on scorestream server from ~s : ~s~%" (ws:client-host client) (ws:client-port client))
  (rwlock:with-write-lock-held *websocket-client-lock*
    (push client *websocket-clients*))
  (send-events client)
  t)

(defmethod ws:resource-client-disconnected ((resource scorestream-resource) client)
  (format t "Client disconnected from resource ~A: ~A~%" resource client))

(defmethod ws:resource-received-text ((res scorestream-resource) client message)
  (format t "got frame ~s from client ~s" message client)
  (ws:write-to-client-text client message))

(defmethod ws:resource-received-binary((res scorestream-resource) client message)
  (format t "got binary frame ~s from client ~s" (length message) client)
  (ws:write-to-client-binary client message))

(defun start-server (port)
  "Start the web application with easy-routes."
  (setf hunchentoot:*catch-errors-p* t)
  (setf hunchentoot:*show-lisp-errors-p* t)
  (setf hunchentoot:*show-lisp-backtraces-p* t)
  (setf hunchentoot:*session-max-time* most-positive-fixnum)

  (setf *db* (make-instance 'db/sqlite :filename "events.db"))

  (log:info "Static content directory: ~Astatic" (uiop:getcwd))
  (log:info "Starting server version ~A on port ~A" +version+ port)

  ;; Set up static file handlers in the global dispatch table
  (setf hunchentoot:*dispatch-table* +static-dispatch-table+)

  (read-credentials)
  (read-challenges +challenges.json+)

  (let ((events (collect-events-since *db* 0)))
    (dolist (event events)
      (push (event-challenge-id event) (gethash (event-user-id event) *solves-table*))))

  (bordeaux-threads:make-thread (lambda ()
                                  (ws:run-server 12345))
                                :name "websockets server")

  (bordeaux-threads:make-thread (lambda ()
                                  (ws:run-resource-listener
                                   (ws:find-global-resource "/scorestream")))
                                :name "resource listener for /scorestream")

  ;; Create and start the easy-routes acceptor
  (setf *acceptor* (make-instance 'my-acceptor :port port))
  (hunchentoot:start *acceptor*)
  (log:info "Server started successfully on port ~A" port)
  *acceptor*)
