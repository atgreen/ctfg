;;; server.lisp
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2025 Anthony Green

(in-package :ctfg)

(version-string:define-version-parameter +version+ :ctfg)

(defvar *db* nil)
(defvar *sentry-dsn* nil)

(defparameter *websocket-url* nil)

(defun capture-exception (e)
  (when *sentry-dsn*
    (sentry-client:capture-exception e)))

(defparameter *challenges-path* nil)

(defun load-challenges ()
  "Read challenges.json from disk, respecting *developer-mode*.
   In production we call this only once at start-up; in developer
   mode we may call it on every page/API request."
  (let* ((default-file "challenges.json")
         (json-file (or *challenges-path* default-file)))
    (when (or *developer-mode*             ; always reload in dev
              (null *all-challenges*))     ; first load in prod
      (log:debug "Reloading challenges from ~A" json-file)
      (read-challenges (uiop:read-file-string json-file)))))

(defun save-solve (key value)
  (loop
    for current-caslist := (lh:gethash key *solves-table*)
    do (cond
         (current-caslist
          ;; If a caslist already exists, try to push to it
          (ll:push value current-caslist)
          (return t)) ; Push successful
         (t
          ;; If no caslist exists, try to add a new one
          (let ((new-caslist (ll:caslist value)))
            (when (lh:put-if-absent *solves-table* key new-caslist)
              (return t)))) ; Successfully added new caslist
          )))

;; ----------------------------------------------------------------------------
;; Machinery for managing the execution of the server.
(defvar *shutdown-cv* (bt:make-condition-variable))
(defvar *server-lock* (bt:make-lock))
(defvar *acceptor* nil)

(defun respond-unauthorized ()
  (setf (hunchentoot:content-type*) "application/json"
        (hunchentoot:return-code*)  401)
  (hunchentoot:no-cache)
  "{\"error\":\"unauthorized\"}")

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

(defparameter +index.html+ #.(uiop:read-file-string "src/index.html"))

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

(easy-routes:defroute threads ("/debug/threads" :method :get) ()
  (format nil "Threads: ~{~A~%~}"
          (mapcar #'bt:thread-name
                  (bt:all-threads))))

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
             `((:displayname . ,(user-displayname user)) (:needs_name . ,needs-name) (:websocket_url . ,*websocket-url*))))))
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
                        (:needs_name . ,(null (user-displayname user)))
                        (:websocket_url . ,*websocket-url*)))
        (respond-json '((:error . "no session")) :code 401))))

(defun user-solved-p (user cid)
  nil)

(defun award-points (user challenge)
  (log:info "award points")
  (multiple-value-bind (ts event-id)
      (record-flag *db* user challenge)
    (let ((msg (format nil "[{ \"id\": ~A, \"type\": \"score\", \"displayname\": ~S, \"ts\": ~A, \"challenge\": ~S, \"points\": ~A }]"
                       event-id
                       (user-displayname user)
                       (floor ts 1000)
                       (challenge-title challenge)
                       (challenge-points challenge))))
      (log:info msg)
      (save-solve (user-id user) (challenge-id challenge))
      (dolist (client (get-client-list))
        (with-write-lock-held ((client-lock client))
          (ws:write-to-client-text (client-socket client) msg))))))

(easy-routes:defroute set-name ("/api/set-name" :method :post) ()
  "Set your display name"
  (with-authenticated-user (user)
    (let* ((body   (json-body))
           (name   (cdr (assoc :name body))))
      (log:info "Setting displayname for player ~A to ~A: " (user-username user) name)
      (set-displayname *db* user name)
      "")))

(easy-routes:defroute hint ("/api/hint" :method :post) ()
  "Request a hint."
  (log:info "HINT!")
  (with-authenticated-user (user)
    (let* ((body   (json-body))
           (cid    (cdr (assoc :id body)))
           (hid    (cdr (assoc :hint--id body)))
           (chal   (find cid *all-challenges* :key #'challenge-id)))
      (let ((hint (find hid (challenge-hints chal) :key (lambda (h) (cdr (assoc :ID h))))))
        (log:info hint)
        (let ((msg (format nil "[{ \"id\": 1001, \"type\": \"hint\", \"text\": ~S }]"
                           (cdr (assoc :text hint)))))
          (log:info msg)
          (dolist (client (get-client-list))
            (with-write-lock-held ((client-lock client))
              (ws:write-to-client-text (client-socket client) msg))))))
    (respond-json
     `((:result . "ok") (:message . "hint purchase")))))

(easy-routes:defroute submit ("/api/submit" :method :post) ()
  "Submit a flag"
  (with-authenticated-user (user)
    (let* ((body   (json-body))
           (cid    (cdr (assoc :id body)))
           (guess  (string-trim '(#\Space #\Tab #\Newline) (cdr (assoc :flag body))))
           (chal   (find cid *all-challenges* :key #'challenge-id))
           (solved (user-solved-p user cid)))
      (log:info "~A submitting for challenge ~A: ~A"
                (user-displayname user)
                cid
                guess)
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

(defun process-description (user description)
  (setf description
        (cl-ppcre:regex-replace-all "@CONTROL_CLUSTER@" description *control-cluster*))
  (setf description
        (cl-ppcre:regex-replace-all "@USERNAME@" description (user-username user)))
  (setf description
        (cl-ppcre:regex-replace-all "@USERID@" description (format nil "~A" (user-id user))))
  (setf description
        (let ((index (rem (user-id user) (length *player-clusters*))))
          (cl-ppcre:regex-replace-all "@PLAYER_CLUSTER@"
                                      description
                                      (nth index *player-clusters*))))
  description)

(easy-routes:defroute challenges ("/api/challenges" :method :get) ()
  "Challenges"
  (when *developer-mode* (load-challenges))
  (with-authenticated-user (user)
    (let ((user (hunchentoot:session-value :user))
          (events (collect-events-since *db* 0)))
      (log:info "Computing challenges for user: " (user-username user))
      (setf (hunchentoot:content-type*) "application/json")
      (let* ((ll (lh:gethash (user-id user) *solves-table*))
             (solves (if ll (ll:to-list ll) nil)))
        (log:info "Solves for user ~A: ~A" (user-displayname user) solves)
        (let ((challenges (available-challenges solves)))
          (let ((json-data (mapcar (lambda (challenge)
                                     (list (cons "id" (challenge-id challenge))
                                           (cons "title" (challenge-title challenge))
                                           (cons "category" (challenge-category challenge))
                                           (cons "difficulty" (challenge-difficulty challenge))
                                           (cons "points" (challenge-points challenge))
                                           (cons "description"
                                                 (process-description
                                                  user
                                                  (challenge-description challenge)))
                                           (cons "hints" (challenge-hints challenge))
                                           (cons "content" (challenge-content challenge))))
                                   challenges)))
            (cl-json:encode-json-to-string json-data)))))))

(cl-ppcre:regex-replace-all
 "@PLAYER_CLUSTER@" "sdgsdgd@PLAYER_CLUSTER@dfgdfgdf" "XXX")



(easy-routes:defroute index ("/" :method :get) ()
  "Main index page"
  (when *developer-mode* (load-challenges))
  (setf (hunchentoot:content-type*) "text/html")
  +index.html+)

(defclass scorestream-resource (ws:ws-resource)
  ())

(ws:register-global-resource "/scorestream"
                             (make-instance 'scorestream-resource)
                             #'ws::any-origin)

(defvar *solves-table* (lh:make-castable))

(defun send-events (client)
  (let ((events (collect-events-since *db* 0)))
    (bt:make-thread
     (lambda ()
       (let ((json-events
              (loop for event in events
                    collect (format nil "{ \"id\": ~A, \"type\": \"score\", \"displayname\": ~S, \"ts\": ~A, \"challenge_id\": ~A, \"challenge\": ~S, \"points\": ~A }"
                                   (event-id event)
                                   (cdr (get-user-name-pair (event-user-id event)))
                                   (floor (event-ts event) 1000)
                                   (event-challenge-id event)
                                   (challenge-title (find (event-challenge-id event) *all-challenges* :key #'challenge-id))
                                   (event-points event)))))
         (log:info json-events)
         (handler-case
             (progn
               (with-write-lock-held ((client-lock client))
                 (ws:write-to-client-text
                  (client-socket client)
                  (format nil "[~{~a~^,~}]" json-events))) ; Single array message
               (log:info "events sent"))
           (error (e)
             (log:error "Error sending events: ~a" e)
             (capture-exception e))))))))

(defmethod ws:resource-client-connected ((res scorestream-resource) client)
  (log:info "Client connected to scorestream server from ~s : ~s" (ws:client-host client) (ws:client-port client))
  (log:info client)
  (handler-case
      (let ((client (make-client :socket client :lock (make-rwlock))))
        (add-client client)
        (send-events client))
    (error (e)
      (log:error "Error on websocket client connect: ~A" e)
      (capture-exception e))))

(defmethod ws:resource-received-text ((res scorestream-resource) client message)
  ;; The only messages we receive from the client are pings designed
  ;; to keep network devices between the client and server from timing
  ;; out the connection.  We can ignore these.
  (when *developer-mode*
    (log:debug "Received ~S from ~S." message client)))

(defmethod ws:resource-client-disconnected ((resource scorestream-resource) client)
  (log:info "Client disconnected from resource ~A: ~A" resource client)
  (handler-case
      (remove-client client)
    (error (e)
      (log:error "Error on websocket client disconnect: ~A" e)
      (capture-exception e))))

(defmethod hunchentoot:maybe-invoke-debugger :after (condition)
    (when hunchentoot:*catch-errors-p*
      ;; There's an error in trivial-backtrace:map-backtrace in SBCL
      ;; if we don't set sb-debug:*stack-top-hint* to NIL
        (let ((sb-debug:*stack-top-hint* nil))
          (capture-exception condition))))

(defun start-server (port)
  "Start the web application with easy-routes."
  (setf hunchentoot:*catch-errors-p* t)
  (setf hunchentoot:*show-lisp-errors-p* t)
  (setf hunchentoot:*show-lisp-backtraces-p* t)
  (setf hunchentoot:*session-max-time* most-positive-fixnum)

  (setf *sentry-dsn* (uiop:getenv "SENTRY_DENS"))
  (when *sentry-dsn*
    (log:info "Initializing sentry client.")
    (sentry-client:initialize-sentry-client *sentry-dsn*))

  (load-challenges)

  (setf *db* (make-instance 'db/sqlite :filename (merge-pathnames "events.db" *dbdir*)))

  (log:info "Static content directory: ~Astatic" (uiop:getcwd))
  (log:info "Starting server version ~A on port ~A" +version+ port)

  ;; Set up static file handlers in the global dispatch table
  (setf hunchentoot:*dispatch-table*
        (list
         (hunchentoot:create-folder-dispatcher-and-handler
          "/images/" (fad:pathname-as-directory
                      (merge-pathnames "static/images/" (app-root)))
          nil #'dev/no-cache-callback)
         (hunchentoot:create-folder-dispatcher-and-handler
          "/js/" (fad:pathname-as-directory
                  (merge-pathnames "static/js/" (app-root)))
          "application/javascript" #'dev/no-cache-callback)
         (hunchentoot:create-folder-dispatcher-and-handler
          "/css/" (fad:pathname-as-directory
                   (merge-pathnames "static/css/" (app-root)))
          "text/css" #'dev/no-cache-callback)))

  (read-credentials)

  (let ((events (collect-events-since *db* 0)))
    (dolist (event events)
      (save-solve (event-user-id event) (event-challenge-id event))))

  (setf ws:*log-level* nil)
  (setf ws:*debug-on-server-errors* nil)
  (setf ws:*debug-on-resource-errors* nil)
  (setf ws::*max-write-backlog* (* 500 30)) ; 500 players, 30 challenges

  (bordeaux-threads:make-thread
   (lambda ()
     (handler-case
         (ws:run-server 12345)
       (error (e)
         (log:error "WebSocket server crashed: ~A" e)
         (capture-exception e))))
   :name "websockets server")

  (bordeaux-threads:make-thread
   (lambda ()
     (handler-case
         (ws:run-resource-listener
          (ws:find-global-resource "/scorestream"))
       (error (e)
         (log:error "Resource listener crashed: ~A" e)
         (capture-exception e))))
   :name "resource listener for /scorestream")

  ;; Create and start the easy-routes acceptor
  (setf *acceptor* (make-instance 'my-acceptor :port port))
  (hunchentoot:start *acceptor*)
  (log:info "Server started successfully on port ~A" port)
  *acceptor*)
