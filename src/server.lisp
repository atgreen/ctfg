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

;; WebSocket keepalive configuration
(defparameter *ws-keepalive-interval* 15
  "Seconds between server-initiated keepalive pings. Set to 0 to disable.")

;; Keepalive thread control
(defvar *ws-keepalive-thread* nil)
(defvar *ws-keepalive-running* nil)
(defparameter *ws-backlog* 512
  "Listen backlog for CLWS server acceptor. If the vendored CLWS supports a :backlog keyword, it will be used.")
(defparameter *ws-ping-timeout* 45
  "Seconds to wait for a client Pong before considering the connection dead.")
(defparameter *ws-ping-latency-warn-ms* 2000
  "Warn if Ping→Pong latency exceeds this many milliseconds.")
(defparameter *ws-log-ping-latency* nil
  "When non-NIL, log Ping→Pong latency per client.")

;; WebSocket access token management (for authenticating WS connects)
(defparameter *ws-token-ttl-secs* 3600)
(defvar *ws-token-table* (lh:make-castable :test #'equal)) ; token string → (user-id . expiry-unix)

(defun %make-ws-token ()
  (let ((bytes (make-array 16 :element-type '(unsigned-byte 8))))
    (dotimes (i 16) (setf (aref bytes i) (random 256)))
    (let ((s (with-output-to-string (out)
               (dotimes (i 16)
                 (format out "~2,'0x" (aref bytes i))))))
      s)))

(defun issue-ws-token (user)
  "Create a short-lived token for WS authentication and return it."
  (let* ((tok (%make-ws-token))
         (exp (+ (get-universal-time) *ws-token-ttl-secs*)))
    (setf (lh:gethash tok *ws-token-table*) (cons (user-id user) exp))
    tok))

(defun validate-ws-token (token)
  "Return user-id when TOKEN is valid and not expired; otherwise NIL."
  (multiple-value-bind (val present) (lh:gethash token *ws-token-table*)
    (when present
      (destructuring-bind (uid . exp) val
        (if (> (get-universal-time) exp)
            (progn (lh:remhash token *ws-token-table*) nil)
            uid)))))

(defun append-query-param (url key val)
  (let* ((sep (if (search "?" url) "&" "?")))
    (format nil "~a~a~a=~a" url sep key (hunchentoot:url-encode val))))

(defun start-ws-keepalive ()
  "Start a background thread that periodically sends tiny keepalive messages
   to all connected WebSocket clients so that intermediary load balancers and
   routes (e.g., OpenShift, AWS) keep the connections alive."
  (when (and *ws-keepalive-thread* (bt2:thread-alive-p *ws-keepalive-thread*))
    (return-from start-ws-keepalive *ws-keepalive-thread*))
  (when (and (numberp *ws-keepalive-interval*) (> *ws-keepalive-interval* 0))
    (setf *ws-keepalive-running* t)
    (setf *ws-keepalive-thread*
          (bt2:make-thread
           (lambda ()
             (handler-bind ((error (lambda (c)
                                     (format *error-output* "Error in thread ~A: ~A~%"
                                             (bt2:current-thread) c)
                                     (sb-debug:print-backtrace :count 50 :stream *error-output*)
                                     (finish-output *error-output*))))
               (let ((ping-fn (when (fboundp 'ws:write-to-client-ping)
                                (symbol-function 'ws:write-to-client-ping))))
                 (loop while *ws-keepalive-running*
                       do (sleep *ws-keepalive-interval*)
                          (when (null ping-fn)
                            (log:warn "CLWS ping not available; disabling server keepalive pings")
                            (return))
                          ;; Send PING to all clients (store last-ping-ms and include ms payload)
                          (ignore-errors
                            (dolist (c (get-client-list))
                              (handler-case
                                  (with-write-lock-held ((client-lock c))
                                    (let* ((tsms (floor (now-micros) 1000))
                                           (payload (format nil "~D" tsms)))
                                      (setf (client-last-ping-ms c) tsms)
                                      (funcall ping-fn (client-socket c) payload)))
                                (error (e)
                                  (log:warn "Keepalive ping failed; removing client: ~A" e)
                                  (ignore-errors (remove-client (client-socket c)))))))
                          ;; Reap clients that haven't PONGed in time
                          (let ((now (get-universal-time)))
                            (dolist (c (get-client-list))
                              (let ((last (client-last-pong-ts c)))
                                (when (or (null last)
                                          (> (- now last) *ws-ping-timeout*))
                                  (handler-case
                                      (progn
                                        (with-write-lock-held ((client-lock c))
                                          (ignore-errors
                                            (ws:write-to-client-close (client-socket c))))
                                        (remove-client (client-socket c))
                                        (log:info "Dropped client due to missed PONG (~As)" (or (and last (- now last)) :unknown)))
                                    (error (e)
                                      (log:warn "Error dropping stale client: ~A" e))))))))))
           :name "ws keepalive")))
    (log:info "WebSocket keepalive enabled (~As)" *ws-keepalive-interval*)
    *ws-keepalive-thread*))


(defun stop-ws-keepalive ()
  "Signal the keepalive thread to stop."
  (setf *ws-keepalive-running* nil))

(defvar *solves-table* (lh:make-castable))

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
              (return t)))))))


;; ----------------------------------------------------------------------------
;; Machinery for managing the execution of the server.
(defvar *shutdown-cv* (bt2:make-condition-variable))
(defvar *server-lock* (bt2:make-lock))
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

;; Static file cache for improved performance
(defvar *static-file-cache* (make-hash-table :test 'equal)
  "Hash table cache for static files. Key: file path, Value: (content . content-type)")

(defvar *cache-lock* (bt2:make-lock :name "static-cache-lock")
  "Lock for thread-safe access to the static file cache")

(defun get-cached-file (file-path content-type)
  "Get file from cache or load and cache it. Returns (values content cached-p)"
  (if *developer-mode*
      ;; In developer mode, don't cache - always read from disk
      (values (alexandria:read-file-into-byte-vector file-path) nil)
      ;; In production mode, use cache
      (bt2:with-lock-held (*cache-lock*)
        (let ((cached (gethash file-path *static-file-cache*)))
          (if cached
              (values (car cached) t)
              ;; Not cached, load and cache it
              (let ((content (alexandria:read-file-into-byte-vector file-path)))
                (setf (gethash file-path *static-file-cache*)
                      (cons content content-type))
                (values content nil)))))))

(defun clear-static-cache ()
  "Clear the static file cache (useful for testing or memory management)"
  (bt2:with-lock-held (*cache-lock*)
    (clrhash *static-file-cache*)))

(defun get-cache-stats ()
  "Get statistics about the static file cache"
  (bt2:with-lock-held (*cache-lock*)
    (let ((entries (hash-table-count *static-file-cache*))
          (total-size 0))
      (loop for (content . content-type) being the hash-values of *static-file-cache*
            do (incf total-size (length content)))
      (values entries total-size))))

(defun preload-static-files ()
  "Preload common static files into cache"
  (unless *developer-mode*
    (let ((common-files '("css/ctfg.css" "js/app.js" "images/banner.png")))
      (log:info "Preloading static files into cache...")
      (dolist (file-rel-path common-files)
        (let ((file-path (merge-pathnames file-rel-path (app-root))))
          (when (probe-file file-path)
            (handler-case
                (let ((content-type (get-content-type file-path)))
                  (get-cached-file file-path content-type)
                  (log:debug "Preloaded: ~A (~A)" file-rel-path content-type))
              (error (e)
                (log:warn "Failed to preload ~A: ~A" file-rel-path e))))))
      (multiple-value-bind (entries total-size) (get-cache-stats)
        (log:info "Static cache preloaded: ~D files, ~D bytes" entries total-size)))))

(defun get-content-type (file-path)
  "Determine content type based on file extension"
  (let ((extension (pathname-type file-path)))
    (cond
      ((string-equal extension "css") "text/css")
      ((string-equal extension "js") "application/javascript")
      ((string-equal extension "png") "image/png")
      ((string-equal extension "jpg") "image/jpeg")
      ((string-equal extension "jpeg") "image/jpeg")
      ((string-equal extension "gif") "image/gif")
      ((string-equal extension "svg") "image/svg+xml")
      ((string-equal extension "html") "text/html")
      ((string-equal extension "htm") "text/html")
      (t "application/octet-stream"))))

(defun cached-static-dispatcher (request)
  "Custom dispatcher that serves static files safely with caching and traversal protection."
  (let* ((raw (hunchentoot:script-name request))
         ;; Ensure URL-decoding so encoded traversals like %2e%2e are handled
         (script-name (hunchentoot:url-decode raw))
         (prefix-map '(("/css/" . "css/") ("/js/" . "js/") ("/images/" . "images/"))))
    (loop for (prefix . base-rel) in prefix-map
          when (alexandria:starts-with-subseq prefix script-name)
            do (return
                 (lambda ()
                   ;; Compute base directory and relative part strictly under prefix
                   (let* ((base-root (merge-pathnames base-rel (app-root)))
                          (rel (subseq script-name (length prefix)))
                          ;; basic sanitization: reject obvious traversal or backslash separators
                          (bad (or (search ".." rel)
                                   (find #\\ rel)
                                   (and (> (length rel) 0) (char= (char rel 0) #\/))))
                          (candidate (unless bad (merge-pathnames rel base-root))))
                     (cond
                       (bad
                        (setf (hunchentoot:return-code*) hunchentoot:+http-bad-request+)
                        "Bad path")
                       ((and candidate (probe-file candidate)
                             (let ((tru (ignore-errors (truename candidate))))
                               (and tru (uiop:subpathp tru base-root))))
                        (let* ((content-type (get-content-type candidate)))
                          (multiple-value-bind (content cached-p)
                              (get-cached-file candidate content-type)
                            (setf (hunchentoot:content-type*) content-type)
                            (if *developer-mode*
                                (hunchentoot:no-cache)
                                (progn
                                  (setf (hunchentoot:header-out "Cache-Control") "public, max-age=31536000")
                                  (setf (hunchentoot:header-out "Expires")
                                        (hunchentoot:rfc-1123-date (+ (get-universal-time) (* 365 24 60 60))))))
                            (when (and (not *developer-mode*) (log:debug))
                              (log:debug "Static file ~A: ~A" candidate (if cached-p "CACHE HIT" "CACHE MISS")))
                            content)))
                       (t
                        (setf (hunchentoot:return-code*) hunchentoot:+http-not-found+)
                        "File not found"))))))))

(defun cache-callback (file content-type)
  "Legacy callback for compatibility (no longer used with custom dispatcher)"
  (declare (ignore file content-type))
  (if *developer-mode*
      (hunchentoot:no-cache)
      (progn
        (setf (hunchentoot:header-out "Cache-Control") "public, max-age=31536000")
        (setf (hunchentoot:header-out "Expires")
              (hunchentoot:rfc-1123-date (+ (get-universal-time) (* 365 24 60 60)))))))

(defvar *index.html* nil)

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
          (mapcar #'bt2:thread-name
                  (bt2:all-threads))))

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
             (let* ((token (issue-ws-token user))
                    (ws-url (append-query-param *websocket-url* "token" token)))
               (respond-json
                `((:displayname . ,(user-displayname user)) (:needs_name . ,needs-name) (:websocket_url . ,ws-url)))))))
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
        (let* ((token (issue-ws-token user))
               (ws-url (append-query-param *websocket-url* "token" token)))
          (respond-json `((:username . ,(user-username user))
                          (:displayname . ,(user-displayname user))
                          (:needs_name . ,(null (user-displayname user)))
                          (:websocket_url . ,ws-url))))
        (respond-json '((:error . "no session")) :code 401))))

(defun user-solved-p (user-id challenge-id)
  "Checks if a user has already solved a given challenge.
   Returns non-NIL (the tail of the list starting from the found element) if solved, NIL otherwise."
  (let ((solves-list (lh:gethash user-id *solves-table*)))
    (when solves-list
      (ll:member challenge-id solves-list))))

(defun award-points (user challenge reload)
  (log:info "award points")
  (multiple-value-bind (ts event-id)
      (record-flag *db* user challenge)
    (let ((msg (format nil "[{ \"id\": ~A, \"type\": \"score\", \"displayname\": ~S, \"ts\": ~A, \"challenge\": ~S, \"points\": ~A ~A}]"
                       event-id
                       (user-displayname user)
                       (floor ts 1000)
                       (challenge-title challenge)
                       (challenge-points challenge)
                       (if reload
                           ", \"reload\": \"true\""
                           ""))))
      (log:info msg)
      (save-solve (user-id user) (challenge-id challenge))
      (dolist (client (get-client-list))
        (with-write-lock-held ((client-lock client))
          (ws:write-to-client-text (client-socket client) msg)))
      (incf (user-total-points user) (challenge-points challenge)))))

(defun award-points-atomic (user challenge reload)
  "Atomically check and award points for a challenge submission.
   Returns T if points were awarded, NIL if already solved."
  (log:info "award points atomic")
  (multiple-value-bind (success ts event-id)
      (record-flag-if-not-solved *db* user challenge)
    (when success
      (let ((msg (format nil "[{ \"id\": ~A, \"type\": \"score\", \"displayname\": ~S, \"ts\": ~A, \"challenge\": ~S, \"points\": ~A ~A}]"
                         event-id
                         (user-displayname user)
                         (floor ts 1000)
                         (challenge-title challenge)
                         (challenge-points challenge)
                         (if reload
                             ", \"reload\": \"true\""
                             ""))))
        (log:info msg)
        (save-solve (user-id user) (challenge-id challenge))
        (dolist (client (get-client-list))
          (with-write-lock-held ((client-lock client))
            (ws:write-to-client-text (client-socket client) msg))))
      (incf (user-total-points user) (challenge-points challenge)))
    success))

(easy-routes:defroute set-name ("/api/set-name" :method :post) ()
  "Set your display name"
  (with-authenticated-user (user)
    ;; Rate limit check
    (unless (check-rate-limit *api-rate-limiter* user "set-name")
      (log:warn "Rate limit exceeded for ~A on /api/set-name" (user-displayname user))
      (return-from set-name
        (respond-json '((:error . "rate_limit_exceeded")) :code 429)))
    (let* ((body   (json-body))
           (name   (cdr (assoc :name body))))
      ;; Check if the display name is already taken by another user
      (when (displayname-exists-p *db* name (user-id user))
        (log:info "Display name ~A already taken (requested by ~A)" name (user-username user))
        (return-from set-name
          (respond-json '((:error . "name_taken")) :code 409)))
      (log:info "Setting displayname for player ~A to ~A: " (user-username user) name)
      (set-displayname *db* user name)
      (respond-json '((:result . "ok"))))))

(easy-routes:defroute hint ("/api/hint" :method :post) ()
  (with-authenticated-user (user)
    ;; Rate limit check
    (unless (check-rate-limit *hint-rate-limiter* user "hint")
      (log:warn "Rate limit exceeded for ~A on /api/hint" (user-displayname user))
      (return-from hint
        (respond-json '((:error . "rate_limit_exceeded")) :code 429)))
    (let* ((body (json-body))
           (cid  (cdr (assoc :id body)))
           (hid  (cdr (assoc :hint--id body))))
      (unless (and cid hid)
        (log:info "Hint missing parameters for " user)
        (return-from hint
          (respond-json '((:error "missing_parameters")) :code 400)))
      (let ((chal (find cid *all-challenges* :key #'challenge-id)))
        (unless chal
          (log:info "Hint for unknown challenge from " user)
          (return-from hint (respond-json '((:error "unknown_challenge")) :code 400)))
        ;; cost & text from challenge meta
        (let* ((hint (find hid (challenge-hints chal)
                           :key (lambda (h) (cdr (assoc :id h)))))
               (cost (cdr (assoc :cost hint))))
          (unless hint
            (log:info "Hint not found for challenge ~A hint ~A" cid hid)
            (return-from hint (respond-json '((:error "unknown_hint")) :code 400)))
          ;; Use atomic hint purchase
          (multiple-value-bind (status ts eid)
              (record-hint-atomic *db* user chal hid cost (user-total-points user))
            (case status
              (:success
               (decf (user-total-points user) cost)
               ;; broadcast score delta
               (let ((msg (format nil
                           "{ \"id\":~A, \"type\":\"hint\", \"displayname\":~S,\
\"ts\":~A, \"challenge_id\":~A, \"hint_id\":~A, \"points\":-~A }"
                           eid (user-displayname user) (floor ts 1000) cid hid cost)))
                 (dolist (client (get-client-list))
                   (with-write-lock-held ((client-lock client))
                     (ws:write-to-client-text (client-socket client) msg))))
               (respond-json '((:result . "ok"))))
              (:already-purchased
               (log:info "Hint already purchased by ~A" user)
               (respond-json '((:error . "already_purchased")) :code 400))
              (:wrong-sequence
               (log:info "Wrong hint sequence for ~A" user)
               (respond-json '((:error . "locked")) :code 403))
              (:insufficient-points
               (log:info "Insufficient points for ~A" user)
               (respond-json '((:error . "insufficient_points")) :code 402))
              (t
               (log:error "Unknown status from record-hint-atomic: ~A" status)
               (respond-json '((:error . "internal_error")) :code 500)))))))))

(easy-routes:defroute award ("/api/award" :method :post) ()
  "Award points as if submitted by user."
  (let* ((request hunchentoot:*request*)
         (access-token (hunchentoot:header-in :AUTHORIZATION request)))
    ;; Do not log bearer token
    (cond
      ((null *ctfg-api-token*)
       (respond-unauthorized))
      ((not (string= access-token *ctfg-api-token*))
       (respond-unauthorized))
      (t
       ;; Apply a basic API rate limit by token string
       (unless (consume-token-p *api-rate-limiter* (or access-token "api-award"))
         (return-from award (respond-json '((:error . "rate_limit_exceeded")) :code 429)))
       (let* ((body (json-body))
              (username (cdr (assoc :username body)))
              (cid (cdr (assoc :id body)))
              (chal (find cid *all-challenges* :key #'challenge-id))
              (user (lh:gethash username *username-to-user-object*))
              (solved (user-solved-p user cid)))
         (cond
           ((null chal)
            (respond-json '(:error "unknown_id") :code 400))
           (solved
            (respond-json `(:result "already_solved" :total ,(user-total-points user))))
           (t
            (log:info "Awarding points for challenge ~A to ~A" cid user)
            ;; Use atomic award to prevent double-awarding
            (if (award-points-atomic user chal t)
                (respond-json '((:result . "ok")))
                (respond-json '((:result . "already_solved")))))))))))

(easy-routes:defroute submit ("/api/submit" :method :post) ()
  "Submit a flag"
  (with-authenticated-user (user)
    ;; Rate limit check
    (unless (check-rate-limit *submit-rate-limiter* user "submit")
      (log:warn "Rate limit exceeded for ~A on /api/submit" (user-displayname user))
      (return-from submit
        (respond-json '((:error . "rate_limit_exceeded")) :code 429)))
    (let* ((body   (json-body))
           (cid    (cdr (assoc :id body)))
           (guess  (string-trim '(#\Space #\Tab #\Newline) (cdr (assoc :flag body))))
           (chal   (find cid *all-challenges* :key #'challenge-id)))
      (log:info "~A submitting for challenge ~A: ~A"
                (user-displayname user)
                cid
                guess)
      (cond
        ((null chal)
         (respond-json '(:error "unknown_id") :code 400))
        ((eq 1 (ppcre:count-matches (challenge-flag chal) guess))
         (log:info "Correct!")
         ;; Use atomic award-points to prevent double-submit
         (if (award-points-atomic user chal nil)
             (respond-json `((:result . "correct")
                             (:points . ,(challenge-points chal))
                             (:total . ,(user-total-points user))))
             ;; Already solved (race condition handled)
             (respond-json `(:result "already_solved" :total ,(user-total-points user)))))
        (t
         (log:info "Incorrect!")
         (respond-json '((:result . "incorrect"))))))))

(defun %string->utf8 (s)
  (babel:string-to-octets s :encoding :utf-8))

(defun %utf8->string (bytes)
  (babel:octets-to-string bytes :encoding :utf-8))

(defun %xor-bytes (bytes key-bytes)
  (let* ((n (length bytes))
         (k (length key-bytes))
         (out (make-array n :element-type '(unsigned-byte 8))))
    (dotimes (i n out)
      (setf (aref out i)
            (logxor (aref bytes i)
                    (aref key-bytes (mod i k)))))))

(defun %hex-encode (bytes)
  (with-output-to-string (out)
    (loop for b across bytes do
      (format out "~2,'0X" b))))

(defun %hex-decode (hex)
  (let* ((len (length hex))
         (_ (assert (evenp len) () "Hex length must be even."))
         (out (make-array (/ len 2) :element-type '(unsigned-byte 8))))
    (loop for i from 0 below len by 2
          for j from 0
          do (setf (aref out j)
                   (parse-integer hex :start i :end (+ i 2) :radix 16)))
    out))

;; CRC32 implementation
(defun %crc32 (bytes)
  "Calculate CRC32 checksum of a byte array."
  (let ((crc #xFFFFFFFF)
        (polynomial #xEDB88320))
    (loop for byte across bytes do
      (setf crc (logxor crc byte))
      (dotimes (j 8)
        (if (logbitp 0 crc)
            (setf crc (logxor (ash crc -1) polynomial))
            (setf crc (ash crc -1)))))
    (logxor crc #xFFFFFFFF)))

(defun mask-string (plain key)
  "XOR mask PLAIN with KEY (both strings), add CRC32 checksum, return uppercase hex."
  (let* ((p (%string->utf8 plain))
         (k (%string->utf8 key))
         (checksum (%crc32 p))
         ;; Create buffer with data + checksum (4 bytes)
         (with-checksum (make-array (+ (length p) 4) :element-type '(unsigned-byte 8))))
    ;; Copy data
    (replace with-checksum p)
    ;; Append checksum (big-endian, 4 bytes)
    (setf (aref with-checksum (length p)) (ldb (byte 8 24) checksum))
    (setf (aref with-checksum (+ (length p) 1)) (ldb (byte 8 16) checksum))
    (setf (aref with-checksum (+ (length p) 2)) (ldb (byte 8 8) checksum))
    (setf (aref with-checksum (+ (length p) 3)) (ldb (byte 8 0) checksum))
    ;; XOR the entire buffer
    (let ((x (%xor-bytes with-checksum k)))
      (%hex-encode x))))

(defun unmask-string (hex key)
  "Reverse of MASK-STRING with checksum verification. HEX is the masked hex string."
  (handler-case
      (let* ((x (%hex-decode hex))
             (k (%string->utf8 key))
             (decrypted (%xor-bytes x k)))
        ;; Check minimum length for checksum
        (when (< (length decrypted) 4)
          (error "Data too short to contain checksum"))
        (let* ((data-length (- (length decrypted) 4))
               (data (subseq decrypted 0 data-length))
               ;; Extract checksum (last 4 bytes, big-endian)
               (stored-checksum (logior (ash (aref decrypted data-length) 24)
                                        (ash (aref decrypted (+ data-length 1)) 16)
                                        (ash (aref decrypted (+ data-length 2)) 8)
                                        (aref decrypted (+ data-length 3))))
               ;; Calculate checksum of data
               (calculated-checksum (%crc32 data)))
          ;; Verify checksum
          (unless (= stored-checksum calculated-checksum)
            (error "Checksum mismatch! Data may be corrupted. Expected: ~X, Got: ~X"
                   stored-checksum calculated-checksum))
          (%utf8->string data)))
    (error (e)
      (log:error "Error unmasking string: ~A" e)
      (error e))))

(defun process-description (user description)
  (setf description
        (cl-ppcre:regex-replace-all "@CONTROL_CLUSTER@" description *control-cluster*))
  (setf description
        (cl-ppcre:regex-replace-all "@USERNAME@" description (user-username user)))
  (setf description
        (cl-ppcre:regex-replace-all "@USERID@" description (format nil "~A" (user-id user))))
  (setf description
        (cl-ppcre:regex-replace-all "@DISPLAYNAME@" description
                                    (or (user-displayname user) "[unset]")))
  (setf description
        (cl-ppcre:regex-replace-all "@OBFUSCATED_DISPLAYNAME@" description
                                    (if (user-displayname user)
                                        (mask-string (user-displayname user) "some-secret-key")
                                        "[unset]")))
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
          (events (collect-events *db*)))
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
                                           (cons "solved" (user-solved-p (user-id user) (challenge-id challenge)))
                                           (cons "description"
                                                 (process-description
                                                  user
                                                  (challenge-description challenge)))
                                           (cons "hints"
                                                 (mapcar
                                                  (lambda (h)                              ; one hint plist
                                                    (let* ((hid   (cdr (assoc :id   h)))
                                                           (cost  (cdr (assoc :cost h)))
                                                           (owned (hint-purchased-p        ; NEW helper, see below
                                                                   (user-id user)
                                                                   (challenge-id challenge)
                                                                   hid)))
                                                      ;; Build a *new* plist to send to the browser
                                                      `(("id"   . ,hid)
                                                        ("cost" . ,cost)
                                                        ,@(when owned                     ; include text **only if** bought
                                                            `(("text" . ,(cdr (assoc :text h))))))))
                                                  (challenge-hints challenge)))
                                           (cons "content" (challenge-content challenge))))
                                   challenges)))
            (cl-json:encode-json-to-string json-data)))))))

(easy-routes:defroute index ("/" :method :get) ()
  "Main index page"
  (when *developer-mode* (load-challenges))
  (setf (hunchentoot:content-type*) "text/html")
  (when (or (null *index.html*) *developer-mode*)
    (setf *index.html* (uiop:read-file-string "index.html")))
  *index.html*)

(defclass scorestream-resource (ws:ws-resource)
  ())

(ws:register-global-resource "/scorestream"
                             (make-instance 'scorestream-resource)
                             (lambda (origin)
                               ;; Allow no Origin (non-browser clients) or localhost in dev.
                               (or (null origin)
                                   *developer-mode*
                                   (alexandria:starts-with-subseq "http://localhost:" origin)
                                   (alexandria:starts-with-subseq "http://127.0.0.1:" origin))))

;; Enforce token on WS handshake
(defmethod ws:resource-accept-connection ((res scorestream-resource) resource-name headers client)
  (declare (ignore res resource-name))
  (let* ((origin (gethash :origin headers))
         (qs (or (ws:client-query-string client) ""))
         (pairs (mapcar (lambda (kv)
                          (let ((p (split-sequence:split-sequence #\= kv)))
                            (cons (string-downcase (first p)) (second p))))
                        (split-sequence:split-sequence #\& qs)))
         (token (cdr (assoc "token" pairs :test #'string=)))
         (uid (and token (validate-ws-token token))))
    (log:info "WebSocket connection attempt: origin=~S qs=~S token=~S uid=~S" origin qs token uid)
    (let ((accepted (and (or (null origin)
                             *developer-mode*
                             (alexandria:starts-with-subseq "http://localhost:" origin)
                             (alexandria:starts-with-subseq "http://127.0.0.1:" origin))
                         uid)))
      (log:info "WebSocket connection ~A" (if accepted "ACCEPTED" "REJECTED"))
      (values accepted nil origin nil nil))))

(defun send-events (client)
  (log:info "send-events called in background thread")
  (let ((events (collect-events *db*)))
    (log:info "Collected ~D events from database" (length events))
    (let ((json-events
              (loop for event in events
                    collect (handler-case
                                (format nil "{ \"id\": ~A, \"type\": \"score\", \"displayname\": ~S, \"ts\": ~A, \"challenge_id\": ~A, \"challenge\": ~S, \"points\": ~A }"
                                       (event-id event)
                                       (get-displayname (event-user-id event))
                                       (floor (event-ts event) 1000)
                                       (event-challenge-id event)
                                       (let ((challenge (find (event-challenge-id event) *all-challenges* :key #'challenge-id)))
                                         (if challenge
                                             (challenge-title challenge)
                                             "Unknown Challenge"))
                                       (event-points event))
                              (error (e)
                                (log:warn "Error formatting event ~A: ~A" (event-id event) e)
                                nil)))))
      ;; Remove any nil entries from failed formatting
      (setf json-events (remove nil json-events))
      (log:info "Sending hydration to WS client (~D events)" (length json-events))
      (handler-case
          (progn
            (log:info "About to send WebSocket message...")
            (with-write-lock-held ((client-lock client))
              (ws:write-to-client-text
               (client-socket client)
               (format nil "[~{~a~^,~}]" json-events))) ; Single array message
            (log:info "Hydration sent successfully"))
        (error (e)
          (log:error "Error sending events to WebSocket: ~A" e)
          (capture-exception e))))))

(defmethod ws:resource-client-connected ((res scorestream-resource) client)
  (log:info "Client connected to scorestream server from ~s : ~s" (ws:client-host client) (ws:client-port client))
  (log:info "Creating client wrapper...")
  (handler-case
      (let ((client-wrapper (make-client :socket client :lock (make-rwlock) :last-pong-ts (get-universal-time))))
        (log:info "Adding client to list...")
        (add-client client-wrapper)
        (log:info "Scheduling async send-events...")
        ;; Schedule async send-events but don't wait for it
        (bt2:make-thread
         (lambda ()
           (handler-bind ((error (lambda (c)
                                   (log:error "Error in send-events thread: ~A" c)
                                   (capture-exception c))))
             (send-events client-wrapper)))
         :name "send-events")
        (log:info "Client setup completed successfully"))
    (error (e)
      (log:error "Error on websocket client connect: ~A" e)
      (capture-exception e))))

(defmethod ws:resource-received-text ((res scorestream-resource) client message)
  ;; The only messages we receive from the client are pings designed
  ;; to keep network devices between the client and server from timing
  ;; out the connection.  We can ignore these.
  (when *developer-mode*
    (log:debug "Received ~S from ~S." message client)))

(defmethod ws:resource-received-pong ((res scorestream-resource) client message)
  ;; Update last-pong timestamp and log latency. MESSAGE is the pong payload octets.
  (declare (ignore res))
  (handler-case
      (let* ((wrapper (find client (get-client-list)
                            :key #'client-socket :test #'eq))
             (now-secs (get-universal-time))
             (payload-str (ignore-errors (%utf8->string message)))
             (now-ms (floor (now-micros) 1000))
             (sent-ms (or (ignore-errors (parse-integer (or payload-str "") :junk-allowed t))
                          (and wrapper (client-last-ping-ms wrapper))))
             (latency (and sent-ms (- now-ms sent-ms))))
        (when wrapper
          (with-write-lock-held ((client-lock wrapper))
            (setf (client-last-pong-ts wrapper) now-secs)))
        (when latency
          (cond
            ((> latency *ws-ping-latency-warn-ms*)
             (log:warn "WS Pong latency ~A ms from ~A:~A" latency (ws:client-host client) (ws:client-port client)))
            (*ws-log-ping-latency*
             (log:info "WS Pong latency ~A ms from ~A:~A" latency (ws:client-host client) (ws:client-port client))))))
    (error (e)
      (log:warn "Error handling PONG: ~A" e))))

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
  ;; Show detailed errors only in developer mode
  (setf hunchentoot:*show-lisp-errors-p* *developer-mode*)
  (setf hunchentoot:*show-lisp-backtraces-p* *developer-mode*)
  (setf hunchentoot:*session-max-time* most-positive-fixnum)

  ;; Start rate limit cleanup thread
  (start-rate-limit-cleanup)

  (setf *sentry-dsn* (uiop:getenv "SENTRY_DSN"))
  (when *sentry-dsn*
    (log:info "Initializing sentry client.")
    (sentry-client:initialize-sentry-client *sentry-dsn*))

  (load-challenges)

  (setf *db* (make-instance 'db/sqlite :filename (merge-pathnames "events.db" *dbdir*)))

  (log:info "Static content directory: ~A" (uiop:getcwd))
  (log:info "Starting server version ~A on port ~A" +version+ port)

  ;; Preload static files into cache for better performance
  (preload-static-files)

  ;; Add our custom static file dispatcher to the front of the dispatch table
  ;; Easy-routes automatically adds its dispatcher when routes are defined
  (push 'cached-static-dispatcher hunchentoot:*dispatch-table*)

  (read-credentials)

  ;; Pre-load users from credentials to avoid INSERT race conditions
  (preload-users *db*)

  (let ((events (collect-events *db*)))
    (dolist (event events)
      (save-solve (event-user-id event) (event-challenge-id event))))

  (setf ws:*log-level* nil)
  (setf ws:*debug-on-server-errors* nil)
  (setf ws:*debug-on-resource-errors* nil)
  (setf ws::*max-write-backlog* (* 500 30)) ; 500 players, 30 challenges

  ;; Increase session store capacity for high load
  (setf hunchentoot:*session-gc-frequency* 20)  ; More frequent cleanup
  (setf hunchentoot:*rewrite-for-session-urls* nil) ; Disable URL rewriting for performance

  (bt2:make-thread
   (lambda ()
     (handler-bind ((error (lambda (c)
                             (format *error-output* "Error in thread ~A: ~A~%"
                                     (bt2:current-thread) c)
                             (sb-debug:print-backtrace :count 50 :stream *error-output*)
                             (finish-output *error-output*))))
       (handler-case
           (handler-case
               ;; Prefer CLWS backlog if available; fall back if keyword not accepted
               (apply #'ws:run-server (append (list 12345)
                                              (when *ws-backlog* (list :backlog *ws-backlog*))))
             (error (e)
               (log:warn "CLWS run-server does not accept :backlog (~A); using default" e)
               (ws:run-server 12345)))
         (error (e)
           (log:error "WebSocket server crashed: ~A" e)
           (capture-exception e)))))
   :name "websockets server")

  (bt2:make-thread
   (lambda ()
     (handler-bind ((error (lambda (c)
                             (format *error-output* "Error in thread ~A: ~A~%"
                                     (bt2:current-thread) c)
                             (sb-debug:print-backtrace :count 50 :stream *error-output*)
                             (finish-output *error-output*))))
       (handler-case
           (ws:run-resource-listener
           (ws:find-global-resource "/scorestream"))
         (error (e)
           (log:error "Resource listener crashed: ~A" e)
           (capture-exception e)))))
   :name "resource listener for /scorestream")

  ;; Start WebSocket keepalive pings if enabled
  (start-ws-keepalive)

  ;; Create and start the easy-routes acceptor with high-capacity taskmaster
  (setf *acceptor* (make-instance 'my-acceptor
                     :port port
                     :taskmaster (make-instance 'hunchentoot:one-thread-per-connection-taskmaster
                                   :max-thread-count 500      ; 5x default for high load
                                   :max-accept-count 600)))   ; 500 + 100 buffer
  (hunchentoot:start *acceptor*)
  (log:info "Server started successfully on port ~A" port)
  *acceptor*)

(defun shutdown-server ()
  "Gracefully shutdown the server and cleanup resources"
  (log:info "Shutting down server...")
  (when *acceptor*
    (hunchentoot:stop *acceptor*)
    (setf *acceptor* nil))
  (stop-ws-keepalive)
  (stop-rate-limit-cleanup)
  (log:info "Server shutdown complete"))

(defmethod hunchentoot:create-request-handler-thread :around ((taskmaster hunchentoot:one-thread-per-connection-taskmaster) socket)
  (handler-bind ((error (lambda (c)
                          (format *error-output* "Error in thread ~A: ~A~%"
                                  (bt2:current-thread) c)
                          (sb-debug:print-backtrace :count 50 :stream *error-output*)
                          (finish-output *error-output*))))
    (call-next-method)))
