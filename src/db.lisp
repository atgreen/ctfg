;;; db.lisp  –  cl-sqlite edition
;;;
;;; SPDX-License-Identifier: MIT
;;; Copyright (C) 2025 Anthony Green

(in-package :ctfg)

;;;; --------------------------------------------------------------------------
;;;;  Thread-safe connection pool
;;;; --------------------------------------------------------------------------

;; Use thread ID as key for connection mapping
(defvar *thread-connections*
  (make-hash-table :test #'equal :synchronized t)
  "Thread-safe hash table mapping thread IDs to SQLite connections.")

(defvar *connection-lock* (bt:make-lock "sqlite-connection-lock")
  "Lock to protect connection creation/cleanup.")

(defun get-thread-id ()
  "Get a unique identifier for the current thread."
  #+sbcl (sb-thread:thread-name sb-thread:*current-thread*)
  #+ccl (ccl:process-name ccl:*current-process*)
  #-(or sbcl ccl) (bt:thread-name (bt:current-thread)))

(defun %connect-sqlite (path &key (busy-timeout 30000))
  "Get or create a SQLite connection for the current thread.
Each thread gets its own connection to avoid corruption under concurrent access.
Busy-timeout is expressed in milliseconds."
  (let ((thread-id (get-thread-id))
        (path-string (namestring path)))
    (bt:with-lock-held (*connection-lock*)
      (let* ((key (format nil "~A:~A" thread-id path-string))
             (existing-conn (gethash key *thread-connections*)))
        (if existing-conn
            existing-conn
            (let ((new-conn (sqlite:connect path :busy-timeout busy-timeout)))
              (setf (gethash key *thread-connections*) new-conn)
              new-conn))))))

(defun close-thread-db-connection (&optional thread-id path)
  "Close SQLite connections for a specific thread, or current thread if not specified."
  (let ((tid (or thread-id (get-thread-id))))
    (bt:with-lock-held (*connection-lock*)
      (if path
          ;; Close specific connection
          (let* ((path-string (namestring path))
                 (key (format nil "~A:~A" tid path-string))
                 (conn (gethash key *thread-connections*)))
            (when conn
              (ignore-errors (sqlite:disconnect conn))
              (remhash key *thread-connections*)))
          ;; Close all connections for this thread
          (let ((keys-to-remove '()))
            (maphash (lambda (key conn)
                       (when (string-prefix-p (format nil "~A:" tid) key)
                         (ignore-errors (sqlite:disconnect conn))
                         (push key keys-to-remove)))
                     *thread-connections*)
            (dolist (key keys-to-remove)
              (remhash key *thread-connections*)))))))

(defun string-prefix-p (prefix string)
  "Check if STRING starts with PREFIX."
  (and (<= (length prefix) (length string))
       (string= prefix string :end2 (length prefix))))

(defclass db/sqlite (db-backend)
  ((sqlite-db-filename :initarg :filename :reader filename))
  (:default-initargs
   :sql-create-user-table-statement
   "CREATE TABLE IF NOT EXISTS users (id INTEGER PRIMARY KEY AUTOINCREMENT,
                                      username TEXT NOT NULL UNIQUE,
                                      displayname TEXT);"
   :filename (error "Must supply a filename.")))

(defmethod connect-cached ((db db/sqlite))
  (%connect-sqlite (filename db)))

;;;; --------------------------------------------------------------------------
;;;;  µs-precision timestamp
;;;; --------------------------------------------------------------------------

(defun now-micros ()
  ;; convert internal-time (1/1 sec on most Lisps) to µs since UNIX epoch
  (+ (* (- (get-universal-time) 2208988800) 1000000)     ; 1900→1970 offset
     (truncate (/ (get-internal-real-time)
                  (/ internal-time-units-per-second 1000000)))))

;;;; --------------------------------------------------------------------------
;;;;  Convenience macro (kept the same surface API)
;;;; --------------------------------------------------------------------------

(defmacro with-open-connection ((var db) &body body)
  "Bind VAR to a live cl-sqlite handle, then execute BODY.
The handle is *not* closed afterwards – it is kept in the process-wide cache,
mirroring the behaviour of dbi:connect-cached."
  (let ((db-sym (gensym "DB")))
    `(let* ((,db-sym ,db)
            (,var   (connect-cached ,db-sym)))
       ,@body)))

(defmacro with-fresh-connection ((var db) &body body)
  "Bind VAR to a fresh cl-sqlite handle (not cached), then execute BODY.
The handle is closed after BODY completes. Use for atomic transactions."
  (let ((db-sym (gensym "DB"))
        (conn-sym (gensym "CONN")))
    `(let* ((,db-sym ,db)
            (,conn-sym (sqlite:connect (filename ,db-sym) :busy-timeout 30000)))
       (unwind-protect
           (let ((,var ,conn-sym))
             ,@body)
         (sqlite:disconnect ,conn-sym)))))

;;;; --------------------------------------------------------------------------
;;;;  Backend definitions
;;;; --------------------------------------------------------------------------

(defclass db-backend ()
  ((key               :initarg :key               :reader key)
   (make-user-fn      :initarg :make-user-fn      :reader make-user-fn)
   (make-api-key-fn   :initarg :make-api-key-fn   :reader make-api-key-fn)
   (sql-create-user-table-statement
                      :initarg :sql-create-user-table-statement
                      :reader  sql-create-user-table-statement)))

(defclass db/pgsql   (db-backend) ())
(defclass db/mysql   (db-backend) ())

;;;; --------------------------------------------------------------------------
;;;;  Database initialisation
;;;; --------------------------------------------------------------------------

(defmethod initialize-instance :after ((db db/sqlite) &key &allow-other-keys)
  (log:info "INITIALIZING DB/SQLITE")
  (with-open-connection (dbc db)
    ;; Pragmas for high-concurrency performance
    (dolist (sql '("PRAGMA busy_timeout = 30000;"
                   "PRAGMA journal_mode = WAL;"
                   "PRAGMA synchronous = NORMAL;"     ; Faster writes
                   "PRAGMA cache_size = -64000;"      ; 64MB cache
                   "PRAGMA temp_store = MEMORY;"      ; Use RAM for temp
                   "PRAGMA mmap_size = 268435456;"))  ; 256MB memory map
      (sqlite:execute-non-query dbc sql))))

(defmethod initialize-instance :after ((db db-backend) &key)
  (log:info "INITIALIZING DB")
  (with-open-connection (dbc db)
    ;; events table
    (sqlite:execute-non-query
     dbc
     "CREATE TABLE IF NOT EXISTS events (
        id           INTEGER PRIMARY KEY AUTOINCREMENT,
        ts           BIGINT  NOT NULL,
        user_id      BIGINT  NOT NULL,
        challenge_id BIGINT  NOT NULL,
        event_type   SMALLINT NOT NULL,   -- 1 = SUBMIT, 2 = HINT
        points       INTEGER NOT NULL,
        hint_number  INTEGER,
        CHECK (event_type IN (1, 2)),
        CHECK ((event_type = 2 AND hint_number IS NOT NULL)
            OR (event_type = 1 AND hint_number IS NULL)));")
    ;; users table (backend-specific SQL)
    (sqlite:execute-non-query dbc (slot-value db 'sql-create-user-table-statement))))

;;;; --------------------------------------------------------------------------
;;;;  Write helpers
;;;; --------------------------------------------------------------------------

(defun record-flag (db user challenge)
  "Insert a SUBMIT event and return two values:
   • the event timestamp in microseconds
   • the autoincremented ID of the new row"
  (let ((ts (now-micros)))
    (with-open-connection (conn db)
      ;; 1 ─ write the row
      (sqlite:execute-non-query
       conn
       "INSERT INTO events
          (ts, user_id, challenge_id, event_type, points)
        VALUES (?, ?, ?, 1, ?)"              ; 1 = SUBMIT
       ts
       (user-id user)
       (challenge-id challenge)
       (challenge-points challenge))

      ;; 2 ─ same connection → fetch its last row-id
      (let ((row-id (sqlite:last-insert-rowid conn)))
        (values ts row-id)))))

(defun record-flag-if-not-solved (db user challenge)
  "Atomically check if the user has already solved this challenge and record the flag if not.
   Returns three values:
   • T if the flag was recorded, NIL if already solved
   • the event timestamp in microseconds (or NIL)
   • the event ID (or NIL)"
  (let ((ts (now-micros)))
    (with-fresh-connection (conn db)
      ;; Start a transaction for atomicity
      (sqlite:execute-non-query conn "BEGIN IMMEDIATE")
      (handler-case
          (let* ((already-solved
                   (sqlite:execute-single
                    conn
                    "SELECT 1 FROM events
                     WHERE user_id = ? AND challenge_id = ? AND event_type = 1
                     LIMIT 1"
                    (user-id user)
                    (challenge-id challenge))))
            (if already-solved
                (progn
                  (sqlite:execute-non-query conn "ROLLBACK")
                  (values nil nil nil))
                (progn
                  ;; Not solved yet, record it
                  (sqlite:execute-non-query
                   conn
                   "INSERT INTO events
                      (ts, user_id, challenge_id, event_type, points)
                    VALUES (?, ?, ?, 1, ?)"
                   ts
                   (user-id user)
                   (challenge-id challenge)
                   (challenge-points challenge))
                  (let ((row-id (sqlite:last-insert-rowid conn)))
                    (sqlite:execute-non-query conn "COMMIT")
                    (values t ts row-id)))))
        (error (e)
          (sqlite:execute-non-query conn "ROLLBACK")
          (error e))))))

(defun record-hint (db user challenge hint-number cost)
  "Store a HINT purchase (negative points).
Returns two values: timestamp µs and new event-id."
  (log:info "Recording hint: user: ~A, challenge: ~A, hint-number: ~A, row-id: ~A"
            user challenge hint-number cost)
  (let ((ts (now-micros)))
    (with-open-connection (conn db)
      (sqlite:execute-non-query
       conn
       "INSERT INTO events
          (ts, user_id, challenge_id, event_type, hint_number, points)
        VALUES (?, ?, ?, 2, ?, ?)"            ; 2 = HINT
       ts
       (user-id user)
       (challenge-id challenge)
       hint-number
       (- cost))                              ; ⇐ negative delta
      (values ts (sqlite:last-insert-rowid conn)))))

(defun record-hint-atomic (db user challenge hint-number cost current-points)
  "Atomically check if hint can be purchased and record it if possible.
   Checks:
   1. The hint hasn't been purchased yet
   2. The hint-number is the next sequential hint
   3. User has enough points
   Returns three values:
   • :success, :already-purchased, :wrong-sequence, or :insufficient-points
   • the event timestamp in microseconds (or NIL)
   • the event ID (or NIL)"
  (let ((ts (now-micros)))
    (with-fresh-connection (conn db)
      ;; Start a transaction for atomicity
      (sqlite:execute-non-query conn "BEGIN IMMEDIATE")
      (handler-case
          (let* ((already-purchased
                   (sqlite:execute-single
                    conn
                    "SELECT 1 FROM events
                     WHERE user_id = ? AND challenge_id = ?
                     AND hint_number = ? AND event_type = 2
                     LIMIT 1"
                    (user-id user)
                    (challenge-id challenge)
                    hint-number))
                 (max-hint-row
                   (%fetch-one conn
                    "SELECT MAX(hint_number) FROM events
                     WHERE user_id = ? AND challenge_id = ?
                     AND event_type = 2"
                    (user-id user)
                    (challenge-id challenge)))
                 (max-hint (or (car max-hint-row) 0))
                 (expected-hint (1+ max-hint)))
            (cond
              (already-purchased
               (sqlite:execute-non-query conn "ROLLBACK")
               (values :already-purchased nil nil))
              ((/= hint-number expected-hint)
               (sqlite:execute-non-query conn "ROLLBACK")
               (values :wrong-sequence nil nil))
              ((< current-points cost)
               (sqlite:execute-non-query conn "ROLLBACK")
               (values :insufficient-points nil nil))
              (t
               ;; All checks passed, record the hint
               (sqlite:execute-non-query
                conn
                "INSERT INTO events
                   (ts, user_id, challenge_id, event_type, hint_number, points)
                 VALUES (?, ?, ?, 2, ?, ?)"
                ts
                (user-id user)
                (challenge-id challenge)
                hint-number
                (- cost))
               (let ((row-id (sqlite:last-insert-rowid conn)))
                 (sqlite:execute-non-query conn "COMMIT")
                 (values :success ts row-id)))))
        (error (e)
          (sqlite:execute-non-query conn "ROLLBACK")
          (error e))))))

;;;; ------------------------------------------------------------------
;;;;  Hint helpers – SQLite version using %FETCH-ONE
;;;; ------------------------------------------------------------------

(defun hint-purchased-p (uid cid hid)
  "Return T if the player already bought this hint."
  (with-open-connection (c *db*)
    (let ((row (%fetch-one c
               "SELECT 1
                  FROM events
                 WHERE user_id      = ?
                   AND challenge_id = ?
                   AND hint_number  = ?
                   AND event_type   = 2   -- 2 = HINT
                 LIMIT 1"
               uid cid hid)))
      (and row t)))                         ; NIL → not purchased, list → T
  )

(defun next-hint-id (uid cid)
  "Return the *id* of the next hint the player may buy."
  (with-open-connection (c *db*)
    (let* ((row (%fetch-one c
                 "SELECT MAX(hint_number)
                    FROM events
                   WHERE user_id      = ?
                     AND challenge_id = ?
                     AND event_type   = 2"
                 uid cid))
           (max-id (or (car row) 0)))
      (1+ max-id))))

;;;; --------------------------------------------------------------------------
;;;;  Read helpers
;;;; --------------------------------------------------------------------------

(defun collect-events (db)
  "Return a list of EVENT objects."
  (with-open-connection (conn db)
    (loop
      for (id ts uid cid etype hint pts)
        in (sqlite:execute-to-list
            conn
            "SELECT id, ts, user_id, challenge_id,
                    event_type, hint_number, points
               FROM events
              ORDER BY ts")
      collect (make-event
               :id           id
               :ts           ts
               :user-id      uid
               :challenge-id cid
               :event-type   etype
               :hint-number  hint
               :points       pts))))
