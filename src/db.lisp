;;; db.lisp  –  cl-sqlite edition
;;;
;;; SPDX-License-Identifier: MIT
;;; Copyright (C) 2025 Anthony Green

(in-package :ctfg)

(defclass db/sqlite (db-backend)
  ((sqlite-db-filename :initarg :filename :reader filename))
  (:default-initargs
   :sql-create-user-table-statement
   "CREATE TABLE IF NOT EXISTS users (id INTEGER PRIMARY KEY AUTOINCREMENT,
                                      username TEXT NOT NULL UNIQUE,
                                      displayname TEXT);"
   :filename (error "Must supply a filename.")))

;;;; --------------------------------------------------------------------------
;;;;  µs-precision timestamp
;;;; --------------------------------------------------------------------------

(defun now-micros ()
  ;; convert internal-time (1/1 sec on most Lisps) to µs since UNIX epoch
  (+ (* (- (get-universal-time) 2208988800) 1000000)     ; 1900→1970 offset
     (truncate (/ (get-internal-real-time)
                  (/ internal-time-units-per-second 1000000)))))

;;;; --------------------------------------------------------------------------
;;;;  Connection macros (fresh per operation)
;;;; --------------------------------------------------------------------------

;; Limit concurrent SQLite connections to avoid file descriptor exhaustion
;; and transient CANTOPEN errors when many threads try to open at once.
(defparameter *sqlite-max-connections* 128)

;; Simple counting gate implemented with a lock + condition variable
(defstruct (conn-gate (:constructor %make-conn-gate))
  (count 0 :type fixnum)
  (lock (bt:make-lock "conn-gate") :read-only t)
  (cv   (bt:make-condition-variable) :read-only t))

(defun make-conn-gate (count &key (name "conn-gate"))
  (declare (ignore name))
  (%make-conn-gate :count (or count 0)))

(defun conn-gate-acquire (g)
  (when g
    (bt:with-lock-held ((conn-gate-lock g))
      (loop while (<= (conn-gate-count g) 0) do
           (bt:condition-wait (conn-gate-cv g) (conn-gate-lock g)))
      (decf (conn-gate-count g))
      g)))

(defun conn-gate-release (g)
  (when g
    (bt:with-lock-held ((conn-gate-lock g))
      (incf (conn-gate-count g))
      (bt:condition-notify (conn-gate-cv g)))
    t))

(defvar *sqlite-connection-gate*
  (and *sqlite-max-connections* (make-conn-gate *sqlite-max-connections*)))

(defmacro with-fresh-connection ((var db) &body body)
  "Bind VAR to a fresh cl-sqlite handle, execute BODY, then disconnect.
Use this for any DB operation; opening/closing is cheap and avoids FD leaks."
  (let ((db-sym (gensym "DB"))
        (conn-sym (gensym "CONN")))
    `(let* ((,db-sym ,db)
            (,conn-sym nil))
       (conn-gate-acquire *sqlite-connection-gate*)
       (unwind-protect
            (progn
              (setf ,conn-sym (connect-sqlite-with-retry (filename ,db-sym) :busy-timeout 30000))
              (let ((,var ,conn-sym))
                ,@body))
         (ignore-errors (when ,conn-sym (sqlite:disconnect ,conn-sym)))
         (conn-gate-release *sqlite-connection-gate*)))))

(defmacro with-open-connection ((var db) &body body)
  "Compatibility macro: open a fresh connection for BODY and close it.
  All existing call sites that used a cached connection now get a fresh one."
  `(with-fresh-connection (,var ,db)
     ,@body))

;;;; --------------------------------------------------------------------------
;;;;  Busy handling helpers
;;;; --------------------------------------------------------------------------

(defun %busy-error-p (e)
  "Return T if condition E represents an SQLITE BUSY lock error."
  (when e
    (or
     (and (typep e 'sqlite:sqlite-error)
          (let ((s (princ-to-string e)))
            (or (search "SQLITE_BUSY" s)
           (search ":BUSY" s)
           (search "database is locked" s))))
     (let ((s (princ-to-string e)))
       (or (search "SQLITE_BUSY" s)
           (search ":BUSY" s)
           (search "database is locked" s))))))

(defun %cantopen-error-p (e)
  "Return T if condition E represents an SQLITE CANTOPEN error."
  (when e
    (let ((s (princ-to-string e)))
      (or (search "SQLITE_CANTOPEN" s)
          (search ":CANTOPEN" s)
          (search "Could not open sqlite3 database" s)))))

(defun connect-sqlite-with-retry (path &key (busy-timeout 30000) (max-wait-ms 2000))
  "Open a SQLite connection with short retries on CANTOPEN (e.g., transient FD exhaustion)."
  (let* ((deadline (+ (now-micros) (* 1000 max-wait-ms)))
         (sleep-ms 5))
    (loop
      (handler-case
          (return (sqlite:connect path :busy-timeout busy-timeout))
        (error (e)
          (if (%cantopen-error-p e)
              (if (< (now-micros) deadline)
                  (progn (sleep (/ sleep-ms 1000.0))
                         (setf sleep-ms (min (* sleep-ms 2) 100)))
                  (error e))
              (error e)))))))

(defun begin-immediate-with-retry (conn &key (max-wait-ms 15000))
  "Execute BEGIN IMMEDIATE with retries on SQLITE_BUSY up to MAX-WAIT-MS."
  (let* ((deadline (+ (now-micros) (* 1000 max-wait-ms)))
         (sleep-ms 5))
    (loop
      (handler-case
          (progn
            (sqlite:execute-non-query conn "BEGIN IMMEDIATE")
            (return-from begin-immediate-with-retry t))
        (error (e)
          (if (%busy-error-p e)
              (if (< (now-micros) deadline)
                  (progn
                    (sleep (/ sleep-ms 1000.0))
                    (setf sleep-ms (min (* sleep-ms 2) 250)))
                  (error e))
              (error e)))))))

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
    (sqlite:execute-non-query dbc (slot-value db 'sql-create-user-table-statement))

    ;; Create indexes for performance optimization as recommended in agent-review
    (sqlite:execute-non-query
     dbc
     "CREATE INDEX IF NOT EXISTS events_user_chal_submit
      ON events(user_id, challenge_id, event_type);")

    (sqlite:execute-non-query
     dbc
     "CREATE INDEX IF NOT EXISTS events_user_chal_hint
      ON events(user_id, challenge_id, event_type, hint_number);")

    (sqlite:execute-non-query
     dbc
     "CREATE INDEX IF NOT EXISTS events_ts
      ON events(ts);"))

  ;; Concurrency-safety constraints
  (with-open-connection (dbc db)
    ;; Deduplicate any legacy rows that would violate new unique indexes
    (sqlite:execute-non-query
     dbc
     "DELETE FROM events
        WHERE event_type = 1
          AND rowid NOT IN (
            SELECT MIN(rowid)
              FROM events
             WHERE event_type = 1
             GROUP BY user_id, challenge_id);")

    (sqlite:execute-non-query
     dbc
     "DELETE FROM events
        WHERE event_type = 2
          AND rowid NOT IN (
            SELECT MIN(rowid)
              FROM events
             WHERE event_type = 2
             GROUP BY user_id, challenge_id, hint_number);")

    ;; Ensure a user can only have one SUBMIT per challenge
    (sqlite:execute-non-query
     dbc
     "CREATE UNIQUE INDEX IF NOT EXISTS uniq_submit
        ON events(user_id, challenge_id)
        WHERE event_type = 1;")

    ;; Ensure a user can only buy the same hint once
    (sqlite:execute-non-query
     dbc
     "CREATE UNIQUE INDEX IF NOT EXISTS uniq_hint
        ON events(user_id, challenge_id, hint_number)
        WHERE event_type = 2;")))

(defmethod preload-users ((db db-backend))
  "Pre-load users from credentials.csv into the database to avoid INSERT race conditions during login"
  (log:info "Pre-loading users from credentials.csv")
  (let ((count 0))
    (with-open-connection (conn db)
      (sqlite:execute-non-query conn "BEGIN IMMEDIATE;")
      (unwind-protect
           (progn
             (maphash (lambda (username password)
                        (declare (ignore password))
                        (sqlite:execute-non-query
                         conn
                         "INSERT OR IGNORE INTO users (username) VALUES (?);"
                         username)
                        (incf count))
                      *credentials*)
             (sqlite:execute-non-query conn "COMMIT;"))
        (ignore-errors (sqlite:execute-non-query conn "ROLLBACK;"))))
    (log:info "Pre-loaded ~A users into database" count)))

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
      ;; Single-statement idempotent insert using a UNIQUE partial index
      (sqlite:execute-non-query
       conn
       "INSERT OR IGNORE INTO events
          (ts, user_id, challenge_id, event_type, points)
        VALUES (?, ?, ?, 1, ?)"
       ts
       (user-id user)
       (challenge-id challenge)
       (challenge-points challenge))

      ;; Did we insert a row?
      (let* ((changes-row (sqlite:execute-to-list conn "SELECT changes()"))
             (changes (or (caar changes-row) 0)))
        (if (> changes 0)
            (values t ts (sqlite:last-insert-rowid conn))
            (values nil nil nil))))))

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
      ;; Start a transaction for atomicity (robust to BUSY contention)
      (begin-immediate-with-retry conn :max-wait-ms 20000)
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
