;;; db.lisp  –  cl-sqlite edition
;;;
;;; SPDX-License-Identifier: MIT
;;; Copyright (C) 2025 Anthony Green

(in-package :ctfg)

;;;; --------------------------------------------------------------------------
;;;;  Low-level connection caching
;;;; --------------------------------------------------------------------------

(defvar *sqlite-handles*
  ;; pathname → sqlite:sqlite-handle
  (make-hash-table :test #'equal))

(defun %connect-sqlite (path &key (busy-timeout 10000))
  "Open (or reuse) a cl-sqlite HANDLE for the database file at PATH.
Busy-timeout is expressed in milliseconds."
  (or (gethash path *sqlite-handles*)
      (setf (gethash path *sqlite-handles*)
            (sqlite:connect path :busy-timeout busy-timeout)))
  ;; no “cached-disconnect” – the handles live for the whole process
  )

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
    ;; Pragmas
    (dolist (sql '("PRAGMA busy_timeout = 30000;"
                   "PRAGMA journal_mode = WAL;"))
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
    (with-open-connection (conn db)
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
    (with-open-connection (conn db)
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
