;;; db.lisp
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2025 Anthony Green

(in-package :ctfg)

(defclass db-backend ()
  ((key :initarg :key :reader key)
   (make-user-fn :initarg :make-user-fn :reader make-user-fn)
   (make-api-key-fn :initarg :make-api-key-fn :reader make-api-key-fn)
   (sql-create-user-table-statement
    :initarg :sql-create-user-table-statement
    :reader sql-create-user-table-statement)))

(defclass db/pgsql (db-backend)
  ())

(defclass db/mysql (db-backend)
  ())

(defclass db/sqlite (db-backend)
  ((sqlite-db-filename
    :initarg :filename
    :reader filename))
  (:default-initargs
   :sql-create-user-table-statement "CREATE TABLE IF NOT EXISTS users (id INTEGER PRIMARY KEY AUTOINCREMENT, username TEXT NOT NULL UNIQUE, displayname TEXT);"
   :filename (error "Must supply a filename.")))

(defmethod connect-cached ((db db/sqlite))
  (dbi:connect-cached :sqlite3 :database-name (filename db) :busy-timeout 10000))

(defun now-micros ()
  ;; convert internal-time (1/1 sec on most Lisps) to µs since UNIX epoch
  (+ (* (- (get-universal-time) 2208988800) 1000000)     ; 1900→1970 offset
     (truncate (/ (get-internal-real-time)
                  (/ internal-time-units-per-second 1000000)))))

(defmacro with-open-connection ((var db &key (disconnect nil)) &body body)
  "Bind VAR to a live DB connection (from CONNECT-CACHED DB) for BODY.

   By default the connection is disconnected when BODY exits—successfully
   or via error—using UNWIND-PROTECT.  Pass :DISCONNECT NIL if you want
   to keep the cached connection open between calls.

   Example usage:

     (with-open-connection (conn my-db)
       (dbi:do-sql conn \"SELECT 1\"))"
  (let ((db-sym (gensym "DB")))
    `(let* ((,db-sym ,db)
            (,var   (connect-cached ,db-sym)))
       (unwind-protect
            (progn ,@body)
         (when ,disconnect
           (ignore-errors
            (dbi:disconnect ,var)))))))

(defmethod initialize-instance :after ((db db/sqlite) &key &allow-other-keys)
  (log:info "INITIALIZING DB/SQLITE")
  (let ((dbc (connect-cached db)))
    (dolist (command '("PRAGMA busy_timeout = 30000;"
                       "PRAGMA journal_mode = WAL;"))
      (dbi:do-sql dbc command))))

(defmethod initialize-instance :after ((db db-backend) &key)
  (log:info "INITIALIZING DB")
  (with-open-connection (dbc db)
    (dbi:do-sql
     dbc
     "CREATE TABLE IF NOT EXISTS events (
        ts           BIGINT  NOT NULL,
        user_id      BIGINT  NOT NULL,
        challenge_id BIGINT  NOT NULL,
        event_type   SMALLINT NOT NULL,      -- 1 = SUBMIT, 2 = HINT
        points       INTEGER NOT NULL,
        hint_number  INTEGER,
        CHECK (event_type IN (1, 2)),
        CHECK ( (event_type = 2 AND hint_number IS NOT NULL)
             OR (event_type = 1 AND hint_number IS NULL) ),
        PRIMARY KEY (ts, user_id, event_type)
      );")
    (dbi:do-sql
      dbc (slot-value db 'sql-create-user-table-statement))))

(defun record-flag (db user challenge)
  (let ((ts (now-micros)))
    (with-open-connection (conn db)
      (let* ((sql "INSERT INTO events
                   (ts, user_id, challenge_id, event_type, points)
                 VALUES (?, ?, ?, ?, ?)")
             (stmt (dbi:prepare conn sql)))
        (dbi:execute stmt (list ts
                                (user-id user)
                                (challenge-id challenge)
                                1                                   ; SUBMIT
                                (challenge-points challenge)))))
    ts))

(defun collect-events-since (db ts)
  (with-open-connection (conn db)
    (let* ((stmt (dbi:prepare conn
                              "SELECT ts, user_id, challenge_id,
                                      event_type, hint_number, points
                                 FROM events
                                WHERE ts > ?
                                ORDER BY ts"))
           (res  (dbi:execute stmt (list ts))))
      (loop for row = (dbi:fetch res)
            while row
            collect (make-event
                     :ts            (getf row :|ts|)
                     :user-id       (getf row :|user_id|)
                     :challenge-id  (getf row :|challenge_id|)
                     :event-type    (getf row :|event_type|)
                     :hint-number   (getf row :|hint_number|)
                     :points        (getf row :|points|))))))
