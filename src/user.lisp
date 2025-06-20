;;; user.lisp
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2025 Anthony Green

(in-package :ctfg)

(defvar *username-table* (make-hash-table))
(defvar *displayname-table* (make-hash-table))

(defstruct user
  id username displayname total-points solved-challenges)

;;;------------------------------------------------------------------
;;;  Fetch (username . display-name)  — reads cache first, hits DB once
;;;------------------------------------------------------------------
(defun get-user-name-pair (id)
  "Return a CONS whose CAR is the username and CDR is the display name
   for user ID.  Caches the result in two hash-tables for fast reuse."
  (let ((u  (gethash id *username-table*))
        (dn (gethash id *displayname-table*)))
    (when (and u dn)                     ; ⟹ both already cached
      (return-from get-user-name-pair (cons u dn)))

    (log:info "GET-USER-NAME-PAIR FROM DB")

    ;; ── not fully cached → fetch from database ─────────────────────
    (with-open-connection (conn *db*)
      (let* ((stmt (dbi:prepare conn
                   "SELECT username, displayname
                      FROM users
                     WHERE id = ?"))
             (row  (dbi:fetch (dbi:execute stmt (list id))))
             ;; column names come back exactly as written
             (username     (getf row :|username|))
             (displayname (getf row :|displayname|)))

        ;; update caches for next time
        (setf (gethash id *username-table*)    username
              (gethash id *displayname-table*) displayname)

        (cons username displayname)))))

(defun get-displayname (id)
  (let ((name-pair (get-user-name-pair id)))
    (cdr name-pair)))

(defun set-displayname (db user displayname)
  "Persist DISPLAYNAME in the users table for USER.
   Returns DISPLAYNAME so the caller can reuse it."
  (setf (user-displayname user) displayname)
  (with-open-connection (conn db)
    (let* ((sql  "UPDATE users
                    SET displayname = ?
                  WHERE id = ?")
           (stmt (dbi:prepare conn sql)))
      (dbi:execute stmt (list displayname
                              (user-id user)))))
  displayname)

(defun ensure-user (db username)
  "Insert USERNAME if it is new.  Return a USER struct with db-assigned id."
  (with-open-connection (conn db)  ; keep pooled handle
    (let* ((insert-sql (cond ((typep db 'db/pgsql)
                              ;; RETURNING works only on Postgres
                              "INSERT INTO users (userame)
                                 VALUES (?)
                                 ON CONFLICT ON CONSTRAINT users_username_key DO NOTHING
                                 RETURNING id")
                             ((typep db 'db/mysql)
                              ;; MySQL 8 syntax
                              "INSERT IGNORE INTO users (username) VALUES (?);")
                             (t              ; sqlite
                              "INSERT OR IGNORE INTO users (username) VALUES (?);")))
           (stmt  (dbi:prepare conn insert-sql)))

      ;; 1. try the insert (or no-op if username exists)
      (dbi:execute stmt (list username))

      ;; 2. fetch the id
      (let* ((stmt (dbi:prepare conn
                                "SELECT id, displayname FROM users WHERE username = ?"))
             (row  (dbi:fetch (dbi:execute stmt (list username))))
             ;; column names come back exactly as written
             (id   (getf row :|id|))
             (displayname (getf row :|displayname|)))
        (make-user :id id :username username :displayname displayname :total-points 0)))))
