;;; user.lisp  –  cl-sqlite edition
;;;
;;; SPDX-License-Identifier: MIT
;;; Copyright (C) 2025 Anthony Green

(in-package :ctfg)

(defvar *username-table*    (lh:make-castable))   ; id ⇒ username
(defvar *displayname-table* (lh:make-castable))   ; id ⇒ display-name

(defstruct user
  id username displayname total-points solved-challenges)

;;; ------------------------------------------------------------------
;;;  Helpers
;;; ------------------------------------------------------------------

(defun %fetch-one (conn sql &rest args)
  "Run SQL with ARGS and return the first row as a list (or NIL)."
  (first (apply #'sqlite:execute-to-list conn sql args)))

(defun %execute (conn sql &rest args)
  "Execute a non-query statement (INSERT/UPDATE/DELETE …) with ARGS."
  (apply #'sqlite:execute-non-query conn sql args))

;;; ------------------------------------------------------------------
;;;  Fetch (username . display-name)  — cache first, DB on miss
;;; ------------------------------------------------------------------

(defun get-user-name-pair (id)
  "Return CONS (username . display-name) for user ID, caching results."
  (let ((u  (lh:gethash id *username-table*))
        (dn (lh:gethash id *displayname-table*)))
    (when (and u dn)
      (return-from get-user-name-pair (cons u dn))))

  (with-open-connection (conn *db*)
    (destructuring-bind (username displayname)
        (%fetch-one conn
                    "SELECT username, displayname
                         FROM users
                        WHERE id = ?" id)
      ;; update caches
      (setf (lh:gethash id *username-table*)    username
            (lh:gethash id *displayname-table*) displayname)
      (cons username displayname))))

(defun get-displayname (id)
  (cdr (get-user-name-pair id)))

;;; ------------------------------------------------------------------
;;;  Updates
;;; ------------------------------------------------------------------

(defun set-displayname (db user displayname)
  "Persist DISPLAYNAME for USER in the database and return it."
  (setf (user-displayname user) displayname)
  (with-open-connection (conn db)
    (%execute conn
              "UPDATE users
                  SET displayname = ?
                WHERE id = ?"
              displayname
              (user-id user)))
  displayname)

;;; ------------------------------------------------------------------
;;;  Insert-or-fetch helper
;;; ------------------------------------------------------------------

(defun ensure-user (db username)
  "Create USERNAME if it is new and return a fully-populated USER struct."
  (unless (typep db 'db/sqlite)
    (error "ensure-user: only db/sqlite is supported in the cl-sqlite build."))

  (with-open-connection (conn db)
    (%execute conn
              "INSERT OR IGNORE INTO users (username) VALUES (?);"
              username)

    (destructuring-bind (id displayname)
        (%fetch-one conn
                    "SELECT id, displayname
                       FROM users
                      WHERE username = ?"
                    username)

      (destructuring-bind (id displayname)
          (%fetch-one conn
                      "SELECT id, displayname
                   FROM users
                  WHERE username = ?" username)

        (let* ((pts (or (car (%fetch-one conn
                                         "SELECT COALESCE(SUM(points),0)
                           FROM events
                          WHERE user_id = ?" id))
                        0)))

          (make-user :id id
                     :username username
                     :displayname displayname
                     :total-points pts))))))
