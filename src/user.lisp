;;; user.lisp – cl-sqlite edition
;;;
;;; SPDX-License-Identifier: MIT
;;; Copyright (C) 2025 Anthony Green

(in-package :ctfg)

;;; --- Global Caches ---
(defvar *user-id-to-username* (lh:make-castable)
  "Cache mapping user ID to username (ID -> Username).")

(defvar *user-id-to-displayname* (lh:make-castable)
  "Cache mapping user ID to display name (ID -> DisplayName).")

(defvar *username-to-user-object* (lh:make-castable :test #'equal)
  "Cache mapping username to the full USER struct (Username -> User Object).")

(defstruct (user (:print-object print-user))
  "Represents a user in the CTF system."
  id
  username
  displayname
  (total-points 0 :type integer) ; Initialize total-points to 0, specify type
  (solved-challenges nil :type list)) ; Solved challenges could be a list of IDs or names

;; Custom print function for the user struct for better debugging
(defun print-user (user stream)
  (print-unreadable-object (user stream :type t :identity t)
    (format stream "ID: ~a Username: ~a DisplayName: ~a Points: ~a Solved: ~a"
            (user-id user)
            (user-username user)
            (user-displayname user)
            (user-total-points user)
            (length (user-solved-challenges user))))) ; Show count for brevity

;;; ------------------------------------------------------------------
;;;  Database Helpers
;;; ------------------------------------------------------------------

(defun %fetch-one (conn sql &rest args)
  "Run SQL with ARGS and return the first row as a list (or NIL).
   This is a thin wrapper around `sqlite:execute-to-list`."
  (first (apply #'sqlite:execute-to-list conn sql args)))

(defun %execute (conn sql &rest args)
  "Execute a non-query statement (INSERT/UPDATE/DELETE …) with ARGS.
   This is a thin wrapper around `sqlite:execute-non-query`."
  (apply #'sqlite:execute-non-query conn sql args))

;;; ------------------------------------------------------------------
;;;  User Fetching and Caching
;;; ------------------------------------------------------------------

(defun get-user-name-pair (id)
  "Return (VALUES username display-name) for user ID, caching results.
   Returns NIL for both if ID is not found."
  (multiple-value-bind (u u-present-p) (lh:gethash id *user-id-to-username*)
    (multiple-value-bind (dn dn-present-p) (lh:gethash id *user-id-to-displayname*)
      (when (and u-present-p dn-present-p)
        (return-from get-user-name-pair (values u dn)))))

  (with-open-connection (conn *db*)
    (destructuring-bind (username displayname)
        (%fetch-one conn
                    "SELECT username, displayname FROM users WHERE id = ?" id)
      (when username ; If a user is found (username will be NIL if no row)
        ;; Update caches
        (setf (lh:gethash id *user-id-to-username*) username
              (lh:gethash id *user-id-to-displayname*) displayname)
        (values username displayname)))))

(defun get-displayname (id)
  "Returns the display name for a given user ID.
   Returns NIL if the user is not found."
  (nth-value 1 (get-user-name-pair id)))

(defun get-username (id)
  "Returns the username for a given user ID.
   Returns NIL if the user is not found."
  (nth-value 0 (get-user-name-pair id)))


(defun get-user-by-username (username)
  "Retrieve a USER struct by username, first from cache, then from DB.
   Populates `solved-challenges` and `total-points` upon fetching from DB.
   Returns NIL if the user is not found."
  (let ((user (lh:gethash username *username-to-user-object*)))
    (when user
      (return-from get-user-by-username user)))

  (with-open-connection (conn *db*)
    (let ((user-data (%fetch-one conn
                                 "SELECT id, displayname FROM users WHERE username = ?" username)))
      (unless user-data
        ;; If no user data is found, return NIL immediately
        (return-from get-user-by-username nil))

      (destructuring-bind (id displayname) user-data
        (let* ((total-points (or (car (%fetch-one conn
                                                    "SELECT COALESCE(SUM(points),0)
                                                     FROM events WHERE user_id = ?" id))
                                 0))
               ;; --- CORRECTED SQL QUERY FOR SOLVED CHALLENGES ---
               ;; A challenge is 'solved' if there's an 'event_type = 1' (SUBMIT)
               ;; for that user and challenge. We want distinct challenge IDs.
               (solved-challenge-ids (mapcar #'car
                                             (sqlite:execute-to-list
                                              conn
                                              "SELECT DISTINCT challenge_id
                                               FROM events
                                               WHERE user_id = ? AND event_type = 1" ; 1 = SUBMIT
                                              id))))

          (let ((user (make-user :id id
                                 :username username
                                 :displayname displayname
                                 :total-points total-points
                                 :solved-challenges solved-challenge-ids)))
            ;; Update all relevant caches
            (setf (lh:gethash id *user-id-to-username*) username
                  (lh:gethash id *user-id-to-displayname*) displayname
                  (lh:gethash username *username-to-user-object*) user)
            user))))))

;;; ------------------------------------------------------------------
;;;  User Updates
;;; ------------------------------------------------------------------

(defun displayname-exists-p (db displayname &optional exclude-user-id)
  "Check if a display name is already taken by another user.
   If EXCLUDE-USER-ID is provided, that user is excluded from the check
   (useful when checking if a user can keep their current name)."
  (with-open-connection (conn db)
    (let ((result (if exclude-user-id
                      (%fetch-one conn
                                  "SELECT 1 FROM users WHERE displayname = ? AND id != ? LIMIT 1"
                                  displayname exclude-user-id)
                      (%fetch-one conn
                                  "SELECT 1 FROM users WHERE displayname = ? LIMIT 1"
                                  displayname))))
      (not (null result)))))

(defun set-displayname (db user-object displayname)
  "Persist DISPLAYNAME for USER-OBJECT in the database and return the updated USER-OBJECT.
   Updates relevant caches.
   'db' is expected to be your CTFG::DB/SQLITE object, which with-open-connection handles."
  (check-type user-object user "a USER struct")
  (setf (user-displayname user-object) displayname) ; Update in-memory struct

  (with-open-connection (conn db)
    (%execute conn
              "UPDATE users SET displayname = ? WHERE id = ?"
              displayname
              (user-id user-object)))

  ;; Update caches
  (setf (lh:gethash (user-id user-object) *user-id-to-displayname*) displayname)
  user-object)

;;; ------------------------------------------------------------------
;;;  Insert-or-fetch helper
;;; ------------------------------------------------------------------

(defun ensure-user (db username)
  "Create USERNAME if it is new and return a fully-populated USER struct.
   Uses the `get-user-by-username` function to check for existing user.
   'db' is expected to be your CTFG::DB/SQLITE object, which with-open-connection handles."
  ;; First, try to fetch from cache/DB
  (let ((user (get-user-by-username username)))
    (when user
      (return-from ensure-user user)))

  ;; If not found, insert and then fetch
  (with-open-connection (conn db)
    (%execute conn
              "INSERT OR IGNORE INTO users (username) VALUES (?);"
              username)
    ;; After insert, the user should definitely exist, so fetch it
    ;; This will also update all caches appropriately.
    (get-user-by-username username)))
