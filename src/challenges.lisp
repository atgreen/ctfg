;;; challenges.lisp
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2025 Anthony Green

(in-package :ctfg)

(defvar *all-challenges* nil)

(defstruct challenge
  id title category difficulty points description content flag hints requirements
  (min-points 0))

(defun read-challenges (challenges.json)
  (let ((clist (list)))
    (let ((raw (cl-json:decode-json-from-string challenges.json)))
      (dolist (challenge raw)
        (push (make-challenge
               :id          (cdr (assoc :id challenge))
               :title       (cdr (assoc :title challenge))
               :category    (cdr (assoc :category challenge))
               :difficulty  (cdr (assoc :difficulty challenge))
               :points      (cdr (assoc :points challenge))
               :description (cdr (assoc :description challenge))
               :content     (cdr (assoc :content challenge))
               :flag        (cdr (assoc :flag challenge))
               :hints       (cdr (assoc :hints challenge))
               :requirements (cdr (assoc :requirements challenge))
               :min-points  (or (cdr (assoc :min-points challenge)) 0))
              clist)))
    (log:info (format nil "Read ~A challenges." (length clist)))
    (setf *all-challenges* clist)))

(defun earned-points (solves)
  "Total points earned from the solved challenge IDs in SOLVES.
Hint purchases don't reduce this, so a points-gated challenge can
never disappear once revealed."
  (loop for challenge in *all-challenges*
        when (member (challenge-id challenge) solves :test #'=)
          sum (challenge-points challenge)))

(defun challenge-available-p (challenge solves)
  "A challenge is available once all of its required challenges are
solved and the player has earned at least its minimum points."
  (and (subsetp (challenge-requirements challenge) solves :test #'=)
       (<= (challenge-min-points challenge) (earned-points solves))))

(defun available-challenges (solves)
  "Give a list of solved challenge IDs (SOLVES), return
the list of challenges available."
  (log:info (format nil "SOLVES: ~A" solves))
  (loop for challenge in *all-challenges*
        when (challenge-available-p challenge solves)
          collect challenge))
