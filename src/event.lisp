;;; event.lisp
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2025 Anthony Green

(in-package :ctfg)

(defstruct event
  id ts user-id challenge-id event-type points hint-number)
