;;; package.lisp
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2025 Anthony Green

(defpackage #:ctfg
  (:use #:cl)
  (:local-nicknames (:lh :org.shirakumo.luckless.hashtable)
                    (:ll :org.shirakumo.luckless.list))
  (:export #:main))
