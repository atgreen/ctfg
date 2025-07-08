;;; ctfg.asd
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2025 Anthony Green

(asdf:defsystem #:ctfg
  :description "A simple Capture-The-Flag engine"
  :author      "Anthony Green"
  :license     "MIT"
  :version     "0.1.5"
  :depends-on  (:version-string :clingon :hunchentoot :easy-routes :log4cl
                :markup :slynk :cl-json :cl-csv :clws :sqlite
                :cl-dbi :dbd-sqlite3 :luckless :sentry-client :cl-ppcre
                :cl-dotenv :cl-yaml :bordeaux-threads)
  :serial      t
  :components  ((:file "src/package")
                (:file "src/rwlock")
                (:file "src/challenges")
                (:file "src/db")
                (:file "src/user")
                (:file "src/event")
                (:file "src/clients")
                (:file "src/server")
                (:file "src/main"))
  :build-operation "program-op"
  :build-pathname "ctfg"
  :entry-point "ctfg:main")

#+sb-core-compression
(defmethod asdf:perform ((o asdf:image-op) (c asdf:system))
  (uiop:dump-image (asdf:output-file o c) :executable t :compression t))
