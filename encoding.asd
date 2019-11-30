;;;   -*- Mode: LISP; Package: COMMON-LISP-USER; BASE: 10; Syntax: ANSI-Common-Lisp; -*-

(in-package :cl-user)

(asdf:defsystem encoding
  :name "encoding"
  ;;:source-pathname "lisp:projects;utilities;"
  :author "Paul Meurer <paul.meurer@uib.no>"
  :maintainer "Paul Meurer <paul.meurer@uib.no>"
  :licence "Lesser Lisp General Public License"
  :description "Character Encoding"
  :depends-on (:utilities :dat)
  :serial t
  :components ((:file "encoding-package")
	       #-sbcl(:file "char-entities")
	       (:file "utf-8")))

:eof