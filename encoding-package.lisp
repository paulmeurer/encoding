;;;-*- Mode: Lisp; Package: COMMON-LISP-USER -*-

(in-package :cl-user)

(defpackage :encoding
  (:use #+mcl "CCL" "COMMON-LISP" "UTILS")
  (:export "WRITE-UTF-8-ENCODED" "UTF-8-ENCODE" "UTF-8-DECODE" "UTF-8-CODE-LENGTH"
	   "ENTITY-TO-UTF-8" "UTF-8-DECODE-OCTETS"
	   "ENTITY-TO-CHAR" "ENTITIES-TO-CHARS"))

:eof
