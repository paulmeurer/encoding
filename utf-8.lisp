;;;-*- Mode: Lisp; Package: ENCODING -*-
;;
;; Copyright (C) Paul Meurer 2001-2006. All rights reserved.
;; paul.meurer@uib.no
;; University of Bergen
;;
;; utf-8 conversion
;;

(in-package :encoding)

;; returns number of chars (= bytes) written
;; iterative
;; string-to-entity-table is a string tree which maps chars to entities, i.e. if "a~" -> "atilde" is an entry
;;   in that table, each occurrence of "a~" should be replaced by "&atilde;".
;; entity-to-string-table is a string tree which maps entities to strings
;;   or numerical entities, i.e. &yAFcros; -> &#xF41A;
(defun write-utf-8-encoded (string stream &key (start 0) end entity-to-string-table
                                            string-to-entity-table char-to-entity-list)
  ;;(declare (optimize (speed 3)) (fixnum start end))
  ;;(unless end (setf end (length (or string ""))))
  (when string
    (loop with count fixnum = 0 and end fixnum = (or end (length string))
       for pos fixnum from start
       until (= pos end)
       do (multiple-value-bind (value wholep match-end rest partial-value)
	      (dat::string-tree-get string-to-entity-table string nil pos end)
	    (declare (ignore rest))
	    (multiple-value-bind (ent-value ent-wholep ent-match-end rest ent-partial-value)
	        (dat::string-tree-get entity-to-string-table string nil pos end)
              (declare (ignore rest))
	      (let* ((char (char string pos))
		     (code (char-code char))
		     (value (or (and wholep value) partial-value))
                     (ent-value (or (and ent-wholep ent-value) ent-partial-value)))
	        (cond (value
		       (write-char #\& stream)
		       (write-string value stream)
		       (write-char #\; stream)
		       (setf pos (1- (or match-end end))))
                      (ent-value
		       (write-string ent-value stream)
		       (setf pos (1- (or ent-match-end end))))
		      ((and char-to-entity-list (find char char-to-entity-list))
		       (write-char #\& stream)
		       (write-string (cadr (member char char-to-entity-list)) stream)
		       (write-char #\; stream))
		      ((< code #x80)
		       (write-char char stream)
		       (incf count))
		      (t
		       (incf count (write-unicode-to-utf-8 code stream)))))))
       finally (return count))))

;; the same for a vector of octets, but only pure utf8 encoding, no entities
(defun write-octets-utf-8-encoded (octets stream &key (start 0) end)
  ;;(declare (optimize (speed 3)) (fixnum start end))
  (when octets
    (loop with count fixnum = 0 and end fixnum = (or end (length octets))
       for pos fixnum from start
       until (= pos end)
       do (let* ((code (aref octets pos)))
	    (cond ((< code #x80)
		   (write-char (code-char code) stream)
		   (incf count))
		  (t
		   (incf count (write-unicode-to-utf-8 code stream)))))
       finally (return count))))

#+test
(write-utf-8-encoded "the corpuscle web interæface" *standard-output* :start 18 :end nil :string-to-entity-table nil)

#+test
(print (utf-8-decode (utf-8-encode "се полъ имѣниѣ моего г҃и дамъ ништиимъ. ꙇ аште е҅смъ кого чимь обидѣлъ. възвращѫ четворицеѭ҄.")))

#+test
(write-utf-8-encoded (format nil "laf&boll'~cfff" (code-char 248)) *standard-output* :string-to-entity-table lxml::*string-to-entity-table*)

#+old
(defun write-utf-8-encoded (string stream &key (start 0) (end (length (or string ""))) string-to-entities-table)
  (declare (optimize (speed 3))
	   (fixnum start end))
  (when string
    (loop with count fixnum = 0
	  for pos fixnum from start
	  until (= pos end)
	  do (let ((code (char-code (char string pos))))
	       (cond ((< code #x80)
		      (write-char (char string pos) stream)
		      (incf count))
		     (t
		      (incf count (write-unicode-to-utf-8 code stream)))))
	  finally (return count))))

;; tests inverseness of functions on arbitrary 16bit-char strings
#+test
(dotimes (i 100)
  (let ((string (make-string 1000)))
    (loop for i from 0 to 999
	  do (setf (char string i) (code-char (random (expt 2 16)))))
    (assert (string= string (utf-8-decode (utf-8-encode string))))))

#+test
(dolist (n '(35000 40000))
  (let ((foo (make-string n :initial-element #\a)))
    (format t "~%Trying n=~a" n)
    (encoding::utf-8-decode foo)
    (print "No overflow.")))

;;(print (utf-8-encode (string #\U+A647)))

;; this crashes Slime:
;; (print (string #\U+A647))

;; new, iterative
;; when resolve-entities-only-p = T; no utf-8 decoding is performed (works only for numerical entities)
(defun utf-8-decode (string &optional resolve-entities-p external-entities warn-p resolve-entities-only-p)
  ;; #-openmcl(declare (optimize (safety 0) (speed 3)))
  (when string
    (let ((length (length string))
	  (pos 0))
      (declare (fixnum pos))
      (with-output-to-string
	  #+allegro (stream)
	  #+sbcl (stream)
	  #+ccl (stream nil :element-type 'character)
	  #+lispworks (stream nil :element-type 'lispworks::simple-char)
	  (labels ((decode-one (code size)
		     (incf pos)
		     (cond ((zerop size)
			    ;;(when (> code #x65533) (warn "too big? ~d" code))
			    (write-char (code-char code) stream))
			   ((>= pos length)
			    (error "The string ~s does not seem to be UTF-8." string))
			   (t
			    (decf size 6)
			    (decode-one (+ code (ash (logand #b00111111
							     (char-code (char string pos)))
						     size))
					size)))))
	    (loop when (> pos length)
		  do (error "The string ~s does not seem to be UTF-8." string)
		  while (< pos length)
		  do (let* ((char (char string pos))
			    (code (char-code char)))
		       (cond ((and resolve-entities-p
				   (char= char #\&))
			      (let ((ent-end (position #\; string :start (1+ pos) :end (min (length string) (+ pos 16)))))
				(cond ((null ent-end)
				       (if warn-p
					   (warn "utf-8-decode(): Could not find entity end marker ';' in ~s." string)
					   (error "utf-8-decode(): Could not find entity end marker ';' in ~s." string))
				       (write-char char stream)
				       (incf pos))
				      (t
				       ;;(subseq string (1+ pos) ent-end)
				       (cond ((char= (char string (1+ pos)) #\#) ;; char code
					      (if (char= (char string (+ pos 2)) #\x)
						  (write-char (code-char
							       (parse-integer string :radix 16
									      :start (+ pos 3)
									      :end ent-end))
							      stream)
						  (write-char (code-char
							       (parse-integer string :radix 10
									      :start (+ pos 2)
									      :end ent-end))
							      stream)))
					     (t
					      (let* ((ent (subseq string (1+ pos) ent-end))
						     (char (entity-to-char ent)))
						(cond (char
						       (write-char char stream))
						      (t
						       (let ((ent-res (cadr (member ent external-entities :test #'string=))))
							 (cond (ent-res
								(write-string ent-res stream))
							       (t ;; if entity can't be resolved write plain entity
								(write-char #\& stream)
								(write-string ent stream)
								(write-char #\; stream)))))))))
				       (incf pos (- ent-end pos -1))))))
			     ((or resolve-entities-only-p
				  (zerop (logand #b10000000 code))) ;; TO DO: reformulate using ldb()
			      (incf pos)
			      (write-char char stream))
			     ((= (logand #b11100000 code) #b11000000)
			      (decode-one (ash (logand #b00011111 code) 6) 6))
			     ((= (logand #b11110000 code) #b11100000)
			      (decode-one (ash (logand #b00001111 code) 12) 12))
			     ((= (logand #b11111000 code) #b11110000)
			      (decode-one (ash (logand #b00000111 code) 18) 18))
			     ((= (logand #b11111100 code) #b11111000)
			      (decode-one (ash (logand #b00000011 code) 24) 24))
			     ((= (logand #b11111110 code) #b11111100)
			      (decode-one (ash (logand #b00000001 code) 30) 30))
			     (t
			      (if warn-p
				  (warn "Code ~d at position ~d not recognized" code pos)
				  (error "Code ~d at position ~d not recognized" code pos))
			      (write-char char stream)
			      (incf pos))))))))))

(defun utf-8-decode-octets (octets &optional warn-p)
  #-openmcl(declare (optimize (safety 0) (speed 3)))
  ;;(print octets)
  (when octets
    (let ((length (length octets))
	  (pos 0))
      (declare (fixnum pos))
      (with-output-to-string
	  #+allegro (stream)
	  #+sbcl (stream)
	  #+ccl (stream nil :element-type 'character)
	  #+lispworks (stream nil :element-type 'lispworks::simple-char)
	  (labels ((decode-one (code size)
		     (incf pos)
		     (cond ((zerop size)
			    (write-char (code-char code) stream))
			   ((>= pos length)
			    (error "The octet vector ~s does not seem to be UTF-8." octets))
			   (t
			    (decf size 6)
			    (decode-one (+ code (ash (logand #b00111111
							     (aref octets pos))
						     size))
					size)))))
	    (loop when (> pos length)
		  do (error "The octet vector ~s does not seem to be UTF-8." octets)
		  while (< pos length)
		  do (let* ((code (aref octets pos)))
		       (cond ((zerop (logand #b10000000 code)) ;; TO DO: reformulate using ldb()
			      (incf pos)
			      (write-char (code-char code) stream))
			     ((= (logand #b11100000 code) #b11000000)
			      (decode-one (ash (logand #b00011111 code) 6) 6))
			     ((= (logand #b11110000 code) #b11100000)
			      (decode-one (ash (logand #b00001111 code) 12) 12))
			     ((= (logand #b11111000 code) #b11110000)
			      (decode-one (ash (logand #b00000111 code) 18) 18))
			     ((= (logand #b11111100 code) #b11111000)
			      (decode-one (ash (logand #b00000011 code) 24) 24))
			     ((= (logand #b11111110 code) #b11111100)
			      (decode-one (ash (logand #b00000001 code) 30) 30))
			     (t
			      (if warn-p
				  (warn "Code ~d at position ~d not recognized" code pos)
				  (error "Code ~d at position ~d not recognized" code pos))
			      (write-char (code-char code) stream)
			      (incf pos))))))))))

(defun utf-8-encode (string &key char-to-entity-list)
  (let ((length nil))
    (values (with-output-to-string
		#-lispworks
	      (stream)
	      #+lispworks
	      (stream nil :element-type 'lispworks::simple-char)
	      (setf length (write-utf-8-encoded (or string "") stream
						:char-to-entity-list char-to-entity-list)))
	    length)))

(defun utf-8-encode-octets (octets)
  (let ((length nil))
    (values (with-output-to-string
		#-lispworks
	      (stream)
	      #+lispworks
	      (stream nil :element-type 'lispworks::simple-char)
	      (setf length (write-octets-utf-8-encoded (or octets #()) stream)))
	    length)))

(defun entity-to-utf-8 (entity)
  (with-output-to-string (stream)
    (let ((code (parse-integer entity :start 3 :junk-allowed t :radix 16)))
      (write-unicode-to-utf-8 code stream))))

(defun write-unicode-to-utf-8 (code stream)
  #+mcl(setf code (mac-to-unix-char-code code))
  (cond ((< code #x80)
         (write-char (code-char code) stream)
	 1)
        ((< code #x800)
         (write-char (code-char (logxor #b11000000 (ash code -6))) stream)
         (write-char (code-char (logxor #b10000000 (logand #b00111111 code))) stream)
	 2)
        ((< code #x10000)
         (write-char (code-char (logxor #b11100000 (ash code -12))) stream)
         (write-char (code-char (logxor #b10000000 (logand #b00111111 (ash code -6)))) stream)
         (write-char (code-char (logxor #b10000000 (logand #b00111111 code))) stream)
	 3)
        ((< code #x200000)
         (write-char (code-char (logxor #b11110000 (ash code -18))) stream)
         (write-char (code-char (logxor #b10000000 (logand #b00111111 (ash code -12)))) stream)
         (write-char (code-char (logxor #b10000000 (logand #b00111111 (ash code -6)))) stream)
         (write-char (code-char (logxor #b10000000 (logand #b00111111 code))) stream)
	 4)
        ((< code #x4000000)
         (write-char (code-char (logxor #b11111000 (ash code -24))) stream)
         (write-char (code-char (logxor #b10000000 (logand #b00111111 (ash code -18)))) stream)
         (write-char (code-char (logxor #b10000000 (logand #b00111111 (ash code -12)))) stream)
         (write-char (code-char (logxor #b10000000 (logand #b00111111 (ash code -6)))) stream)
         (write-char (code-char (logxor #b10000000 (logand #b00111111 code))) stream)
	 5)
        ((< code #x80000000)
         (write-char (code-char (logxor #b11111100 (ash code -30))) stream)
         (write-char (code-char (logxor #b10000000 (logand #b00111111 (ash code -24)))) stream)
         (write-char (code-char (logxor #b10000000 (logand #b00111111 (ash code -18)))) stream)
         (write-char (code-char (logxor #b10000000 (logand #b00111111 (ash code -12)))) stream)
         (write-char (code-char (logxor #b10000000 (logand #b00111111 (ash code -6)))) stream)
         (write-char (code-char (logxor #b10000000 (logand #b00111111 code))) stream)
	 6)))

(defun utf-8-code-length (char-code)
  (cond ((< char-code #x80)
         1)
        ((< char-code #x800)
         2)
        ((< char-code #x10000)
         3)
        ((< char-code #x200000)
         4)
        ((< char-code #x4000000)
         5)
        ((< char-code #x80000000)
         6)))

:eof
