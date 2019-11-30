;;;-*- Mode: Lisp; Package: ENCODING -*-
;;
;; Copyright (C) Paul Meurer 2001-2004. All rights reserved.
;; paul.meurer@aksis.uib.no
;; Aksis, University of Bergen
;;
;; entities to chars
;;

;;-------------------------------------------------------------------------------------
;; TO DO:
;;-------------------------------------------------------------------------------------

(in-package :encoding)

(defvar *entity-to-char-table* (make-hash-table :test #'equal))

(defun entity-to-char (entity-string)
  (gethash entity-string *entity-to-char-table*)) 

(defun entities-to-chars (string)
  (labels ((convert (pos)
	     (let* ((start (position #\& string :start pos))
		    (end (when start (position #\; string :start start)))
		    (entity (when end (subseq string (1+ start) end)))
		    (char (entity-to-char entity)))
	       (cond (char
		      (concat (subseq string pos start)
			      (string char)
			      (convert (1+ end))))
		     ((or end start)
		      (concat (subseq string pos (1+ (or end start)))
			      (convert (1+ (or end start)))))
		     ((= pos 0)
		      string)
		     (t
		      (subseq string pos))))))
    (convert 0)))

(loop for (entity char)
    on
      '(
	;; iso lat 1
	"aacute" #\á ;; "&#x00E1;" ;; <!-- LATIN SMALL LETTER A WITH ACUTE -->
	"Aacute" #\Á ;; "&#x00C1;" ;; <!-- LATIN CAPITAL LETTER A WITH ACUTE -->
	"acirc" #\â ;; "&#x00E2;" ;; <!-- LATIN SMALL LETTER A WITH CIRCUMFLEX -->
	"Acirc" #\Â ;; "&#x00C2;" ;; <!-- LATIN CAPITAL LETTER A WITH CIRCUMFLEX -->
	"agrave" #\à ;; "&#x00E0;" ;; <!-- LATIN SMALL LETTER A WITH GRAVE -->
	"Agrave" #\À ;; "&#x00C0;" ;; <!-- LATIN CAPITAL LETTER A WITH GRAVE -->
	"aring" #\å ;; "&#x00E5;" ;; <!-- LATIN SMALL LETTER A WITH RING ABOVE -->
	"Aring"	#\Å ;; "&#x00C5;" ;; <!-- LATIN CAPITAL LETTER A WITH RING ABOVE -->
	"atilde" #\ã ;; "&#x00E3;" ;; <!-- LATIN SMALL LETTER A WITH TILDE -->
	"Atilde" #\Ã ;; "&#x00C3;" ;; <!-- LATIN CAPITAL LETTER A WITH TILDE -->
	"auml" #\ä ;; "&#x00E4;" ;; <!-- LATIN SMALL LETTER A WITH DIAERESIS -->
	"Auml" #\Ä ;; "&#x00C4;" ;; <!-- LATIN CAPITAL LETTER A WITH DIAERESIS -->
	"aelig" #\æ ;; "&#x00E6;" ;; <!-- LATIN SMALL LETTER AE -->
	"AElig" #\Æ ;; "&#x00C6;" ;; <!-- LATIN CAPITAL LETTER AE -->
	"ccedil" #\ç ;; "&#x00E7;" ;; <!-- LATIN SMALL LETTER C WITH CEDILLA -->
	"Ccedil" #\Ç ;; "&#x00C7;" ;; <!-- LATIN CAPITAL LETTER C WITH CEDILLA -->
	"eth" #\ð ;; "&#x00F0;" ;; <!-- LATIN SMALL LETTER ETH -->
	"ETH" #\Ð ;; "&#x00D0;" ;; <!-- LATIN CAPITAL LETTER ETH -->
	"eacute" #\é ;; "&#x00E9;" ;; <!-- LATIN SMALL LETTER E WITH ACUTE -->
	"Eacute" #\É ;; "&#x00C9;" ;; <!-- LATIN CAPITAL LETTER E WITH ACUTE -->
	"ecirc" #\ê ;; "&#x00EA;" ;; <!-- LATIN SMALL LETTER E WITH CIRCUMFLEX -->
	"Ecirc" #\Ê ;; "&#x00CA;" ;; <!-- LATIN CAPITAL LETTER E WITH CIRCUMFLEX -->
	"egrave" #\è ;; "&#x00E8;" ;; <!-- LATIN SMALL LETTER E WITH GRAVE -->
	"Egrave" #\È ;; "&#x00C8;" ;; <!-- LATIN CAPITAL LETTER E WITH GRAVE -->
	"euml" #\ë ;; "&#x00EB;" ;; <!-- LATIN SMALL LETTER E WITH DIAERESIS -->
	"Euml" #\Ë ;; "&#x00CB;" ;; <!-- LATIN CAPITAL LETTER E WITH DIAERESIS -->
	"iacute" #\í ;; "&#x00ED;" ;; <!-- LATIN SMALL LETTER I WITH ACUTE -->
	"Iacute" #\Í ;; "&#x00CD;" ;; <!-- LATIN CAPITAL LETTER I WITH ACUTE -->
	"icirc" #\î ;; "&#x00EE;" ;; <!-- LATIN SMALL LETTER I WITH CIRCUMFLEX -->
	"Icirc" #\Î ;; "&#x00CE;" ;; <!-- LATIN CAPITAL LETTER I WITH CIRCUMFLEX -->
	"igrave" #\ì ;; "&#x00EC;" ;; <!-- LATIN SMALL LETTER I WITH GRAVE -->
	"Igrave" #\Ì ;; "&#x00CC;" ;; <!-- LATIN CAPITAL LETTER I WITH GRAVE -->
	"iuml" #\ï ;; "&#x00EF;" ;; <!-- LATIN SMALL LETTER I WITH DIAERESIS -->
	"Iuml" #\Ï ;; "&#x00CF;" ;; <!-- LATIN CAPITAL LETTER I WITH DIAERESIS -->
	"ntilde" #\ñ ;; "&#x00F1;" ;; <!-- LATIN SMALL LETTER N WITH TILDE -->
	"Ntilde" #\Ñ ;; "&#x00D1;" ;; <!-- LATIN CAPITAL LETTER N WITH TILDE -->
	"oacute" #\ó ;; "&#x00F3;" ;; <!-- LATIN SMALL LETTER O WITH ACUTE -->
	"Oacute" #\Ó ;; "&#x00D3;" ;; <!-- LATIN CAPITAL LETTER O WITH ACUTE -->
	"ocirc" #\ô ;; "&#x00F4;" ;; <!-- LATIN SMALL LETTER O WITH CIRCUMFLEX -->
	"Ocirc" #\Ô ;; "&#x00D4;" ;; <!-- LATIN CAPITAL LETTER O WITH CIRCUMFLEX -->
	"ograve" #\ò ;; "&#x00F2;" ;; <!-- LATIN SMALL LETTER O WITH GRAVE -->
	"Ograve" #\Ò ;; "&#x00D2;" ;; <!-- LATIN CAPITAL LETTER O WITH GRAVE -->
	"oslash" #\ø ;; "&#x00F8;" ;; <!-- LATIN SMALL LETTER O WITH STROKE -->
	"Oslash" #\Ø ;; "&#x00D8;" ;; <!-- LATIN CAPITAL LETTER O WITH STROKE -->
	"otilde" #\õ ;; "&#x00F5;" ;; <!-- LATIN SMALL LETTER O WITH TILDE -->
	"Otilde" #\Õ ;; "&#x00D5;" ;; <!-- LATIN CAPITAL LETTER O WITH TILDE -->
	"ouml" #\ö ;; "&#x00F6;" ;; <!-- LATIN SMALL LETTER O WITH DIAERESIS -->
	"Ouml" #\Ö ;; "&#x00D6;" ;; <!-- LATIN CAPITAL LETTER O WITH DIAERESIS -->
	"szlig" #\ß ;; "&#x00DF;" ;; <!-- LATIN SMALL LETTER SHARP S -->
	"thorn" #\þ ;; "&#x00FE;" ;; <!-- LATIN SMALL LETTER THORN -->
	"THORN" #\Þ ;; "&#x00DE;" ;; <!-- LATIN CAPITAL LETTER THORN -->
	"uacute" #\ú ;; "&#x00FA;" ;; <!-- LATIN SMALL LETTER U WITH ACUTE -->
	"Uacute" #\Ú ;; "&#x00DA;" ;; <!-- LATIN CAPITAL LETTER U WITH ACUTE -->
	"ucirc" #\û ;; "&#x00FB;" ;; <!-- LATIN SMALL LETTER U WITH CIRCUMFLEX -->
	"Ucirc" #\Û ;; "&#x00DB;" ;; <!-- LATIN CAPITAL LETTER U WITH CIRCUMFLEX -->
	"ugrave" #\ù ;; "&#x00F9;" ;; <!-- LATIN SMALL LETTER U WITH GRAVE -->
	"Ugrave" #\Ù ;; "&#x00D9;" ;; <!-- LATIN CAPITAL LETTER U WITH GRAVE -->
	"uuml" #\ü ;; "&#x00FC;" ;; <!-- LATIN SMALL LETTER U WITH DIAERESIS -->
	"Uuml" #\Ü ;; "&#x00DC;" ;; <!-- LATIN CAPITAL LETTER U WITH DIAERESIS -->
	"yacute" #\ý ;; "&#x00FD;" ;; <!-- LATIN SMALL LETTER Y WITH ACUTE -->
	"Yacute" #\Ý ;; "&#x00DD;" ;; <!-- LATIN CAPITAL LETTER Y WITH ACUTE -->
	"yuml" #\ÿ ;; "&#x00FF;" ;; <!-- LATIN SMALL LETTER Y WITH DIAERESIS -->

	;; ;; iso lat 2

	;; abreve	#\x "&#x0103;" ;; <!-- LATIN SMALL LETTER A WITH BREVE -->
	;; Abreve	#\x "&#x0102;" ;; <!-- LATIN CAPITAL LETTER A WITH BREVE -->
	;; amacr	#\x "&#x0101;" ;; <!-- LATIN SMALL LETTER A WITH MACRON -->
	;; Amacr	#\x "&#x0100;" ;; <!-- LATIN CAPITAL LETTER A WITH MACRON -->
	;; aogon	#\x "&#x0105;" ;; <!-- LATIN SMALL LETTER A WITH OGONEK -->
	;; Aogon	#\x "&#x0104;" ;; <!-- LATIN CAPITAL LETTER A WITH OGONEK -->
	;; cacute	#\x "&#x0107;" ;; <!-- LATIN SMALL LETTER C WITH ACUTE -->
	;; Cacute	#\x "&#x0106;" ;; <!-- LATIN CAPITAL LETTER C WITH ACUTE -->
	;; ccaron	#\x "&#x010D;" ;; <!-- LATIN SMALL LETTER C WITH CARON -->
	;; Ccaron	#\x "&#x010C;" ;; <!-- LATIN CAPITAL LETTER C WITH CARON -->
	;; ccirc	#\x "&#x0109;" ;; <!-- LATIN SMALL LETTER C WITH CIRCUMFLEX -->
	;; Ccirc	#\x "&#x0108;" ;; <!-- LATIN CAPITAL LETTER C WITH CIRCUMFLEX -->
	;; cdot	#\x "&#x010B;" ;; <!-- DOT OPERATOR -->
	;; Cdot	#\x "&#x010A;" ;; <!-- LATIN CAPITAL LETTER C WITH DOT ABOVE -->
	;; dcaron	#\x "&#x010F;" ;; <!-- LATIN SMALL LETTER D WITH CARON -->
	;; Dcaron	#\x "&#x010E;" ;; <!-- LATIN CAPITAL LETTER D WITH CARON -->
	;; dstrok	#\x "&#x0111;" ;; <!-- LATIN SMALL LETTER D WITH STROKE -->
	;; Dstrok	#\x "&#x0110;" ;; <!-- LATIN CAPITAL LETTER D WITH STROKE -->
	;; ecaron	#\x "&#x011B;" ;; <!-- LATIN SMALL LETTER E WITH CARON -->
	;; Ecaron	#\x "&#x011A;" ;; <!-- LATIN CAPITAL LETTER E WITH CARON -->
	;; edot	#\x "&#x0117;" ;; <!-- LATIN SMALL LETTER E WITH DOT ABOVE -->
	;; Edot	#\x "&#x0116;" ;; <!-- LATIN CAPITAL LETTER E WITH DOT ABOVE -->
	;; emacr	#\x "&#x0113;" ;; <!-- LATIN SMALL LETTER E WITH MACRON -->
	;; Emacr	#\x "&#x0112;" ;; <!-- LATIN CAPITAL LETTER E WITH MACRON -->
	;; eogon	#\x "&#x0119;" ;; <!-- LATIN SMALL LETTER E WITH OGONEK -->
	;; Eogon	#\x "&#x0118;" ;; <!-- LATIN CAPITAL LETTER E WITH OGONEK -->
	;; gacute	#\x "&#x01F5;" ;; <!-- LATIN SMALL LETTER G WITH ACUTE -->
	;; gbreve	#\x "&#x011F;" ;; <!-- LATIN SMALL LETTER G WITH BREVE -->
	;; Gbreve	#\x "&#x011E;" ;; <!-- LATIN CAPITAL LETTER G WITH BREVE -->
	;; Gcedil	#\x "&#x0122;" ;; <!-- LATIN CAPITAL LETTER G WITH CEDILLA -->
	;; gcirc	#\x "&#x011D;" ;; <!-- LATIN SMALL LETTER G WITH CIRCUMFLEX -->
	;; Gcirc	#\x "&#x011C;" ;; <!-- LATIN CAPITAL LETTER G WITH CIRCUMFLEX -->
	;; gdot	#\x "&#x0121;" ;; <!-- LATIN SMALL LETTER G WITH DOT ABOVE -->
	;; Gdot	#\x "&#x0120;" ;; <!-- LATIN CAPITAL LETTER G WITH DOT ABOVE -->
	;; hcirc	#\x "&#x0125;" ;; <!-- LATIN SMALL LETTER H WITH CIRCUMFLEX -->
	;; Hcirc	#\x "&#x0124;" ;; <!-- LATIN CAPITAL LETTER H WITH CIRCUMFLEX -->
	;; hstrok	#\x "&#x0127;" ;; <!-- LATIN SMALL LETTER H WITH STROKE -->
	;; Hstrok	#\x "&#x0126;" ;; <!-- LATIN CAPITAL LETTER H WITH STROKE -->
	;; Idot	#\x "&#x0130;" ;; <!-- LATIN CAPITAL LETTER I WITH DOT ABOVE -->
	;; Imacr	#\x "&#x012A;" ;; <!-- LATIN CAPITAL LETTER I WITH MACRON -->
	;; imacr	#\x "&#x012B;" ;; <!-- LATIN SMALL LETTER I WITH MACRON -->
	;; ijlig	#\x "&#x0133;" ;; <!-- LATIN SMALL LIGATURE IJ -->
	;; IJlig	#\x "&#x0132;" ;; <!-- LATIN CAPITAL LIGATURE IJ -->
	;; inodot	#\x "&#x0131;" ;; <!-- LATIN SMALL LETTER DOTLESS I -->
	;; iogon	#\x "&#x012F;" ;; <!-- LATIN SMALL LETTER I WITH OGONEK -->
	;; Iogon	#\x "&#x012E;" ;; <!-- LATIN CAPITAL LETTER I WITH OGONEK -->
	;; itilde	#\x "&#x0129;" ;; <!-- LATIN SMALL LETTER I WITH TILDE -->
	;; Itilde	#\x "&#x0128;" ;; <!-- LATIN CAPITAL LETTER I WITH TILDE -->
	;; jcirc	#\x "&#x0135;" ;; <!-- LATIN SMALL LETTER J WITH CIRCUMFLEX -->
	;; Jcirc	#\x "&#x0134;" ;; <!-- LATIN CAPITAL LETTER J WITH CIRCUMFLEX -->
	;; kcedil	#\x "&#x0137;" ;; <!-- LATIN SMALL LETTER K WITH CEDILLA -->
	;; Kcedil	#\x "&#x0136;" ;; <!-- LATIN CAPITAL LETTER K WITH CEDILLA -->
	;; kgreen	#\x "&#x0138;" ;; <!-- LATIN SMALL LETTER KRA -->
	;; lacute	#\x "&#x013A;" ;; <!-- LATIN SMALL LETTER L WITH ACUTE -->
	;; Lacute	#\x "&#x0139;" ;; <!-- LATIN CAPITAL LETTER L WITH ACUTE -->
	;; lcaron	#\x "&#x013E;" ;; <!-- LATIN SMALL LETTER L WITH CARON -->
	;; Lcaron	#\x "&#x013D;" ;; <!-- LATIN CAPITAL LETTER L WITH CARON -->
	;; lcedil	#\x "&#x013C;" ;; <!-- LATIN SMALL LETTER L WITH CEDILLA -->
	;; Lcedil	#\x "&#x013B;" ;; <!-- LATIN CAPITAL LETTER L WITH CEDILLA -->
	;; lmidot	#\x "&#x0140;" ;; <!-- LATIN SMALL LETTER L WITH MIDDLE DOT -->
	;; Lmidot	#\x "&#x013F;" ;; <!-- LATIN CAPITAL LETTER L WITH MIDDLE DOT -->
	;; lstrok	#\x "&#x0142;" ;; <!-- LATIN SMALL LETTER L WITH STROKE -->
	;; Lstrok	#\x "&#x0141;" ;; <!-- LATIN CAPITAL LETTER L WITH STROKE -->
	;; nacute	#\x "&#x0144;" ;; <!-- LATIN SMALL LETTER N WITH ACUTE -->
	;; Nacute	#\x "&#x0143;" ;; <!-- LATIN CAPITAL LETTER N WITH ACUTE -->
	;; eng	#\x "&#x014B;" ;; <!-- LATIN SMALL LETTER ENG -->
	;; ENG	#\x "&#x014A;" ;; <!-- LATIN CAPITAL LETTER ENG -->
	;; napos	#\x "&#x0149;" ;; <!-- LATIN SMALL LETTER N PRECEDED BY APOSTROPHE -->
	;; ncaron	#\x "&#x0148;" ;; <!-- LATIN SMALL LETTER N WITH CARON -->
	;; Ncaron	#\x "&#x0147;" ;; <!-- LATIN CAPITAL LETTER N WITH CARON -->
	;; ncedil	#\x "&#x0146;" ;; <!-- LATIN SMALL LETTER N WITH CEDILLA -->
	;; Ncedil	#\x "&#x0145;" ;; <!-- LATIN CAPITAL LETTER N WITH CEDILLA -->
	;; odblac	#\x "&#x0151;" ;; <!-- LATIN SMALL LETTER O WITH DOUBLE ACUTE -->
	;; Odblac	#\x "&#x0150;" ;; <!-- LATIN CAPITAL LETTER O WITH DOUBLE ACUTE -->
	;; Omacr	#\x "&#x014C;" ;; <!-- LATIN CAPITAL LETTER O WITH MACRON -->
	;; omacr	#\x "&#x014D;" ;; <!-- LATIN SMALL LETTER O WITH MACRON -->
	;; oelig	#\x "&#x0153;" ;; <!-- LATIN SMALL LIGATURE OE -->
	;; OElig	#\x "&#x0152;" ;; <!-- LATIN CAPITAL LIGATURE OE -->
	;; racute	#\x "&#x0155;" ;; <!-- LATIN SMALL LETTER R WITH ACUTE -->
	;; Racute	#\x "&#x0154;" ;; <!-- LATIN CAPITAL LETTER R WITH ACUTE -->
	;; rcaron	#\x "&#x0159;" ;; <!-- LATIN SMALL LETTER R WITH CARON -->
	;; Rcaron	#\x "&#x0158;" ;; <!-- LATIN CAPITAL LETTER R WITH CARON -->
	;; rcedil	#\x "&#x0157;" ;; <!-- LATIN SMALL LETTER R WITH CEDILLA -->
	;; Rcedil	#\x "&#x0156;" ;; <!-- LATIN CAPITAL LETTER R WITH CEDILLA -->
	;; sacute	#\x "&#x015B;" ;; <!-- LATIN SMALL LETTER S WITH ACUTE -->
	;; Sacute	#\x "&#x015A;" ;; <!-- LATIN CAPITAL LETTER S WITH ACUTE -->
	;; scaron	#\x "&#x0161;" ;; <!-- LATIN SMALL LETTER S WITH CARON -->
	;; Scaron	#\x "&#x0160;" ;; <!-- LATIN CAPITAL LETTER S WITH CARON -->
	;; scedil	#\x "&#x015F;" ;; <!-- LATIN SMALL LETTER S WITH CEDILLA -->
	;; Scedil	#\x "&#x015E;" ;; <!-- LATIN CAPITAL LETTER S WITH CEDILLA -->
	;; scirc	#\x "&#x015D;" ;; <!-- LATIN SMALL LETTER S WITH CIRCUMFLEX -->
	;; Scirc	#\x "&#x015C;" ;; <!-- LATIN CAPITAL LETTER S WITH CIRCUMFLEX -->
	;; tcaron	#\x "&#x0165;" ;; <!-- LATIN SMALL LETTER T WITH CARON -->
	;; Tcaron	#\x "&#x0164;" ;; <!-- LATIN CAPITAL LETTER T WITH CARON -->
	;; tcedil	#\x "&#x0163;" ;; <!-- LATIN SMALL LETTER T WITH CEDILLA -->
	;; Tcedil	#\x "&#x0162;" ;; <!-- LATIN CAPITAL LETTER T WITH CEDILLA -->
	;; tstrok	#\x "&#x0167;" ;; <!-- LATIN SMALL LETTER T WITH STROKE -->
	;; Tstrok	#\x "&#x0166;" ;; <!-- LATIN CAPITAL LETTER T WITH STROKE -->
	;; ubreve	#\x "&#x016D;" ;; <!-- LATIN SMALL LETTER U WITH BREVE -->
	;; Ubreve	#\x "&#x016C;" ;; <!-- LATIN CAPITAL LETTER U WITH BREVE -->
	;; udblac	#\x "&#x0171;" ;; <!-- LATIN SMALL LETTER U WITH DOUBLE ACUTE -->
	;; Udblac	#\x "&#x0170;" ;; <!-- LATIN CAPITAL LETTER U WITH DOUBLE ACUTE -->
	;; umacr	#\x "&#x016B;" ;; <!-- LATIN SMALL LETTER U WITH MACRON -->
	;; Umacr	#\x "&#x016A;" ;; <!-- LATIN CAPITAL LETTER U WITH MACRON -->
	;; uogon	#\x "&#x0173;" ;; <!-- LATIN SMALL LETTER U WITH OGONEK -->
	;; Uogon	#\x "&#x0172;" ;; <!-- LATIN CAPITAL LETTER U WITH OGONEK -->
	;; uring	#\x "&#x016F;" ;; <!-- LATIN SMALL LETTER U WITH RING ABOVE -->
	;; Uring	#\x "&#x016E;" ;; <!-- LATIN CAPITAL LETTER U WITH RING ABOVE -->
	;; utilde	#\x "&#x0169;" ;; <!-- LATIN SMALL LETTER U WITH TILDE -->
	;; Utilde	#\x "&#x0168;" ;; <!-- LATIN CAPITAL LETTER U WITH TILDE -->
	;; wcirc	#\x "&#x0175;" ;; <!-- LATIN SMALL LETTER W WITH CIRCUMFLEX -->
	;; Wcirc	#\x "&#x0174;" ;; <!-- LATIN CAPITAL LETTER W WITH CIRCUMFLEX -->
	;; ycirc	#\x "&#x0177;" ;; <!-- LATIN SMALL LETTER Y WITH CIRCUMFLEX -->
	;; Ycirc	#\x "&#x0176;" ;; <!-- LATIN CAPITAL LETTER Y WITH CIRCUMFLEX -->
	;; Yuml	#\x "&#x0178;" ;; <!-- LATIN CAPITAL LETTER Y WITH DIAERESIS -->
	;; zacute	#\x "&#x017A;" ;; <!-- LATIN SMALL LETTER Z WITH ACUTE -->
	;; Zacute	#\x "&#x0179;" ;; <!-- LATIN CAPITAL LETTER Z WITH ACUTE -->
	;; zcaron	#\x "&#x017E;" ;; <!-- LATIN SMALL LETTER Z WITH CARON -->
	;; Zcaron	#\x "&#x017D;" ;; <!-- LATIN CAPITAL LETTER Z WITH CARON -->
	;; zdot	#\x "&#x017C;" ;; <!-- LATIN SMALL LETTER Z WITH DOT ABOVE -->
	;; Zdot	#\x "&#x017B;" ;; <!-- LATIN CAPITAL LETTER Z WITH DOT ABOVE -->

	;; ;; iso-num

	;; half	#\x "&#x00BD;" ;; <!-- VULGAR FRACTION ONE HALF -->
	;; frac12	#\x "&#x00BD;" ;; <!-- VULGAR FRACTION ONE HALF -->
	;; frac14	#\x "&#x00BC;" ;; <!-- VULGAR FRACTION ONE QUARTER -->
	;; frac34	#\x "&#x00BE;" ;; <!-- VULGAR FRACTION THREE QUARTERS -->
	;; frac18	#\x "&#x215B;" ;; <!--  -->
	;; frac38	#\x "&#x215C;" ;; <!--  -->
	;; frac58	#\x "&#x215D;" ;; <!--  -->
	;; frac78	#\x "&#x215E;" ;; <!--  -->
	;; sup1	#\x "&#x00B9;" ;; <!-- SUPERSCRIPT ONE -->
	;; sup2	#\x "&#x00B2;" ;; <!-- SUPERSCRIPT TWO -->
	;; sup3	#\x "&#x00B3;" ;; <!-- SUPERSCRIPT THREE -->
	;; plus	#\x "&#x002B;" ;; <!-- PLUS SIGN -->
	;; plusmn	#\x "&#x00B1;" ;; <!-- PLUS-MINUS SIGN -->
	"lt" #\< ;; "&#38;#60;" ;; <!-- LESS-THAN SIGN -->
	"equals" #\= ;; "&#x003D;" ;; <!-- EQUALS SIGN -->
	"gt" #\> ;; "&#x003E;" ;; <!-- GREATER-THAN SIGN -->
	"divide" #\/ ;; "&#x00F7;" ;; <!-- DIVISION SIGN -->
	;; times	#\x "&#x00D7;" ;; <!-- MULTIPLICATION SIGN -->
	;; curren	#\x "&#x00A4;" ;; <!-- CURRENCY SIGN -->
	;; pound	#\x "&#x00A3;" ;; <!-- POUND SIGN -->
	;; dollar	#\x "&#x0024;" ;; <!-- DOLLAR SIGN -->
	;; cent	#\x "&#x00A2;" ;; <!-- CENT SIGN -->
	;; yen	#\x "&#x00A5;" ;; <!-- YEN SIGN -->
	;; num	#\x "&#x0023;" ;; <!-- NUMBER SIGN -->
	;; percnt	#\x "&#x0025;" ;; <!-- PERCENT SIGN -->
	 "amp" #\& ;; "&#38;#38;" ;; <!-- AMPERSAND -->
	;; ast	#\x "&#x002A;" ;; <!-- ASTERISK OPERATOR -->
	;; commat	#\x "&#x0040;" ;; <!-- COMMERCIAL AT -->
	;; lsqb	#\x "&#x005B;" ;; <!-- LEFT SQUARE BRACKET -->
	;; bsol	#\x "&#x005C;" ;; <!-- REVERSE SOLIDUS -->
	;; rsqb	#\x "&#x005D;" ;; <!-- RIGHT SQUARE BRACKET -->
	;; lcub	#\x "&#x007B;" ;; <!-- LEFT CURLY BRACKET -->
	;; horbar	#\x "&#x2015;" ;; <!-- HORIZONTAL BAR -->
	;; verbar	#\x "&#x007C;" ;; <!-- VERTICAL LINE -->
	;; rcub	#\x "&#x007D;" ;; <!-- RIGHT CURLY BRACKET -->
	;; micro	#\x "&#x00B5;" ;; <!-- MICRO SIGN -->
	;; ohm	#\x "&#x2126;" ;; <!-- OHM SIGN -->
	;; deg	#\x "&#x00B0;" ;; <!-- DEGREE SIGN -->
	;; ordm	#\x "&#x00BA;" ;; <!-- MASCULINE ORDINAL INDICATOR -->
	;; ordf	#\x "&#x00AA;" ;; <!-- FEMININE ORDINAL INDICATOR -->
	;; sect	#\x "&#x00A7;" ;; <!-- SECTION SIGN -->
	"para"	#\§ ;; "&#x00B6;" ;; <!-- PILCROW SIGN -->
	;; middot	#\x "&#x00B7;" ;; <!-- MIDDLE DOT -->
	;; larr	#\x "&#x2190;" ;; <!-- LEFTWARDS DOUBLE ARROW -->
	;; rarr	#\x "&#x2192;" ;; <!-- RIGHTWARDS DOUBLE ARROW -->
	;; uarr	#\x "&#x2191;" ;; <!-- UPWARDS ARROW -->
	;; darr	#\x "&#x2193;" ;; <!-- DOWNWARDS ARROW -->
	;; copy	#\x "&#x00A9;" ;; <!-- COPYRIGHT SIGN -->
	;; reg	#\x "&#x00AE;" ;; <!-- REG TRADE MARK SIGN -->
	;; trade	#\x "&#x2122;" ;; <!-- TRADE MARK SIGN -->
	;; brvbar	#\x "&#x00A6;" ;; <!-- BROKEN BAR -->
	;; not	#\x "&#x00AC;" ;; <!-- NOT SIGN -->
	;; sung	#\x "&#x2669;" ;; <!--  -->
	;; excl	#\x "&#x0021;" ;; <!-- EXCLAMATION MARK -->
	;; iexcl	#\x "&#x00A1;" ;; <!-- INVERTED EXCLAMATION MARK -->
	"quot"	#\" ;; "&#x0022;" ;; <!-- QUOTATION MARK -->
	;; apos	#\x "&#x0027;" ;; <!-- APOSTROPHE -->
	;; lpar	#\x "&#x0028;" ;; <!-- LEFT PARENTHESIS -->
	;; rpar	#\x "&#x0029;" ;; <!-- RIGHT PARENTHESIS -->
	;; comma	#\x "&#x002C;" ;; <!-- COMMA -->
	;; lowbar	#\x "&#x005F;" ;; <!-- LOW LINE -->
	;; hyphen	#\x "&#x002D;" ;; <!-- HYPHEN-MINUS -->
	;; period	#\x "&#x002E;" ;; <!-- FULL STOP -->
	;; sol	#\x "&#x002F;" ;; <!-- SOLIDUS -->
	;; colon	#\x "&#x003A;" ;; <!-- COLON -->
	;; semi	#\x "&#x003B;" ;; <!-- SEMICOLON -->
	;; quest	#\x "&#x003F;" ;; <!-- QUESTION MARK -->
	;; iquest	#\x "&#x00BF;" ;; <!-- INVERTED QUESTION MARK -->
	;; laquo	#\x "&#x00AB;" ;; <!-- LEFT-POINTING DOUBLE ANGLE QUOTATION MARK -->
	;; raquo	#\x "&#x00BB;" ;; <!-- RIGHT-POINTING DOUBLE ANGLE QUOTATION MARK -->
	;; lsquo	#\x "&#39;" #\x "&#x2018;" ;; <!--  -->
	;; rsquo	#\x "&#39;" #\x "&#x2019;" ;; <!-- RIGHT SINGLE QUOTATION MARK -->
	;; ldquo	#\x "&#34;" #\x "&#x201C;" ;; <!--  -->
	;; rdquo	#\x "&#34;" #\x "&#x201D;" ;; <!-- RIGHT DOUBLE QUOTATION MARK -->
        "nbsp"	#-allegro #.(code-char #xa0) #+allegro #\no-break_space ;; "&#x00A0;" ;; <!-- NO-BREAK SPACE -->
	;; shy	#\x "&#x00AD;" ;; <!-- SOFT HYPHEN -->

	;; ;; iso-pub

	;; emsp	#\x "&#x2003;" ;; <!-- EM SPACE -->
	;; ensp	#\x "&#x2002;" ;; <!-- EN SPACE -->
	;; emsp13	#\x "&#x2004;" ;; <!-- THREE-PER-EM SPACE -->
	;; emsp14	#\x "&#x2005;" ;; <!-- FOUR-PER-EM SPACE -->
	;; numsp	#\x "&#x2007;" ;; <!-- FIGURE SPACE -->
	;; puncsp	#\x "&#x2008;" ;; <!-- PUNCTUATION SPACE -->
	;; thinsp	#\x "&#x2009;" ;; <!-- THIN SPACE -->
	;; hairsp	#\x "&#x200A;" ;; <!-- HAIR SPACE -->
	"mdash"	#\- ;; prelim *** "&#45;&#45;" #\x "&#x2014;" ;; <!-- EM DASH -->
	"ndash"	#\- ;; "&#45;&#45;" #\x "&#x2013;" ;; <!-- EN DASH -->
	;; dash	#\x "&#45;#45;" #\x "&#x2010;" ;; <!-- HYPHEN -->
	;; blank	#\x "&#x2423;" ;; <!-- OPEN BOX -->
	;; hellip	#\x "&#x2026;" ;; <!-- HORIZONTAL ELLIPSIS -->
	;; nldr	#\x "&#x2025;" ;; <!-- TWO DOT LEADER -->
	;; frac13	#\x "&#x2153;" ;; <!-- VULGAR FRACTION ONE THIRD -->
	;; frac23	#\x "&#x2154;" ;; <!-- VULGAR FRACTION TWO THIRDS -->
	;; frac15	#\x "&#x2155;" ;; <!-- VULGAR FRACTION ONE FIFTH -->
	;; frac25	#\x "&#x2156;" ;; <!-- VULGAR FRACTION TWO FIFTHS -->
	;; frac35	#\x "&#x2157;" ;; <!-- VULGAR FRACTION THREE FIFTHS -->
	;; frac45	#\x "&#x2158;" ;; <!-- VULGAR FRACTION FOUR FIFTHS -->
	;; frac16	#\x "&#x2159;" ;; <!-- VULGAR FRACTION ONE SIXTH -->
	;; frac56	#\x "&#x215A;" ;; <!-- VULGAR FRACTION FIVE SIXTHS -->
	;; incare	#\x "&#x2105;" ;; <!-- CARE OF -->
	;; block	#\x "&#x2588;" ;; <!-- FULL BLOCK -->
	;; uhblk	#\x "&#x2580;" ;; <!-- UPPER HALF BLOCK -->
	;; lhblk	#\x "&#x2584;" ;; <!-- LOWER HALF BLOCK -->
	;; blk14	#\x "&#x2591;" ;; <!-- LIGHT SHADE -->
	;; blk12	#\x "&#x2592;" ;; <!-- MEDIUM SHADE -->
	;; blk34	#\x "&#x2593;" ;; <!-- DARK SHADE -->
	;; marker	#\x "&#x25AE;" ;; <!-- BLACK VERTICAL RECTANGLE -->
	;; cir	#\x "&#x25CB;" ;; <!-- WHITE CIRCLE -->
	;; squ	#\x "&#x25A1;" ;; <!-- WHITE SQUARE -->
	;; rect	#\x "&#x25AD;" ;; <!-- WHITE RECTANGLE -->
	;; utri	#\x "&#x25B5;" ;; <!-- WHITE UP-POINTING TRIANGLE -->
	;; dtri	#\x "&#x25BF;" ;; <!-- WHITE DOWN-POINTING TRIANGLE -->
	;; star	#\x "&#x22C6;" ;; <!-- STAR OPERATOR -->
	;; bull	#\x "&#x2022;" ;; <!-- BULLET -->
	;; squf	#\x "&#x25AA;" ;; <!--  -->
	;; utrif	#\x "&#x25B4;" ;; <!-- BLACK UP-POINTING TRIANGLE -->
	;; dtrif	#\x "&#x25BE;" ;; <!-- BLACK DOWN-POINTING TRIANGLE -->
	;; ltrif	#\x "&#x25C2;" ;; <!-- BLACK LEFT-POINTING TRIANGLE -->
	;; rtrif	#\x "&#x25B8;" ;; <!-- BLACK RIGHT-POINTING TRIANGLE -->
	;; clubs	#\x "&#x2663;" ;; <!-- BLACK CLUB SUIT -->
	;; diams	#\x "&#x2666;" ;; <!-- BLACK DIAMOND SUIT -->
	;; hearts	#\x "&#x2665;" ;; <!-- BLACK HEART SUIT -->
	;; spades	#\x "&#x2660;" ;; <!-- BLACK SPADE SUIT -->
	;; malt	#\x "&#x2720;" ;; <!-- MALTESE CROSS -->
	;; dagger	#\x "&#x2020;" ;; <!-- DAGGER -->
	;; Dagger	#\x "&#x2021;" ;; <!-- DOUBLE DAGGER -->
	;; check	#\x "&#x2713;" ;; <!-- CHECK MARK -->
	;; cross	#\x "&#x2717;" ;; <!-- BALLOT X -->
	;; sharp	#\x "&#x266F;" ;; <!-- MUSIC SHARP SIGN -->
	;; flat	#\x "&#x266D;" ;; <!-- MUSIC FLAT SIGN -->
	;; male	#\x "&#x2642;" ;; <!-- MALE SIGN -->
	;; female	#\x "&#x2640;" ;; <!--  -->
	;; phone	#\x "&#x260E;" ;; <!-- TELEPHONE SIGN -->
	;; telrec	#\x "&#x2315;" ;; <!-- TELEPHONE RECORDER -->
	;; copysr	#\x "&#x2117;" ;; <!-- SOUND RECORDING COPYRIGHT -->
	;; caret	#\x "&#x2041;" ;; <!-- CARET -->
	;; lsquor	#\x "&#x201A;" ;; <!-- SINGLE LOW-9 QUOTATION MARK -->
	;; ldquor	#\x "&#x201E;" ;; <!-- DOUBLE LOW-9 QUOTATION MARK -->
	;; fflig	#\x "&#xFB00;" ;; <!--  -->
	;; filig	#\x "&#xFB01;" ;; <!--  -->
	;; ;; <!--     fjlig	Unknown unicode character -->
	;; ffilig	#\x "&#xFB03;" ;; <!--  -->
	;; ffllig	#\x "&#xFB04;" ;; <!--  -->
	;; fllig	#\x "&#xFB02;" ;; <!--  -->
	;; mldr	#\x "&#x2026;" ;; <!-- HORIZONTAL ELLIPSIS -->
	;; rdquor	#\x "&#x201C;" ;; <!--  -->
	;; rsquor	#\x "&#x2018;" ;; <!--  -->
	;; vellip	#\x "&#x22EE;" ;; <!--  -->
	;; hybull	#\x "&#x2043;" ;; <!-- HYPHEN BULLET -->
	;; loz	#\x "&#x25CA;" ;; <!-- LOZENGE -->
	;; lozf	#\x "&#x2726;" ;; <!--  -->
	;; ltri	#\x "&#x25C3;" ;; <!-- WHITE LEFT-POINTING TRIANGLE -->
	;; rtri	#\x "&#x25B9;" ;; <!-- WHITE RIGHT-POINTING TRIANGLE -->
	;; starf	#\x "&#x2605;" ;; <!-- BLACK STAR -->
	;; natur	#\x "&#x266E;" ;; <!-- MUSIC NATURAL SIGN -->
	;; rx	#\x "&#x211E;" ;; <!-- PRESCRIPTION TAKE -->
	;; sext	#\x "&#x2736;" ;; <!-- SIX POINTED BLACK STAR -->
	;; target	#\x "&#x2316;" ;; <!-- POSITION INDICATOR -->
	;; dlcrop	#\x "&#x230D;" ;; <!-- BOTTOM LEFT CROP -->
	;; drcrop	#\x "&#x230C;" ;; <!-- BOTTOM RIGHT CROP -->
	;; ulcrop	#\x "&#x230F;" ;; <!-- TOP LEFT CROP -->
	;; urcrop	#\x "&#x230E;" ;; <!-- TOP RIGHT CROP -->
	)
    by #'cddr
    do (setf (gethash entity *entity-to-char-table*) char))



