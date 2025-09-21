;; -*- Mode: lisp; Syntax: ansi-common-lisp; Package: ENCODING; Base: 10 -*-
;;
;; Copyright (c) 2015, Paul Meurer, University of Bergen
;; https://clarino.uib.no
;; All rights reserved.
;; 

(in-package :encoding)

;; "აბგდევზთიკლმნოპჟრსტუფქღყშჩცძწჭხჯჰჱჲჳჴჵჶჷჸჹჺ჻"

;; scientific transliteration
(defparameter +georgian-transliterate+
  #("a" "b" "g" "d" "e" "v" "z" "t" "i" "ḳ" "l" "m" "n" "o" "ṗ" "ž" "r" "s" "ṭ" "u" "p" "k" "ġ" "q̇" "š" "č" "c" "ʒ" "c̣" "č̣" "x" "ǯ" "h" "ē" "y" "w" "q" "ō" "f" "ǝ" "ჸ" "ჹ" "ჺ" "჻"))

;; national transliteration (2002)
(defparameter +georgian-transliterate-national+
  #("a" "b" "g" "d" "e" "v" "z" "t" "i" "k’" "l" "m" "n" "o" "p’" "zh" "r" "s" "t’" "u" "p" "k" "gh" "q’" "sh" "ch" "ts" "dz" "ts’" "ch’" "kh" "j" "h"
    ;; not used
    "ē""y""w""q""ō""f""ǝ""ჸ""ჹ""ჺ""჻"))

;; not sorted!
(defparameter +abkhaz-unicode+
  "АаБбВвГгҔҕӶӷДдЏџЕеҼҽҾҿЖжЗзӠӡИиКкҚқҞҟЛлМмНнОоҨҩПпҦҧԤԥРрСсТтҬҭУуФфХхҲҳЦцҴҵЧчҶҷШшЫыЬьӘәЙйЮюЯяЭэЩщ")

#+test
(loop with uc = (split "а б в г гь гә ҕ ҕь ҕә д дә е ж жь жә з ӡ ӡә и к кь кә қ қь қә ҟ ҟь ҟә л м н о п ҧ р с т тә ҭ ҭә у ф х хь хә ҳ ҳә ц цә ҵ ҵә ч ҷ ҽ ҿ ш шь шә ы ҩ џ џь ь ъ й ю я э щ" #\space)
   with tr = (split "a b v g g’ gʷ γ γ’ γʷ d dʷ e ž ž’ žʷ z ʒ ʒʷ i ḳ ḳ’ ḳʷ k k’ kʷ q̇ q̇’ q̇ʷ l m n o ṗ p r s ṭ ṭʷ t tʷ u f x x’ xʷ ḥ ḥʷ c cʷ c̣ c̣ʷ ć ć̣ č č̣ š š’ šʷ ə ʿʷ ǯ ǯ’ ’ ' j ju ja e šć" #\space)
for i below 23
do (format t "|=.~a|=.~a||=.~a|=.~a||=.~a|=.~a|~%"
	   (nth i uc)
	   (nth i tr)
	   (nth (+ i 23) uc)
	   (nth (+ i 23) tr)
	   (nth (+ i 46) uc)
	   (nth (+ i 46) tr)))
	      
(defparameter +abkhaz-transliterate+
  #("A""a""B""b""V""v"
    "G""g""Γ""γ""Γ""γ"
    "D""d""Ǯ""ǯ""E""e""Č""č""Č̣""č̣""Ž""ž""Z""z""Ʒ""ʒ"
    "I""i""Ḳ""ḳ""K""k""Q̇""q̇""L""l""M""m""N""n""O""o" "Y°" "y°";; "Yʷ" "yʷ" ;;"Ŵ" "ʿʷ" ;; "ŵ"
    "Ṗ""ṗ""P""p""P""p"
    "R""r""S""s""Ṭ""ṭ""T""t""U""u""F""f""X""x""Ḥ""ḥ"
    "C""c""C̣""c̣""Ć""ć""Ć̣""ć̣""Š""š""Ə""ə""’""’" "W" ;; "ʷ"
     "°" ;; "ʷ";;  ;; "w"
    "J" "j" "Ju" "ju" "Ja" "ja" "E" "e" "Šć" "šć"
    ))


;; "АаБбВвГгҔҕӶӷДдЏџЕеҼҽҾ̣ҿ̣ЖжЗзӠӡИиКкҚқҞҟЛлМмНнОоҨҩПпҦҧԤԥРрСсТтҬҭУуФфХхҲҳЦцҴҵЧчҶҷШшЫыЬьӘә"
;; "AaBbVvGgΓγΓγDdǮǯEeČčČ̣č̣ŽžZzƷʒIiḲḳKkQ̇q̇LlMmNnOoŴŵṖṗPpPpRrSsṬṭTtUuFfXxḤḥC̣c̣ĊċĆćĆ̣ć̣ŠšƏə’’Wẃ"


;; (loop for c across +georgian-transliterate+ do (print c))

#+test
(print (coerce (loop for i below 44
		  collect (code-char (+ (char-code #\ა) i)))
	       'string))

(defparameter *transliteration-standard* :scientific)

(defparameter *abk-cyr-to-mxedr-table*
  (dat:make-string-tree))

(loop for (cyr mxedr) on 
      '("а" "ა"
        "б" "ბ"
        "в" "ვ"
        "г" "გ"
        "гь" "გჲ"
        "гә" "გუ"
        "гәы" "გუ"
        "ҕ" "ღ"
        "ҕь" "ღჲ"
        "ҕә" "ღუ"
        "ҕәы" "ღუ"
        "д" "დ"
        "дә" "დჿ"
        "е" "ე"
        "ж" "ჟჾ"
        "жь" "ჟ"
        "жә" "ჟჿ"
        "з" "ზ"
        "ӡ" "ძ"
        "ӡә" "ძჿ"
        "и" "ი"
        "к" "კ"
        "кь" "კჲ"
        "кә" "კუ"
        "кәы" "კუ"
        "қ" "ქ"
        "қь" "ქჲ"
        "қә" "ქუ"
        "қәы" "ქუ"
        "ҟ" "ყ"
        "ҟь" "ყჲ"
        "ҟә" "ყუ"
        "ҟәы" "ყუ"
        "л" "ლ"
        "м" "მ"
        "н" "ნ"
        "о" "ო"
        "п" "პ"
        "ҧ" "ფ"
        "р" "რ"
        "с" "ს"
        "т" "ტ"
        "тә" "ტჿ"
        "ҭ" "თ"
        "ҭә" "თჿ"
        "у" "უ"
        "ф" "ჶ"
        "х" "ხ"
        "хь" "ხჲ"
        "хә" "ხუ"
        "хәы" "ხუ"
        "ҳ" "ჰ"
        "ҳә" "ჰჿ"
        "ц" "ც"
        "цә" "ცჿ"
        "ҵ" "წ"
        "ҵә" "წჿ"
        "ч" "ჩ"
        "ҷ" "ჭ"
        "ҽ" "ჩჾ"
        "ҿ" "ჭჾ"
        "ш" "შჾ"
        "шь" "შ"
        "шә" "შჿ"
        "ы" "ჷ"
        "ҩ" "ჳ"
        "џ" "ჯჾ"
        "џь" "ჯ"
        "ь" "ჲ")
      by #'cddr
      do (setf (dat:string-tree-get *abk-cyr-to-mxedr-table* cyr) mxedr))


;; romanized transliteration of Tigrinya

(defparameter *tir-to-rom-table*
  (dat:make-string-tree))

(loop for (tir rom) on 
     '("ሀ" "he" "ሁ" "hu" "ሂ" "hi" "ሃ" "ha" "ሄ" "hE" "ህ" "hI" "ሆ" "ho" "ለ" "le" "ሉ" "lu" "ሊ" "li" "ላ" "la"
       "ሌ" "lE" "ል" "lI" "ሎ" "lo" "ሐ" "He" "ሑ" "Hu" "ሒ" "Hi" "ሓ" "Ha" "ሔ" "HE" "ሕ" "HI" "ሖ" "Ho" "መ" "me"
       "ሙ" "mu" "ሚ" "mi" "ማ" "ma" "ሜ" "mE" "ም" "mI" "ሞ" "mo" "ሠ" "Se" "ሡ" "Su" "ሢ" "Si" "ሣ" "Sa" "ሤ" "SE"
       "ሥ" "SI" "ሦ" "So" "ረ" "re" "ሩ" "ru" "ሪ" "ri" "ራ" "ra" "ሬ" "rE" "ር" "rI" "ሮ" "ro" "ሰ" "se" "ሱ" "su"
       "ሲ" "si" "ሳ" "sa" "ሴ" "sE" "ስ" "sI" "ሶ" "so" "ሸ" "she" "ሹ" "shu" "ሺ" "shi" "ሻ" "sha" "ሼ" "shE" "ሽ"
       "shI" "ሾ" "sho" "ቀ" "qe" "ቁ" "qu" "ቂ" "qi" "ቃ" "qa" "ቄ" "qE" "ቅ" "qI" "ቆ" "qo" "ቐ" "Qe" "ቑ" "Qu"
       "ቒ" "Qi" "ቓ" "Qa" "ቔ" "QE" "ቕ" "QI" "ቖ" "Qo" "በ" "be" "ቡ" "bu" "ቢ" "bi" "ባ" "ba" "ቤ" "bE" "ብ" "bI"
       "ቦ" "bo" "ቨ" "ve" "ቩ" "vu" "ቪ" "vi" "ቫ" "va" "ቬ" "vE" "ቭ" "vI" "ቮ" "vo" "ተ" "te" "ቱ" "tu" "ቲ" "ti"
       "ታ" "ta" "ቴ" "tE" "ት" "tI" "ቶ" "to" "ቸ" "ce" "ቹ" "cu" "ቺ" "ci" "ቻ" "ca" "ቼ" "cE" "ች" "cI" "ቾ" "co"
       "ኀ" "Hhe" "ኁ" "Hhu" "ኂ" "Hhi" "ኃ" "Hha" "ኄ" "HhE" "ኅ" "HhI" "ኆ" "Hho" "ነ" "ne" "ኑ" "nu" "ኒ" "ni"
       "ና" "na" "ኔ" "nE" "ን" "nI" "ኖ" "no" "ኘ" "Ne" "ኙ" "Nu" "ኚ" "Ni" "ኛ" "Na" "ኜ" "NE" "ኝ" "NI" "ኞ" "No"
       "አ" "e" "ኡ" "u" "ኢ" "i" "ኣ" "a" "ኤ" "E" "እ" "I" "ኦ" "o" "ከ" "ke" "ኩ" "ku" "ኪ" "ki" "ካ" "ka" "ኬ" "kE"
       "ክ" "kI" "ኮ" "ko" "ኸ" "Ke" "ኹ" "Ku" "ኺ" "Ki" "ኻ" "Ka" "ኼ" "KE" "ኽ" "KI" "ኾ" "Ko" "ወ" "we" "ዉ" "wu"
       "ዊ" "wi" "ዋ" "wa" "ዌ" "wE" "ው" "wI" "ዎ" "wo" "ዐ" "Oe" "ዑ" "Ou" "ዒ" "Oi" "ዓ" "Oa" "ዔ" "OE" "ዕ" "OI"
       "ዖ" "Oo" "ዘ" "ze" "ዙ" "zu" "ዚ" "zi" "ዛ" "za" "ዜ" "zE" "ዝ" "zI" "ዞ" "zo" "ዠ" "Ze" "ዡ" "Zu" "ዢ" "Zi"
       "ዣ" "Za" "ዤ" "ZE" "ዥ" "ZI" "ዦ" "Zo" "የ" "ye" "ዩ" "yu" "ዪ" "yi" "ያ" "ya" "ዬ" "yE" "ይ" "yI" "ዮ" "yo"
       "ደ" "de" "ዱ" "du" "ዲ" "di" "ዳ" "da" "ዴ" "dE" "ድ" "dI" "ዶ" "do" "ጀ" "je" "ጁ" "ju" "ጂ" "ji" "ጃ" "ja"
       "ጄ" "jE" "ጅ" "jI" "ጆ" "jo" "ገ" "ge" "ጉ" "gu" "ጊ" "gi" "ጋ" "ga" "ጌ" "gE" "ግ" "gI" "ጎ" "go" "ጠ" "Te"
       "ጡ" "Tu" "ጢ" "Ti" "ጣ" "Ta" "ጤ" "TE" "ጥ" "TI" "ጦ" "To" "ጨ" "Ce" "ጩ" "Cu" "ጪ" "Ci" "ጫ" "Ca" "ጬ" "CE"
       "ጭ" "CI" "ጮ" "Co" "ጰ" "Pe" "ጱ" "Pu" "ጲ" "Pi" "ጳ" "Pa" "ጴ" "PE" "ጵ" "PI" "ጶ" "Po" "ጸ" "xe" "ጹ" "xu"
       "ጺ" "xi" "ጻ" "xa" "ጼ" "xE" "ጽ" "xI" "ጾ" "xo" "ፀ" "Xe" "ፁ" "Xu" "ፂ" "Xi" "ፃ" "Xa" "ፄ" "XE" "ፅ" "XI"
       "ፆ" "Xo" "ፈ" "fe" "ፉ" "fu" "ፊ" "fi" "ፋ" "fa" "ፌ" "fE" "ፍ" "fI" "ፎ" "fo" "ፐ" "pe" "ፑ" "pu" "ፒ" "pi"
       "ፓ" "pa" "ፔ" "pE" "ፕ" "pI" "ፖ" "po" "ቈ" "qWe" "ቊ" "qWi" "ቋ" "qWa" "ቌ" "qWE" "ቍ" "qW" "ቘ" "QWe"
       "ቚ" "QWi" "ቛ" "QWa" "ቜ" "QWE" "ቝ" "QW" "ኈ" "HhWe" "ኊ" "HhWi" "ኋ" "HhWa" "ኌ" "HhWE" "ኍ" "HhW"
       "ኰ" "kWe" "ኲ" "kWi" "ኳ" "kWa" "ኴ" "kWE" "ኵ" "kW" "ዀ" "KWe" "ዂ" "KWi" "ዃ" "KWa" "ዄ" "KWE" "ዅ" "KW"
       "ጐ" "gWe" "ጒ" "gWi" "ጓ" "gWa" "ጔ" "gWE" "ጕ" "gW" "ሏ" "lWa" "ሗ" "HWa" "ሟ" "mWa" "ሧ" "SWa" "ሯ" "rWa"
       "ሷ" "sWa" "ሿ" "shWa" "ቧ" "bWa" "ቯ" "vWa" "ቷ" "tWa" "ቿ" "cWa" "ኗ" "nWa" "ኟ" "Nwa" "ኧ" "Wa" "ዟ" "zWa"
       "ዧ" "ZWa" "ዷ" "dWa" "ጇ" "jWa" "ጧ" "TWa" "ጯ" "CWa" "ጷ" "PWa" "ጿ" "SWa" "ፏ" "fWa" "ፗ" "pWa" "ዸ" "De"
       "ዹ" "Du" "ዺ" "Di" "ዻ" "Da" "ዼ" "DE" "ዽ" "DI" "ዾ" "Do" "ጘ" "Ge" "ጙ" "Gu" "ጚ" "Gi" "ጛ" "Ga" "ጜ" "GE"
       "ጝ" "GI" "ጞ" "Go" "ፘ" "rYa" "ፙ" "mYa" "ፚ" "fYa" "።" "." "፡" ":" "፧" "?")
   by #'cddr
   do (setf (dat:string-tree-get *tir-to-rom-table* tir) rom))

(defun transliterate (string &key (standard *transliteration-standard*))
  (when string
    (with-output-to-string (stream)
      (case standard
	((:scientific :national)
	 (loop for c across string
	    for i from 0
	    if (char<= #\ა c #\ჺ)
	    do (write-string
		(aref (ecase standard
			(:scientific +georgian-transliterate+)
			(:national +georgian-transliterate-national+))
		      (- (char-code c) (char-code #\ა)))
                stream)
	    else do (write-char c stream)))
	(:abkhaz
	 (loop for c across string
	    for i from 0
	    for pos = (position c +abkhaz-unicode+)
	    do (if pos
		   (write-string (aref +abkhaz-transliterate+ pos) stream)
		   (write-char c stream))))
        (:abkhaz-mxedr
         (let ((dstring (string-downcase string)))
	   (loop with i = 0
                 while (< i (length string))
	         do (let ((mxedr (and (< i (- (length string) 2))
                                      (dat:string-tree-get *abk-cyr-to-mxedr-table*
                                                           dstring nil
                                                           i (+ i 3)))))
                      (cond (mxedr
                             (incf i 3)
                             (write-string mxedr stream))
                            (t
                             (let ((mxedr (and (< i (- (length string) 1))
                                               (dat:string-tree-get *abk-cyr-to-mxedr-table*
                                                                    dstring nil
                                                                    i (+ i 2)))))
                               (cond (mxedr
                                      (incf i 2)
                                      (write-string mxedr stream))
                                     (t
                                      (write-string (or (dat:string-tree-get *abk-cyr-to-mxedr-table*
                                                                             dstring nil
                                                                             i (+ i 1))
                                                        (subseq string i (1+ i)))
                                                    stream)
                                      (incf i))))))))))
        (:tir-rom
         (loop for c across string
            for s = (string c)
	    do (write-string (dat:string-tree-get *tir-to-rom-table* s s) stream)))
        (t
         (write-string string stream))))))

#+test
(write-line (transliterate "እቶም ዝኾነ ሰብ ከለልዮም ዝኽእሉ ምልክታት እዞም ዝስዕቡ ኮይኖም ፡" :standard :tir-rom))

#+test
(print (transliterate "Афольклор атрадициатә жанрқәа, дара зхылҵыз аҭоурыхтә ҭагылазаашьа ҳаназааигәаха, рыҽдырҿыцит. Избан акәзар уажәтәи аибашьраантәи ахҭысқәеи акрызхыҵхьаз аҭоурыхтә хҭысқәеи иааџьоушьартә еиҧш еишьашәалахеит." :standard :abkhaz-mxedr))

:eof
