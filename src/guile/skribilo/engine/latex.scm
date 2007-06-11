;;; latex.scm  --  LaTeX engine.
;;;
;;; Copyright 2003, 2004  Manuel Serrano
;;; Copyright 2007  Ludovic Courtès <ludovic.courtes@laas.fr>
;;;
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301,
;;; USA.

(define-module (skribilo engine latex)
  :use-module (skribilo lib)
  :use-module (skribilo ast)
  :use-module (skribilo engine)
  :use-module (skribilo writer)
  :use-module (skribilo location)
  :use-module (skribilo utils strings)
  :use-module (skribilo utils syntax)
  :use-module (skribilo package base)
  :autoload   (skribilo utils images)  (convert-image)
  :autoload   (skribilo evaluator)     (evaluate-document)
  :autoload   (skribilo output)        (output)
  :autoload   (skribilo debug)         (*debug*)
  :autoload   (skribilo color)         (skribe-color->rgb
                                        skribe-use-color!)
  :use-module (srfi srfi-13)
  :use-module (ice-9 optargs)
  :use-module (ice-9 receive)

  :export (latex-engine
           LaTeX TeX !latex
           skribe-get-latex-color))

(fluid-set! current-reader %skribilo-module-reader)



;*---------------------------------------------------------------------*/
;*    latex-verbatim-encoding ...                                      */
;*---------------------------------------------------------------------*/
(define latex-verbatim-encoding
   '((#\\ "{\\char92}")
     (#\^ "{\\char94}")
     (#\{ "\\{")
     (#\} "\\}")
     (#\& "\\&")
     (#\$ "\\$")
     (#\# "\\#")
     (#\_ "\\_")
     (#\% "\\%")
     (#\~ "$_{\\mbox{\\char126}}$")
     (#\ç "\\c{c}")
     (#\Ç "\\c{C}")
     (#\â "\\^{a}")
     (#\Â "\\^{A}")
     (#\à "\\`{a}")
     (#\À "\\`{A}")
     (#\á "\\'{a}")
     (#\Á "\\'{A}")
     (#\é "\\'{e}")
     (#\É "\\'{E}")
     (#\è "\\`{e}")
     (#\È "\\`{E}")
     (#\ê "\\^{e}")
     (#\Ê "\\^{E}")
     (#\ù "\\`{u}")
     (#\Ù "\\`{U}")
     (#\û "\\^{u}")
     (#\Û "\\^{U}")
     (#\ø "{\\o}")
     (#\ô "\\^{o}")
     (#\Ô "\\^{O}")
     (#\ö "\\\"{o}")
     (#\Ö "\\\"{O}")
     (#\î "\\^{\\i}")
     (#\Î "\\^{I}")
     (#\ï "\\\"{\\i}")
     (#\Ï "\\\"{I}")
     (#\] "{\\char93}")
     (#\[ "{\\char91}")
     (#\» "\\,{\\tiny{$^{\\gg}$}}")
     (#\« "{\\tiny{$^{\\ll}$}}\\,")))

;*---------------------------------------------------------------------*/
;*    latex-encoding ...                                               */
;*---------------------------------------------------------------------*/
(define latex-encoding
   (append '((#\| "$|$")
	     (#\< "$<$")
	     (#\> "$>$")
	     (#\: "{\\char58}")
	     (#\# "{\\char35}")
	     (#\Newline " %\n"))
	   latex-verbatim-encoding))
		  
;*---------------------------------------------------------------------*/
;*    latex-tt-encoding ...                                            */
;*---------------------------------------------------------------------*/
(define latex-tt-encoding
   (append '((#\. ".\\-")
	     (#\/ "/\\-"))
	   latex-encoding))
		  
;*---------------------------------------------------------------------*/
;*    latex-pre-encoding ...                                           */
;*---------------------------------------------------------------------*/
(define latex-pre-encoding
   (append '((#\Space "\\ ")
	     (#\Newline "\\\\\n"))
	   latex-encoding))

;*---------------------------------------------------------------------*/
;*    latex-symbol-table ...                                           */
;*---------------------------------------------------------------------*/
(define (latex-symbol-table math)
   `(("iexcl" "!`")
     ("cent" "c")
     ("pound" "\\pounds")
     ("yen" "Y")
     ("section" "\\S")
     ("mul" ,(math "^-"))
     ("copyright" "\\copyright")
     ("lguillemet" ,(math "\\ll"))
     ("not" ,(math "\\neg"))
     ("degree" ,(math "^{\\small{o}}"))
     ("plusminus" ,(math "\\pm"))
     ("micro" ,(math "\\mu"))
     ("paragraph" "\\P")
     ("middot" ,(math "\\cdot"))
     ("rguillemet" ,(math "\\gg"))
     ("1/4" ,(math "\\frac{1}{4}"))
     ("1/2" ,(math "\\frac{1}{2}"))
     ("3/4" ,(math "\\frac{3}{4}"))
     ("iquestion" "?`")
     ("Agrave" "\\`{A}")
     ("Aacute" "\\'{A}")
     ("Acircumflex" "\\^{A}")
     ("Atilde" "\\~{A}")
     ("Amul" "\\\"{A}")
     ("Aring" "{\\AA}")
     ("AEligature" "{\\AE}")
     ("Oeligature" "{\\OE}")
     ("Ccedilla" "{\\c{C}}")
     ("Egrave" "{\\`{E}}")
     ("Eacute" "{\\'{E}}")
     ("Ecircumflex" "{\\^{E}}")
     ("Euml" "\\\"{E}")
     ("Igrave" "{\\`{I}}")
     ("Iacute" "{\\'{I}}")
     ("Icircumflex" "{\\^{I}}")
     ("Iuml" "\\\"{I}")
     ("ETH" "D")
     ("Ntilde" "\\~{N}")
     ("Ograve" "\\`{O}")
     ("Oacute" "\\'{O}")
     ("Ocurcumflex" "\\^{O}")
     ("Otilde" "\\~{O}")
     ("Ouml" "\\\"{O}")
     ("times" ,(math "\\times"))
     ("Oslash" "\\O")
     ("Ugrave" "\\`{U}")
     ("Uacute" "\\'{U}")
     ("Ucircumflex" "\\^{U}")
     ("Uuml" "\\\"{U}")
     ("Yacute" "\\'{Y}")
     ("szlig" "\\ss")
     ("agrave" "\\`{a}")
     ("aacute" "\\'{a}")
     ("acircumflex" "\\^{a}")
     ("atilde" "\\~{a}")
     ("amul" "\\\"{a}")
     ("aring" "\\aa")
     ("aeligature" "\\ae")
     ("oeligature" "{\\oe}")
     ("ccedilla" "{\\c{c}}")
     ("egrave" "{\\`{e}}")
     ("eacute" "{\\'{e}}")
     ("ecircumflex" "{\\^{e}}")
     ("euml" "\\\"{e}")
     ("igrave" "{\\`{\\i}}")
     ("iacute" "{\\'{\\i}}")
     ("icircumflex" "{\\^{\\i}}")
     ("iuml" "\\\"{\\i}")
     ("ntilde" "\\~{n}")
     ("ograve" "\\`{o}")
     ("oacute" "\\'{o}")
     ("ocurcumflex" "\\^{o}")
     ("otilde" "\\~{o}")
     ("ouml" "\\\"{o}")
     ("divide" ,(math "\\div"))
     ("oslash" "\\o")
     ("ugrave" "\\`{u}")
     ("uacute" "\\'{u}")
     ("ucircumflex" "\\^{u}")
     ("uuml" "\\\"{u}")
     ("yacute" "\\'{y}")
     ("ymul" "\\\"{y}")
     ;; Greek
     ("Alpha" "A")
     ("Beta" "B")
     ("Gamma" ,(math "\\Gamma"))
     ("Delta" ,(math "\\Delta"))
     ("Epsilon" "E")
     ("Zeta" "Z")
     ("Eta" "H")
     ("Theta" ,(math "\\Theta"))
     ("Iota" "I")
     ("Kappa" "K")
     ("Lambda" ,(math "\\Lambda"))
     ("Mu" "M")
     ("Nu" "N")
     ("Xi" ,(math "\\Xi"))
     ("Omicron" "O")
     ("Pi" ,(math "\\Pi"))
     ("Rho" "P")
     ("Sigma" ,(math "\\Sigma"))
     ("Tau" "T")
     ("Upsilon" ,(math "\\Upsilon"))
     ("Phi" ,(math "\\Phi"))
     ("Chi" "X")
     ("Psi" ,(math "\\Psi"))
     ("Omega" ,(math "\\Omega"))
     ("alpha" ,(math "\\alpha"))
     ("beta" ,(math "\\beta"))
     ("gamma" ,(math "\\gamma"))
     ("delta" ,(math "\\delta"))
     ("epsilon" ,(math "\\varepsilon"))
     ("zeta" ,(math "\\zeta"))
     ("eta" ,(math "\\eta"))
     ("theta" ,(math "\\theta"))
     ("iota" ,(math "\\iota"))
     ("kappa" ,(math "\\kappa"))
     ("lambda" ,(math "\\lambda"))
     ("mu" ,(math "\\mu"))
     ("nu" ,(math "\\nu"))
     ("xi" ,(math "\\xi"))
     ("omicron" ,(math "\\o"))
     ("pi" ,(math "\\pi"))
     ("rho" ,(math "\\rho"))
     ("sigmaf" ,(math "\\varsigma"))
     ("sigma" ,(math "\\sigma"))
     ("tau" ,(math "\\tau"))
     ("upsilon" ,(math "\\upsilon"))
     ("phi" ,(math "\\varphi"))
     ("chi" ,(math "\\chi"))
     ("psi" ,(math "\\psi"))
     ("omega" ,(math "\\omega"))
     ("thetasym" ,(math "\\vartheta"))
     ("piv" ,(math "\\varpi"))
     ;; punctuation
     ("bullet" ,(math "\\bullet"))
     ("ellipsis" ,(math "\\ldots"))
     ("weierp" ,(math "\\wp"))
     ("image" ,(math "\\Im"))
     ("real" ,(math "\\Re"))
     ("tm" ,(math "^{\\sc\\tiny{tm}}"))
     ("alef" ,(math "\\aleph"))
     ("<-" ,(math "\\leftarrow"))
     ("<--" ,(math "\\longleftarrow"))
     ("uparrow" ,(math "\\uparrow"))
     ("->" ,(math "\\rightarrow"))
     ("-->" ,(math "\\longrightarrow"))
     ("downarrow" ,(math "\\downarrow"))
     ("<->" ,(math "\\leftrightarrow"))
     ("<-->" ,(math "\\longleftrightarrow"))
     ("<+" ,(math "\\hookleftarrow"))
     ("<=" ,(math "\\Leftarrow"))
     ("<==" ,(math "\\Longleftarrow"))
     ("Uparrow" ,(math "\\Uparrow"))
     ("=>" ,(math "\\Rightarrow"))
     ("==>" ,(math "\\Longrightarrow"))
     ("Downarrow" ,(math "\\Downarrow"))
     ("<=>" ,(math "\\Leftrightarrow"))
     ("<==>" ,(math "\\Longleftrightarrow"))
     ;; Mathematical operators
     ("forall" ,(math "\\forall"))
     ("partial" ,(math "\\partial"))
     ("exists" ,(math "\\exists"))
     ("emptyset" ,(math "\\emptyset"))
     ("infinity" ,(math "\\infty"))
     ("nabla" ,(math "\\nabla"))
     ("in" ,(math "\\in"))
     ("notin" ,(math "\\notin"))
     ("ni" ,(math "\\ni"))
     ("prod" ,(math "\\Pi"))
     ("sum" ,(math "\\Sigma"))
     ("asterisk" ,(math "\\ast"))
     ("sqrt" ,(math "\\surd"))
     ("propto" ,(math "\\propto"))
     ("angle" ,(math "\\angle"))
     ("and" ,(math "\\wedge"))
     ("or" ,(math "\\vee"))
     ("cap" ,(math "\\cap"))
     ("cup" ,(math "\\cup"))
     ("integral" ,(math "\\int"))
     ("models" ,(math "\\models"))
     ("vdash" ,(math "\\vdash"))
     ("dashv" ,(math "\\dashv"))
     ("sim" ,(math "\\sim"))
     ("cong" ,(math "\\cong"))
     ("approx" ,(math "\\approx"))
     ("neq" ,(math "\\neq"))
     ("equiv" ,(math "\\equiv"))
     ("le" ,(math "\\leq"))
     ("ge" ,(math "\\geq"))
     ("subset" ,(math "\\subset"))
     ("supset" ,(math "\\supset"))
     ("subseteq" ,(math "\\subseteq"))
     ("supseteq" ,(math "\\supseteq"))
     ("oplus" ,(math "\\oplus"))
     ("otimes" ,(math "\\otimes"))
     ("perp" ,(math "\\perp"))
     ("mid" ,(math "\\mid"))
     ("lceil" ,(math "\\lceil"))
     ("rceil" ,(math "\\rceil"))
     ("lfloor" ,(math "\\lfloor"))
     ("rfloor" ,(math "\\rfloor"))
     ("langle" ,(math "\\langle"))
     ("rangle" ,(math "\\rangle"))
     ;; Misc
     ("loz" ,(math "\\diamond"))
     ("spades" ,(math "\\spadesuit"))
     ("clubs" ,(math "\\clubsuit"))
     ("hearts" ,(math "\\heartsuit"))
     ("diams" ,(math "\\diamondsuit"))
     ("euro" "\\euro{}")
     ;; LaTeX
     ("dag" "\\dag")
     ("ddag" "\\ddag")
     ("circ" ,(math "\\circ"))
     ("top" ,(math "\\top"))
     ("bottom" ,(math "\\bot"))
     ("lhd" ,(math "\\triangleleft"))
     ("rhd" ,(math "\\triangleright"))
     ("parallel" ,(math "\\parallel"))))

;*---------------------------------------------------------------------*/
;*    latex-engine ...                                                 */
;*---------------------------------------------------------------------*/
(define latex-engine
   (default-engine-set!
      (make-engine 'latex
	 :version 1.0
	 :format "latex"
	 :delegate (find-engine 'base)
	 :filter (make-string-replace latex-encoding)
	 :custom '((documentclass "\\documentclass{article}")
                   (class-has-chapters? #f)
		   (usepackage "\\usepackage{epsfig}\n")
		   (predocument "\\newdimen\\oldframetabcolsep\n\\newdimen\\oldcolortabcolsep\n\\newdimen\\oldpretabcolsep\n")
		   (postdocument #f)
		   (maketitle "\\date{}\n\\maketitle")
		   (%font-size 0)
		   ;; color
		   (color #t)
		   (color-usepackage "\\usepackage{color}\n")
		   ;; hyperref
		   (hyperref #t)
		   (hyperref-usepackage "\\usepackage[setpagesize=false]{hyperref}\n")
		   ;; source fontification
		   (source-color #t)
		   (source-comment-color "#ffa600")
		   (source-error-color "red")
		   (source-define-color "#6959cf")
		   (source-module-color "#1919af")
		   (source-markup-color "#1919af")
		   (source-thread-color "#ad4386")
		   (source-string-color "red")
		   (source-bracket-color "red")
		   (source-type-color "#00cf00")
		   (image-format ("eps"))
		   (index-page-ref #t))
	 :symbol-table (latex-symbol-table 
			(lambda (m)
			   (format #f "\\begin{math}~a\\end{math}" m))))))

;*---------------------------------------------------------------------*/
;*    latex-title-engine ...                                           */
;*---------------------------------------------------------------------*/
(define latex-title-engine
   (make-engine 'latex-title
      :version 1.0
      :format "latex-title"
      :delegate latex-engine
      :filter (make-string-replace latex-encoding)
      :symbol-table (latex-symbol-table (lambda (m) (format #f "$~a$" m)))))

;*---------------------------------------------------------------------*/
;*    latex-color? ...                                                 */
;*---------------------------------------------------------------------*/
(define (latex-color? e)
   (engine-custom e 'color))

;*---------------------------------------------------------------------*/
;*    LaTeX ...                                                        */
;*---------------------------------------------------------------------*/
(define* (LaTeX :key (space #t))
   (if (engine-format? "latex")
       (! (if space "\\LaTeX\\ " "\\LaTeX"))
       "LaTeX"))

;*---------------------------------------------------------------------*/
;*    TeX ...                                                          */
;*---------------------------------------------------------------------*/
(define* (TeX :key (space #t))
   (if (engine-format? "latex")
       (! (if space "\\TeX\\ " "\\TeX"))
       "TeX"))

;*---------------------------------------------------------------------*/
;*    latex ...                                                        */
;*---------------------------------------------------------------------*/
(define* (!latex fmt :rest opt)
   (if (engine-format? "latex")
       (apply ! fmt opt)
       #f))

;*---------------------------------------------------------------------*/
;*    latex-width ...                                                  */
;*---------------------------------------------------------------------*/
(define (latex-width width)
   (if (and (number? width) (inexact? width))
       (string-append (number->string (/ width 100.)) "\\linewidth")
       (string-append (number->string width) "pt")))

;*---------------------------------------------------------------------*/
;*    latex-font-size ...                                              */
;*---------------------------------------------------------------------*/
(define (latex-font-size size)
   (case size
      ((4) "Huge")
      ((3) "huge")
      ((2) "Large")
      ((1) "large")
      ((0) "normalsize")
      ((-1) "small")
      ((-2) "footnotesize")
      ((-3) "scriptsize")
      ((-4) "tiny")
      (else (if (number? size)
		(if (< size 0) "tiny" "Huge")
		"normalsize"))))

;*---------------------------------------------------------------------*/
;*    *skribe-latex-color-table* ...                                   */
;*---------------------------------------------------------------------*/
(define *skribe-latex-color-table* #f)

;*---------------------------------------------------------------------*/
;*    latex-declare-color ...                                          */
;*---------------------------------------------------------------------*/
(define (latex-declare-color name rgb)
   (format #t "\\definecolor{~a}{rgb}{~a}\n" name rgb))

;*---------------------------------------------------------------------*/
;*    skribe-get-latex-color ...                                       */
;*---------------------------------------------------------------------*/
(define (skribe-get-latex-color spec)
   (let ((c (and (hash-table? *skribe-latex-color-table*)
		 (hash-ref *skribe-latex-color-table* spec))))
      (if (not (string? c))
	  (skribe-error 'latex "Can't find color" spec)
	  c)))

;*---------------------------------------------------------------------*/
;*    skribe-color->latex-rgb ...                                      */
;*---------------------------------------------------------------------*/
(define (skribe-color->latex-rgb spec)
   (receive (r g b)
      (skribe-color->rgb spec)
      (cond
	 ((and (= r 0) (= g 0) (= b 0))
	  "0.,0.,0.")
	 ((and (= r #xff) (= g #xff) (= b #xff))
	  "1.,1.,1.")
	 (else
	  (let ((ff (exact->inexact #xff)))
	    (format #f "~a,~a,~a"
		    (number->string (/ r ff))
		    (number->string (/ g ff))
		    (number->string (/ b ff))))))))

;*---------------------------------------------------------------------*/
;*    skribe-latex-declare-colors ...                                  */
;*---------------------------------------------------------------------*/
(define (skribe-latex-declare-colors colors)
   (set! *skribe-latex-color-table* (make-hash-table))
   (for-each (lambda (spec)
		(let ((old (hash-ref *skribe-latex-color-table* spec)))
		   (if (not (string? old))
		       (let ((name (symbol->string (gensym "c"))))
			  ;; bind the color 
			  (hash-set! *skribe-latex-color-table* spec name)
			  ;; and emit a latex declaration
			  (latex-declare-color 
			   name 
			   (skribe-color->latex-rgb spec))))))
	     colors))

;*---------------------------------------------------------------------*/
;*    ~ ...                                                           */
;*---------------------------------------------------------------------*/
(markup-writer '~
   :before "~"
   :action #f)

;*---------------------------------------------------------------------*/
;*    breakable-space ...                                              */
;*---------------------------------------------------------------------*/
(markup-writer 'breakable-space
   :before " %\n"
   :action #f)

;*---------------------------------------------------------------------*/
;*    &latex-table-start                                               */
;*---------------------------------------------------------------------*/
(markup-writer '&latex-table-start
   :options '()
   :action (lambda (n e)
	      (let ((width (markup-option n 'width)))
		 (if (number? width)
		     (format #t "\\begin{tabular*}{~a}" (latex-width width))
		     (display "\\begin{tabular}")))))

;*---------------------------------------------------------------------*/
;*    &latex-table-stop                                                */
;*---------------------------------------------------------------------*/
(markup-writer '&latex-table-stop
   :options '()
   :action (lambda (n e)
	      (let ((width (markup-option n 'width)))
		 (if (number? width)
		 (display "\\end{tabular*}\n")
		 (display "\\end{tabular}\n")))))
   
;*---------------------------------------------------------------------*/
;*    document ...                                                     */
;*---------------------------------------------------------------------*/
(markup-writer 'document
   :options '(:title :author :ending :env)
   :before (lambda (n e)
	      ;; documentclass
	      (let ((dc (engine-custom e 'documentclass)))
		 (if dc
		     (begin (display dc) (newline))
		     (display "\\documentclass{article}\n")))
	      (if (latex-color? e)
		  (display (engine-custom e 'color-usepackage)))
	      (if (engine-custom e 'hyperref)
		  (display (engine-custom e 'hyperref-usepackage)))
	      ;; usepackage
	      (let ((pa (engine-custom e 'usepackage)))
		 (if pa (begin (display pa) (newline))))
	      ;; colors
	      (if (latex-color? e)
		  (begin
		     (skribe-use-color! (engine-custom e 'source-comment-color))
		     (skribe-use-color! (engine-custom e 'source-define-color))
		     (skribe-use-color! (engine-custom e 'source-module-color))
		     (skribe-use-color! (engine-custom e 'source-markup-color))
		     (skribe-use-color! (engine-custom e 'source-thread-color))
		     (skribe-use-color! (engine-custom e 'source-string-color))
		     (skribe-use-color! (engine-custom e 'source-bracket-color))
		     (skribe-use-color! (engine-custom e 'source-type-color))
		     (display "\n%% colors\n")
		     (skribe-latex-declare-colors (skribe-get-used-colors))
		     (display "\n\n")))
	      ;; predocument
	      (let ((pd (engine-custom e 'predocument)))
		 (when pd (display pd) (newline)))
	      ;; title
	      (let ((t (markup-option n :title)))
		 (when t
		    (evaluate-document
                     (new markup
                          (markup '&latex-title)
                          (body t))
                     e
                     :env `((parent ,n)))))
	      ;; author
	      (let ((a (markup-option n :author)))
		 (when a
		    (evaluate-document
                     (new markup
                          (markup '&latex-author)
                          (body a))
                     e
                     :env `((parent ,n)))))
	      ;; document
	      (display "\\begin{document}\n")
	      ;; postdocument
	      (let ((pd (engine-custom e 'postdocument)))
		 (if pd (begin (display pd) (newline))))
	      ;; maketitle
	      (let ((mt (engine-custom e 'maketitle)))
		 (if mt (begin (display mt) (newline)))))
   :action (lambda (n e)
	      (output (markup-body n) e))
   :after (lambda (n e)
	     (display "\n\\end{document}\n")))

;*---------------------------------------------------------------------*/
;*    &latex-title ...                                                 */
;*---------------------------------------------------------------------*/
(markup-writer '&latex-title
   :before "\\title{"
   :after "}\n")

;*---------------------------------------------------------------------*/
;*    &latex-author ...                                                */
;*---------------------------------------------------------------------*/
(markup-writer '&latex-author
   :before "\\author{\\centerline{\n"
   :action (lambda (n e)
	      (let ((body (markup-body n)))
		 (if (pair? body)
		     (begin
			(output (new markup
				   (markup '&latex-table-start)
				   (class "&latex-author-table"))
				e)
			(format #t "{~a}\n" (make-string (length body) #\c))
			(let loop ((as body))
			   (output (car as) e)
			   (if (pair? (cdr as))
			       (begin
				  (display " & ")
				  (loop (cdr as)))))
			(display "\\\\\n")
			(output (new markup
				   (markup '&latex-table-stop)
				   (class "&latex-author-table"))
				e))
		     (output body e))))
   :after "}}\n")
		 
;*---------------------------------------------------------------------*/
;*    author ...                                                       */
;*---------------------------------------------------------------------*/
(markup-writer 'author
   :options '(:name :title :affiliation :email :url :address :phone :photo :align)
   :before (lambda (n e)
	      (output (new markup
			 (markup '&latex-table-start)
			 (class "author"))
		      e)
	      (format #t "{~a}\n"
		      (case (markup-option n :align)
			 ((left) "l")
			 ((right) "r")
			 (else "c"))))
   :action (lambda (n e)
	      (let ((name (markup-option n :name))
		    (title (markup-option n :title))
		    (affiliation (markup-option n :affiliation))
		    (email (markup-option n :email))
		    (url (markup-option n :url))
		    (address (markup-option n :address))
		    (phone (markup-option n :phone)))
		 (define (row n)
		    (output n e)
		    (display "\\\\\n"))
		 ;; name
		 (if name (row name))
		 ;; title
		 (if title (row title))
		 ;; affiliation
		 (if affiliation (row affiliation))
		 ;; address
		 (cond
		    ((pair? address)
		     (for-each row address))
		    ((string? address)
		     (row address)))
		 ;; telephone
		 (if phone (row phone))
		 ;; email
		 (if email (row email))
		 ;; url
		 (if url (row url))))
   :after (lambda (n e)
	     (output (new markup
			(markup '&latex-table-stop)
			(class "author"))
		     e)))

;*---------------------------------------------------------------------*/
;*    author ...                                                       */
;*---------------------------------------------------------------------*/
(markup-writer 'author
   :options '(:name :title :affiliation :email :url :address :phone :photo :align)
   :predicate (lambda (n e) (markup-option n :photo))
   :before (lambda (n e)
	      (output (new markup
			 (markup '&latex-table-start)
			 (class "author"))
		      e)
	      (display "{cc}\n"))
   :action (lambda (n e)
	      (let ((photo (markup-option n :photo)))
		 (output photo e)
		 (display " & ")
		 (markup-option-add! n :photo #f)
		 (output n e)
		 (markup-option-add! n :photo photo)
		 (display "\\\\\n")))
   :after (lambda (n e)
	     (output (new markup
			(markup '&latex-table-stop)
			(class "author"))
		     e)))

;*---------------------------------------------------------------------*/
;*    toc ...                                                          */
;*---------------------------------------------------------------------*/
(markup-writer 'toc
   :options '()
   :action (lambda (n e) (display "\\tableofcontents\n")))

;*---------------------------------------------------------------------*/
;*    latex-block-before ...                                           */
;*---------------------------------------------------------------------*/
(define (latex-block-before m)

   ;; Mapping of Skribilo markups to LaTeX, with and without chapters.
   (define %chapter-mapping
     '((chapter       . "chapter")
       (section       . "section")
       (subsection    . "subsection")
       (subsubsection . "subsubsection")))
   (define %chapterless-mapping
     '((chapter       . "section")
       (section       . "subsection")
       (subsection    . "subsubsection")
       (subsubsection . "subsubsection")))

   (lambda (n e)
      (let* ((num (markup-option n :number))
             (markup-mapping (if (engine-custom e 'class-has-chapters?)
                                 %chapter-mapping
                                 %chapterless-mapping))
             (latex-markup (cdr (assq m markup-mapping))))
	 (format #t "\n\n%% ~a\n" (string-canonicalize (markup-ident n)))
	 (format #t "\\~a~a{" latex-markup (if (not num) "*" ""))
	 (output (markup-option n :title) latex-title-engine)
	 (display "}\n")
	 (when num
	    (format #t "\\label{~a}\n" (string-canonicalize (markup-ident n)))))))

;*---------------------------------------------------------------------*/
;*    section ... .. @label chapter@                                   */
;*---------------------------------------------------------------------*/
(markup-writer 'chapter
   :options '(:title :number :toc :file :env)
   :before (latex-block-before 'chapter))

;*---------------------------------------------------------------------*/
;*    section ... . @label section@                                    */
;*---------------------------------------------------------------------*/
(markup-writer 'section
   :options '(:title :number :toc :file :env)
   :before (latex-block-before 'section))

;*---------------------------------------------------------------------*/
;*    subsection ... @label subsection@                                */
;*---------------------------------------------------------------------*/
(markup-writer 'subsection
   :options '(:title :number :toc :file :env)
   :before (latex-block-before 'subsection))

;*---------------------------------------------------------------------*/
;*    subsubsection ... @label subsubsection@                          */
;*---------------------------------------------------------------------*/
(markup-writer 'subsubsection
   :options '(:title :number :toc :file :env)
   :before (latex-block-before 'subsubsection))

;*---------------------------------------------------------------------*/
;*    paragraph ...                                                    */
;*---------------------------------------------------------------------*/
(markup-writer 'paragraph
   :options '(:title :number :toc :env)
   :before (lambda (n e)
	      (when (and (>= (*debug*) 2) (location? (ast-loc n)))
		 (format #t "\n\\makebox[\\linewidth][l]{\\hspace{-1.5cm}\\footnotesize{$\\triangleright$\\textit{~a}}}\n" 
			 (ast-location n)))
	      (display "\\noindent "))
   :after "\\par\n")

;*---------------------------------------------------------------------*/
;*    footnote ...                                                     */
;*---------------------------------------------------------------------*/
(markup-writer 'footnote
   :before "\\footnote{"
   :after "}")

;*---------------------------------------------------------------------*/
;*    linebreak ...                                                    */
;*---------------------------------------------------------------------*/
(markup-writer 'linebreak
   :action (lambda (n e)
	      (display "\\makebox[\\linewidth]{}")))

;*---------------------------------------------------------------------*/
;*    hrule ...                                                        */
;*---------------------------------------------------------------------*/
(markup-writer 'hrule 
   :options '()
   :before "\\hrulefill"
   :action #f)

;*---------------------------------------------------------------------*/
;*    latex-color-counter                                              */
;*---------------------------------------------------------------------*/
(define latex-color-counter 1)

;*---------------------------------------------------------------------*/
;*    latex-color ...                                                  */
;*---------------------------------------------------------------------*/
(define latex-color 
   (lambda (bg fg n e)
      (if (not (latex-color? e))
	  (output n e)
	  (begin
	     (if bg
		 (format #t "\\setbox~a \\vbox \\bgroup " latex-color-counter))
	     (set! latex-color-counter (+ latex-color-counter 1))
	     (if fg
		 (begin
		    (format #t "\\textcolor{~a}{" (skribe-get-latex-color fg))
		    (output n e)
		    (display "}"))
		 (output n e))
	     (set! latex-color-counter (- latex-color-counter 1))
	     (if bg
		 (format #t "\\egroup\\colorbox{~a}{\\box~a}%\n"
			 (skribe-get-latex-color bg) latex-color-counter))))))
   
;*---------------------------------------------------------------------*/
;*    color ...                                                        */
;*---------------------------------------------------------------------*/
(markup-writer 'color
   :options '(:bg :fg :width)
   :action (lambda (n e) 
	      (let* ((w (markup-option n :width))
		     (bg (markup-option n :bg))
		     (fg (markup-option n :fg))
		     (m (markup-option n :margin))
		     (tw (cond
			    ((not w)
			     #f)
			    ((and (integer? w) (exact? w))
			     w)
			    ((real? w)
			     (latex-width w)))))
		 (when bg 
		    (display "\\setlength{\\oldcolortabcolsep}{\\tabcolsep}\n\\addtolength{\\tabcolsep}{-\\tabcolsep}\n")
		    (when m
		       (format #t "\\addtolength{\\tabcolsep}{~a}" 
			       (latex-width m)))
		    (output (new markup
			       (markup '&latex-table-start)
			       (class "color"))
			    e)
		    (if tw
			(format #t "{p{~a}}\n" tw)
			(display "{l}\n")))
		 (latex-color bg fg (markup-body n) e)
		 (when bg 
		    (output (new markup
			       (markup '&latex-table-stop)
			       (class "color"))
			    e)
		    (display "\\setlength{\\tabcolsep}{\\oldcolortabcolsep}\n")))))

;*---------------------------------------------------------------------*/
;*    frame ...                                                        */
;*---------------------------------------------------------------------*/
(markup-writer 'frame
   :options '(:width :border :margin)
   :before (lambda (n e)
	      (display "\\setlength{\\oldframetabcolsep}{\\tabcolsep}\n\\addtolength{\\tabcolsep}{-\\tabcolsep}")
	      (let ((m (markup-option n :margin)))
		 (when m
		    (format #t "\\addtolength{\\tabcolsep}{~a}" (latex-width m))))
	      (newline))
   :action (lambda (n e) 
	      (let* ((b (markup-option n :border))
		     (w (markup-option n :width))
		     (tw (cond
			    ((not w)
			     ".96\\linewidth")
			    ((and (integer? w) (exact? w))
			     w)
			    ((real? w)
			     (latex-width w)))))
		 (output (new markup
			    (markup '&latex-table-start)
			    (class "frame"))
			 e)
		 (if (and (integer? b) (> b 0))
		     (begin
			(format #t "{|p{~a}|}\\hline\n" tw)
			(output (markup-body n) e)
			(display "\\\\\\hline\n"))
		     (begin
			(format #t "{p{~a}}\n" tw)
			(output (markup-body n) e)))
		 (output (new markup
			    (markup '&latex-table-stop)
			    (class "author"))
			 e)))
   :after "\\setlength{\\tabcolsep}{\\oldframetabcolsep}\n")

;*---------------------------------------------------------------------*/
;*    font ...                                                         */
;*---------------------------------------------------------------------*/
(markup-writer 'font
   :options '(:size)
   :action (lambda (n e) 
	      (let* ((size (markup-option n :size))
		     (cs (let ((n (engine-custom e '%font-size)))
			    (if (number? n)
				n 
				0)))
		     (ns (cond
			    ((and (integer? size) (exact? size))
			     (if (> size 0)
				 size
				 (+ cs size)))
			    ((and (number? size) (inexact? size))
			     (+ cs (inexact->exact size)))
			    ((string? size)
			     (let ((nb (string->number size)))
				(if (not (number? nb))
				    (skribe-error 
				     'font
				     (format #f "Illegal font size ~s" size)
				     nb)
				    (+ cs nb))))))
		     (ne (make-engine (gensym "latex")
				      :delegate e
				      :filter (engine-filter e)
				      :symbol-table (engine-symbol-table e)
				      :custom `((%font-size ,ns)
						,@(engine-customs e)))))
		 (format #t "{\\~a{" (latex-font-size ns))
		 (output (markup-body n) ne)
		 (display "}}"))))

;*---------------------------------------------------------------------*/
;*    flush ...                                                        */
;*---------------------------------------------------------------------*/
(markup-writer 'flush
   :options '(:side)
   :before (lambda (n e)
	      (case (markup-option n :side)
		 ((center)
		  (display "\\begin{center}\n"))
		 ((left)
		  (display "\\begin{flushleft}"))
		 ((right)
		  (display "\\begin{flushright}"))))
   :after (lambda (n e)
	     (case (markup-option n :side)
		((center)
		 (display "\\end{center}\n"))
		((left)
		 (display "\\end{flushleft}\n"))
		((right)
		 (display "\\end{flushright}\n")))))

;*---------------------------------------------------------------------*/
;*    center ...                                                       */
;*---------------------------------------------------------------------*/
(markup-writer 'center
   :before "\\begin{center}\n"
   :after "\\end{center}\n")

;*---------------------------------------------------------------------*/
;*    pre ...                                                          */
;*---------------------------------------------------------------------*/
(markup-writer 'pre
   :before (lambda (n e)
	      (format #t "\\setlength{\\oldpretabcolsep}{\\tabcolsep}\n\\addtolength{\\tabcolsep}{-\\tabcolsep}\n{\\setbox~a \\vbox \n\\bgroup\n{\\noindent \\texttt{"
		      latex-color-counter)
	      (output (new markup
			 (markup '&latex-table-start)
			 (class "pre"))
		      e)
	      (display "{l}\n")
	      (set! latex-color-counter (+ latex-color-counter 1)))
   :action (lambda (n e)
	      (let ((ne (make-engine
			   (gensym "latex")
			   :delegate e
			   :filter (make-string-replace latex-pre-encoding)
			   :symbol-table (engine-symbol-table e)
			   :custom (engine-customs e))))
		 (output (markup-body n) ne)))
   :after (lambda (n e)
	     (set! latex-color-counter (- latex-color-counter 1))
	     (output (new markup
			(markup '&latex-table-stop)
			(class "pre"))
		     e)
	     (format #t "}}\n\\egroup{\\box~a}}%\n\\setlength{\\tabcolsep}{\\oldpretabcolsep}\n" latex-color-counter)))

;*---------------------------------------------------------------------*/
;*    prog ...                                                         */
;*---------------------------------------------------------------------*/
(markup-writer 'prog
   :options '(:line :mark)
   :before (lambda (n e)
	      (format #t "\\setlength{\\oldpretabcolsep}{\\tabcolsep}\n\\addtolength{\\tabcolsep}{-\\tabcolsep}\n{\\setbox~a \\vbox \\bgroup\n{\\noindent \\texttt{"
		      latex-color-counter)
	      (output (new markup
			 (markup '&latex-table-start)
			 (class "pre"))
		      e)
	      (display "{l}\n")
	      (set! latex-color-counter (+ latex-color-counter 1)))
   :action (lambda (n e)
	      (let ((ne (make-engine
			   (gensym "latex")
			   :delegate e
			   :filter (make-string-replace latex-pre-encoding)
			   :symbol-table (engine-symbol-table e)
			   :custom (engine-customs e))))
		 (output (markup-body n) ne)))
   :after (lambda (n e)
	     (set! latex-color-counter (- latex-color-counter 1))
	     (output (new markup
			(markup '&latex-table-stop)
			(class "prog"))
		     e)
	     (format #t "}}\n\\egroup{\\box~a}}%\n\\setlength{\\tabcolsep}{\\oldpretabcolsep}\n" latex-color-counter)))

;*---------------------------------------------------------------------*/
;*    &prog-line ...                                                   */
;*---------------------------------------------------------------------*/
(markup-writer '&prog-line
   :before (lambda (n e)
             (let ((num (markup-option n :number)))
               (if (number? num)
                   (evaluate-document
                    (it (string-append (string-pad (number->string num) 3)
                                       ": "))
                    e))))
   :after "\\\\\n")

;*---------------------------------------------------------------------*/
;*    itemize ...                                                      */
;*---------------------------------------------------------------------*/
(markup-writer 'itemize
   :options '(:symbol)	       
   :before "\\begin{itemize}\n"
   :action (lambda (n e)
	      (for-each (lambda (item)
			   (display " \\item ")
			   (output item e)
			   (newline))
			(markup-body n)))
   :after "\\end{itemize} ")

(markup-writer 'itemize
   :predicate (lambda (n e) (markup-option n :symbol))
   :options '(:symbol)	       
   :before (lambda (n e)
	      (display "\\begin{list}{")
	      (output (markup-option n :symbol) e)
	      (display "}{}")
	      (newline))
   :action (lambda (n e)
	      (for-each (lambda (item)
			   (display " \\item ")
			   (output item e)
			   (newline))
			(markup-body n)))
   :after "\\end{list}\n")

;*---------------------------------------------------------------------*/
;*    enumerate ...                                                    */
;*---------------------------------------------------------------------*/
(markup-writer 'enumerate
   :options '(:symbol)	       
   :before "\\begin{enumerate}\n"
   :action (lambda (n e)
	      (for-each (lambda (item)
			   (display " \\item ")
			   (output item e)
			   (newline))
			(markup-body n)))
   :after "\\end{enumerate}\n")

;*---------------------------------------------------------------------*/
;*    description ...                                                  */
;*---------------------------------------------------------------------*/
(markup-writer 'description
   :options '(:symbol)	       
   :before "\\begin{description}\n"
   :action (lambda (n e)
	      (for-each (lambda (item)
			   (let ((k (markup-option item :key)))
			      (for-each (lambda (i)
					   (display " \\item[")
					   (output i e)
					   (display "]\n"))
					(if (pair? k) k (list k)))
			      (output (markup-body item) e)))
			(markup-body n)))
   :after "\\end{description}\n")

;*---------------------------------------------------------------------*/
;*    item ...                                                         */
;*---------------------------------------------------------------------*/
(markup-writer 'item
   :options '(:key)	       
   :action (lambda (n e)
	      (let ((k (markup-option n :key)))
		 (if k
		     (begin
			(display "[")
			(output k e)
			(display "] "))))
	      (output (markup-body n) e)))

;*---------------------------------------------------------------------*/
;*    blockquote ...                                                   */
;*---------------------------------------------------------------------*/
(markup-writer 'blockquote
   :before "\n\\begin{quote}\n"
   :after  "\n\\end{quote}")

;*---------------------------------------------------------------------*/
;*    figure ... @label figure@                                        */
;*---------------------------------------------------------------------*/
(markup-writer 'figure
   :options '(:legend :number :multicolumns)
   :action (lambda (n e)
	      (let ((ident (markup-ident n))
		    (legend (markup-option n :legend))
		    (mc (markup-option n :multicolumns)))
		 (display (if mc
			      "\\begin{figure*}[!th]\n"
			      "\\begin{figure}[ht]\n"))
		 (output (markup-body n) e)
		 (format #t "\\caption{\\label{~a}" (string-canonicalize ident))
		 (output legend e)
		 (display (if mc
			      "}\\end{figure*}\n"
			      "}\\end{figure}\n")))))

;*---------------------------------------------------------------------*/
;*    table-column-number ...                                          */
;*    -------------------------------------------------------------    */
;*    Computes how many columns are contained in a table.              */
;*---------------------------------------------------------------------*/
(define (table-column-number t)
   (define (row-columns row)
      (let luup ((cells (markup-body row))
		 (nbcols 0))
	 (cond
	   ((null? cells)
	     nbcols)
	   ((pair? cells)
	    (luup (cdr cells)
		  (+ nbcols (markup-option (car cells) :colspan))))
	   (else
	    (skribe-type-error 'tr "Illegal tr body, " row "pair")))))
   (let loop ((rows (markup-body t))
	      (nbcols 0))
      (if (null? rows)
	  nbcols
	  (loop (cdr rows)
		(max (row-columns (car rows)) nbcols)))))

;*---------------------------------------------------------------------*/
;*    table ...                                                        */
;*---------------------------------------------------------------------*/
(markup-writer 'table
   :options '(:width :frame :rules :cellstyle)
   :before (lambda (n e)
	      (let ((width (markup-option n :width))
		    (frame (markup-option n :frame))
		    (rules (markup-option n :rules))
		    (cstyle (markup-option n :cellstyle))
		    (nbcols (table-column-number n))
		    (id (markup-ident n))
		    (rows (markup-body n)))
		 ;; the table header
		 (output (new markup
			    (markup '&latex-table-start)
			    (class "table")
			    (options `((width ,width))))
			 e)
		 ;; store the actual number of columns
		 (markup-option-add! n '&nbcols nbcols)
		 ;; compute the table header
		 (let ((cols (cond
				((= nbcols 0)
				 (skribe-error 'table
					       "Illegal empty table"
					       n))
				((or (not width) (= nbcols 1))
				 (make-string nbcols #\c))
				(else
				 (let ((v (make-vector 
					   (- nbcols 1)
					   "@{\\extracolsep{\\fill}}c")))
				    (string-concatenate
					   (cons "c" (vector->list v))))))))
		    (case frame
		       ((none)
			(format #t "{~a}\n" cols))
		       ((border box)
			(format #t "{|~a|}" cols)
			(markup-option-add! n '&lhs #t)
			(markup-option-add! n '&rhs #t)
			(output (new markup
				   (markup '&latex-table-hline)
				   (parent n)
				   (ident (format #f "~a-above" id))
				   (class "table-line-above"))
				e))
		       ((above hsides)
			(format #t "{~a}" cols)
			(output (new markup
				   (markup '&latex-table-hline)
				   (parent n)
				   (ident (format #f "~a-above" id))
				   (class "table-line-above"))
				e))
		       ((vsides)
			(markup-option-add! n '&lhs #t)
			(markup-option-add! n '&rhs #t)
			(format #t "{|~a|}\n" cols))
		       ((lhs)
			(markup-option-add! n '&lhs #t)
			(format #t "{|~a}\n" cols))
		       ((rhs)
			(markup-option-add! n '&rhs #t)
			(format #t "{~a|}\n" cols))
		       (else
			(format #t "{~a}\n" cols)))
		    ;; mark each row with appropriate '&tl (top-line)
		    ;; and &bl (bottom-line) options
		    (when (pair? rows)
		       (if (and (memq rules '(rows all))
				(or (not (eq? cstyle 'collapse))
				    (not (memq frame '(border box above hsides)))))
			   (let ((frow (car rows)))
			      (if (is-markup? frow 'tr)
				  (markup-option-add! frow '&tl #t))))
		       (if (eq? rules 'header)
			   (let ((frow (car rows)))
			      (if (is-markup? frow 'tr)
				  (markup-option-add! frow '&bl #t))))
		       (when (and (pair? (cdr rows))
				  (memq rules '(rows all)))
			  (for-each (lambda (row)
				       (if (is-markup? row 'tr)
					   (markup-option-add! row '&bl #t)))
				    rows)
			  (markup-option-add! (car (last-pair rows)) '&bl #f))
		       (if (and (memq rules '(rows all))
				(or (not (eq? cstyle 'collapse))
				    (not (memq frame '(border box above hsides)))))
			   (let ((lrow (car (last-pair rows))))
			      (if (is-markup? lrow 'tr)
				  (markup-option-add! lrow '&bl #t))))))))
   :after (lambda (n e)
	     (case (markup-option n :frame)
		((hsides below box border)
		 (output (new markup
			    (markup '&latex-table-hline)
			    (parent n)
			    (ident (format #f "~a-below" (markup-ident n)))
			    (class "table-hline-below"))
			 e)))
	     (output (new markup
			(markup '&latex-table-stop)
			(class "table")
			(options `((width ,(markup-option n :width)))))
		     e)))

;*---------------------------------------------------------------------*/
;*    &latex-table-hline                                               */
;*---------------------------------------------------------------------*/
(markup-writer '&latex-table-hline
   :action "\\hline\n")

;*---------------------------------------------------------------------*/
;*    tr ...                                                           */
;*---------------------------------------------------------------------*/
(markup-writer 'tr
   :options '()
   :action (lambda (n e)
              (if (not (is-markup? (ast-parent n) 'table))
                  (skribe-type-error 'tr "Illegal parent, " (ast-parent n)
                                     "#<table>"))

	      (let* ((parent (ast-parent n))
		     (nbcols (markup-option parent '&nbcols))
		     (lhs (markup-option parent '&lhs))
		     (rhs (markup-option parent '&rhs))
		     (rules (markup-option parent :rules))
		     (collapse (eq? (markup-option parent :cellstyle) 
				    'collapse))
		     (vrules (memq rules '(cols all)))
		     (cells (markup-body n)))
		 (if (markup-option n '&tl)
		     (output (new markup
				(markup '&latex-table-hline)
				(parent n)
				(ident (markup-ident n))
				(class (markup-class n)))
			     e))
		 (if (> nbcols 0)
		     (let laap ((nbc nbcols)
				(cs cells))
			(if (null? cs)
			    (when (> nbc 1)
			       (display " & ")
			       (laap (- nbc 1) cs))
			    (let* ((c (car cs))
				   (nc (- nbc (markup-option c :colspan))))
			       (when (= nbcols nbc)
				  (cond
				     ((and lhs vrules (not collapse))
				      (markup-option-add! c '&lhs "||"))
				     ((or lhs vrules)
				      (markup-option-add! c '&lhs #\|))))
			       (when (= nc 0)
				  (cond
				     ((and rhs vrules (not collapse))
				      (markup-option-add! c '&rhs "||"))
				     ((or rhs vrules)
				      (markup-option-add! c '&rhs #\|))))
			       (when (and vrules (> nc 0) (< nc nbcols))
				  (markup-option-add! c '&rhs #\|))
			       (output c e)
			       (when (> nc 0)
				  (display " & ")
				  (laap nc (cdr cs)))))))))
   :after (lambda (n e)
	     (display "\\\\")
	     (if (markup-option n '&bl)
		 (output (new markup
			    (markup '&latex-table-hline)
			    (parent n)
			    (ident (markup-ident n))
			    (class (markup-class n)))
			 e)
		 (newline))))

;*---------------------------------------------------------------------*/
;*    tc                                                               */
;*---------------------------------------------------------------------*/
(markup-writer 'tc
   :options '(:width :align :valign :colspan)
   :action (lambda (n e)
	      (let ((id (markup-ident n))
		    (cla (markup-class n)))
		 (let* ((o0 (markup-body n))
			(o1 (if (eq? (markup-option n 'markup) 'th)
				(new markup
				   (markup '&latex-th)
				   (parent n)
				   (ident id)
				   (class cla)
				   (options (markup-options n))
				   (body o0))
				o0))
			(o2 (if (markup-option n :width)
				(new markup
				   (markup '&latex-tc-parbox)
				   (parent n)
				   (ident id)
				   (class cla)
				   (options (markup-options n))
				   (body o1))
				o1))
			(o3 (if (or (> (markup-option n :colspan) 1)
				    (not (eq? (markup-option n :align) 
					      'center))
				    (markup-option n '&lhs)
				    (markup-option n '&rhs))
				(new markup
				   (markup '&latex-tc-multicolumn)
				   (parent n)
				   (ident id)
				   (class cla)
				   (options (markup-options n))
				   (body o2))
				o2)))
		    (output o3 e)))))

;*---------------------------------------------------------------------*/
;*    &latex-th ...                                                    */
;*---------------------------------------------------------------------*/
(markup-writer '&latex-th
   :before "\\textsf{"
   :after "}")

;*---------------------------------------------------------------------*/
;*    &latex-tc-parbox ...                                             */
;*---------------------------------------------------------------------*/
(markup-writer '&latex-tc-parbox
   :before (lambda (n e)
	      (let ((width (markup-option n :width)))
		 (format #t "\\parbox{~a}{" (latex-width width))))
   :after "}")
		 
;*---------------------------------------------------------------------*/
;*    &latex-tc-multicolumn ...                                        */
;*---------------------------------------------------------------------*/
(markup-writer '&latex-tc-multicolumn
   :before (lambda (n e)
	      (let ((colspan (markup-option n :colspan))
		    (lhs (or (markup-option n '&lhs) ""))
		    (rhs (or (markup-option n '&rhs) ""))
		    (align (case (markup-option n :align)
			      ((left) #\l)
			      ((center) #\c)
			      ((right) #\r)
			      (else #\c))))
		 (format #t "\\multicolumn{~a}{~a~a~a}{" colspan lhs align rhs)))
   :after "}")

;*---------------------------------------------------------------------*/
;*    image ... @label image@                                          */
;*---------------------------------------------------------------------*/
(markup-writer 'image
   :options '(:file :url :width :height :zoom)
   :action (lambda (n e)
	      (let* ((file (markup-option n :file))
		     (url (markup-option n :url))
		     (width (markup-option n :width))
		     (height (markup-option n :height))
		     (zoom (markup-option n :zoom))
		     (efmt (engine-custom e 'image-format))
		     (img (or url (convert-image file 
						 (if (list? efmt) 
						     efmt
						     '("eps"))))))
		 (if (not (string? img))
		     (skribe-error 'latex "Illegal image" file)
		     (begin
			(format #t "\\epsfig{file=~a" (strip-ref-base img))
			(if width (format #t ", width=~a" (latex-width width)))
			(if height (format #t ", height=~apt" height))
			(if zoom (format #t ", zoom=\"~a\"" zoom))
			(display "}"))))))

;*---------------------------------------------------------------------*/
;*    Ornaments ...                                                    */
;*---------------------------------------------------------------------*/
(markup-writer 'roman :before "{\\textrm{" :after "}}")
(markup-writer 'bold :before "{\\textbf{" :after "}}")
(markup-writer 'underline :before  "{\\underline{" :after "}}")
(markup-writer 'emph :before "{\\em{" :after "}}")
(markup-writer 'it :before "{\\textit{" :after "}}")
(markup-writer 'code :before "{\\texttt{" :after "}}")
(markup-writer 'var :before "{\\texttt{" :after "}}")
(markup-writer 'sc :before "{\\sc{" :after "}}")
(markup-writer 'sf :before "{\\sf{" :after "}}")
(markup-writer 'sub :before "\\begin{math}\\sb{\\mbox{" :after "}}\\end{math}")
(markup-writer 'sup :before "\\begin{math}\\sp{\\mbox{" :after "}}\\end{math}")

(markup-writer 'tt
   :before "{\\texttt{"
   :action (lambda (n e)
	      (let ((ne (make-engine
			   (gensym "latex")
			   :delegate e
			   :filter (make-string-replace latex-tt-encoding)
			   :custom (engine-customs e)
			   :symbol-table (engine-symbol-table e))))
		 (output (markup-body n) ne)))
   :after "}}")

;*---------------------------------------------------------------------*/
;*    q ... @label q@                                                  */
;*---------------------------------------------------------------------*/
(markup-writer 'q
   :before "``"
   :after "''")

;*---------------------------------------------------------------------*/
;*    mailto ... @label mailto@                                        */
;*---------------------------------------------------------------------*/
(markup-writer 'mailto
   :options '(:text)
   :before "{\\texttt{"
   :action (lambda (n e)
	      (let ((text (markup-option n :text)))
		 (output (or text (markup-body n)) e)))
   :after "}}")

;*---------------------------------------------------------------------*/
;*    mark ... @label mark@                                            */
;*---------------------------------------------------------------------*/
(markup-writer 'mark
   :before (lambda (n e)
	      (format #t "\\label{~a}" (string-canonicalize (markup-ident n)))))

;*---------------------------------------------------------------------*/
;*    ref ... @label ref@                                              */
;*---------------------------------------------------------------------*/
(markup-writer 'ref
   :options '(:text :chapter :section :subsection :subsubsection :figure :mark :handle :page)
   :action (lambda (n e)
	      (let ((t (markup-option n :text)))
		 (if t
                     (let* ((c (handle-ast (markup-body n)))
                            (i (markup-ident c))
                            (hyper? (engine-custom e 'hyperref)))
                       (if (and hyper? i)
                           (format #t "\\hyperref[~a]{" i))
                       (output t e)
                       (if (and hyper? i)
                           (display "}"))))))
   :after (lambda (n e)
	     (let* ((c (handle-ast (markup-body n)))
		    (id (markup-ident c)))
		(cond ((markup-option n :page)
                       (format #t "~~\\begin{math}{\\pageref{~a}}\\end{math}"
                               (string-canonicalize id)))
                      ((markup-option n :text)
                       #t)
                      (else
                       (format #t "\\ref{~a}"
                               (string-canonicalize id)))))))

;*---------------------------------------------------------------------*/
;*    bib-ref ...                                                      */
;*---------------------------------------------------------------------*/
(markup-writer 'bib-ref
   :options '(:text :bib)
   :before "["
   :action (lambda (n e)
	      (output (markup-option (handle-ast (markup-body n)) :title) e))
   :after "]")

;*---------------------------------------------------------------------*/
;*    bib-ref+ ...                                                     */
;*---------------------------------------------------------------------*/
(markup-writer 'bib-ref+
   :options '(:text :bib)
   :before "["
   :action (lambda (n e) 
	      (let loop ((rs (markup-body n)))
		 (cond
		    ((null? rs)
		     #f)
		    (else
		     (if (is-markup? (car rs) 'bib-ref)
			 (invoke (writer-action (markup-writer-get 'bib-ref e))
				 (car rs)
				 e)
			 (output (car rs) e))
		     (if (pair? (cdr rs))
			 (begin
			    (display ",")
			    (loop (cdr rs))))))))
   :after "]")

;*---------------------------------------------------------------------*/
;*    url-ref ...                                                      */
;*---------------------------------------------------------------------*/
(markup-writer 'url-ref
   :options '(:url :text)
   :action (lambda (n e) 
	      (let ((text (markup-option n :text))
		    (url (markup-option n :url)))
		 (if (not text)
		     (output url e)
		     (output text e)))))

;*---------------------------------------------------------------------*/
;*    url-ref hyperref ...                                             */
;*---------------------------------------------------------------------*/
(markup-writer 'url-ref
   :options '(:url :text)
   :predicate (lambda (n e)
		 (engine-custom e 'hyperref))
   :action (lambda (n e) 
	      (let ((body (markup-option n :text))
		    (url (markup-option n :url)))
		 (if (and body (not (equal? body url)))
		     (begin
			(display "\\href{")
			(display url)
			(display "}{")
			(output body e)
			(display "}"))
		     (begin
			(display "\\href{")
			(display url)
			(format #t "}{~a}" url))))))

;*---------------------------------------------------------------------*/
;*    line-ref ...                                                     */
;*---------------------------------------------------------------------*/
(markup-writer 'line-ref
   :options '(:offset)
   :before "{\\textit{"
   :action (lambda (n e)
	      (let ((o (markup-option n :offset))
		    (v (string->number (markup-option n :text))))
		 (cond
		    ((and (number? o) (number? v))
		     (display (+ o v)))
		    (else
		     (display v)))))
   :after "}}")

;*---------------------------------------------------------------------*/
;*    &the-bibliography ...                                            */
;*---------------------------------------------------------------------*/
(markup-writer '&the-bibliography
   :before (lambda (n e)
	      (display "{%
\\sloppy
\\sfcode`\\.=1000\\relax
\\newdimen\\bibindent
\\bibindent=0em
\\begin{list}{}{%
        \\settowidth\\labelwidth{[21]}%
        \\leftmargin\\labelwidth
        \\advance\\leftmargin\\labelsep
        \\advance\\leftmargin\\bibindent
        \\itemindent -\\bibindent
        \\listparindent \\itemindent
        \\itemsep 0pt
    }%\n"))
   :after (lambda (n e)
	     (display "\n\\end{list}}\n")))

;*---------------------------------------------------------------------*/
;*    &bib-entry ...                                                   */
;*---------------------------------------------------------------------*/
(markup-writer '&bib-entry
   :options '(:title)
   :action (lambda (n e)
	      (output n e (markup-writer-get '&bib-entry-label e))
	      (output n e (markup-writer-get '&bib-entry-body e)))
   :after "\n")

;*---------------------------------------------------------------------*/
;*    &bib-entry-title ...                                             */
;*---------------------------------------------------------------------*/
(markup-writer '&bib-entry-title
   :predicate (lambda (n e)
		 (engine-custom e 'hyperref))
   :action (lambda (n e)
	      (let* ((t (markup-body n))
		     (en (handle-ast (ast-parent n)))
		     (url (markup-option en 'url))
		     (ht (if url (ref :url (markup-body url) :text t) t)))
		 (evaluate-document ht e))))

;*---------------------------------------------------------------------*/
;*    &bib-entry-label ...                                             */
;*---------------------------------------------------------------------*/
(markup-writer '&bib-entry-label
   :options '(:title)
   :before "\\item[{\\char91}"
   :action (lambda (n e) (output (markup-option n :title) e))
   :after "{\\char93}] ")

;*---------------------------------------------------------------------*/
;*    &bib-entry-url ...                                               */
;*---------------------------------------------------------------------*/
(markup-writer '&bib-entry-url
   :action (lambda (n e)
	      (let* ((en (handle-ast (ast-parent n)))
		     (url (markup-option en 'url))
		     (t (it (markup-body url))))
		 (evaluate-document (ref :url (markup-body url) :text t) e))))

;*---------------------------------------------------------------------*/
;*    &source-comment ...                                              */
;*---------------------------------------------------------------------*/
(markup-writer '&source-comment
   :action (lambda (n e)
	      (let* ((cc (engine-custom e 'source-comment-color))
		     (n1 (it (markup-body n)))
		     (n2 (if (and (engine-custom e 'source-color) cc)
			     (color :fg cc n1)
			     n1)))
		 (evaluate-document n2 e))))
	      
;*---------------------------------------------------------------------*/
;*    &source-line-comment ...                                         */
;*---------------------------------------------------------------------*/
(markup-writer '&source-line-comment
   :action (lambda (n e)
	      (let* ((cc (engine-custom e 'source-comment-color))
		     (n1 (bold (markup-body n)))
		     (n2 (if (and (engine-custom e 'source-color) cc)
			     (color :fg cc n1)
			     n1)))
		 (evaluate-document n2 e))))
	      
;*---------------------------------------------------------------------*/
;*    &source-keyword ...                                              */
;*---------------------------------------------------------------------*/
(markup-writer '&source-keyword
   :action (lambda (n e)
	      (evaluate-document (underline (markup-body n)) e)))

;*---------------------------------------------------------------------*/
;*    &source-error ...                                                */
;*---------------------------------------------------------------------*/
(markup-writer '&source-error
   :action (lambda (n e)
	      (let* ((cc (engine-custom e 'source-error-color))
		     (n1 (bold (markup-body n)))
		     (n2 (if (and (engine-custom e 'error-color) cc)
			     (color :fg cc (underline n1))
			     (underline n1))))
		 (evaluate-document n2 e))))

;*---------------------------------------------------------------------*/
;*    &source-define ...                                               */
;*---------------------------------------------------------------------*/
(markup-writer '&source-define
   :action (lambda (n e)
	      (let* ((cc (engine-custom e 'source-define-color))
		     (n1 (bold (markup-body n)))
		     (n2 (if (and (engine-custom e 'source-color) cc)
			     (color :fg cc n1)
			     n1)))
		 (evaluate-document n2 e))))

;*---------------------------------------------------------------------*/
;*    &source-module ...                                               */
;*---------------------------------------------------------------------*/
(markup-writer '&source-module
   :action (lambda (n e)
	      (let* ((cc (engine-custom e 'source-module-color))
		     (n1 (bold (markup-body n)))
		     (n2 (if (and (engine-custom e 'source-color) cc)
			     (color :fg cc n1)
			     n1)))
		 (evaluate-document n2 e))))

;*---------------------------------------------------------------------*/
;*    &source-markup ...                                               */
;*---------------------------------------------------------------------*/
(markup-writer '&source-markup
   :action (lambda (n e)
	      (let* ((cc (engine-custom e 'source-markup-color))
		     (n1 (bold (markup-body n)))
		     (n2 (if (and (engine-custom e 'source-color) cc)
			     (color :fg cc n1)
			     n1)))
		 (evaluate-document n2 e))))

;*---------------------------------------------------------------------*/
;*    &source-thread ...                                               */
;*---------------------------------------------------------------------*/
(markup-writer '&source-thread
   :action (lambda (n e)
	      (let* ((cc (engine-custom e 'source-thread-color))
		     (n1 (bold (markup-body n)))
		     (n2 (if (and (engine-custom e 'source-color) cc)
			     (color :fg cc n1)
			     n1)))
		 (evaluate-document n2 e))))

;*---------------------------------------------------------------------*/
;*    &source-string ...                                               */
;*---------------------------------------------------------------------*/
(markup-writer '&source-string
   :action (lambda (n e)
	      (let* ((cc (engine-custom e 'source-string-color))
		     (n1 (markup-body n))
		     (n2 (if (and (engine-custom e 'source-color) cc)
			     (color :fg cc n1)
			     n1)))
		 (evaluate-document n2 e))))

;*---------------------------------------------------------------------*/
;*    &source-bracket ...                                              */
;*---------------------------------------------------------------------*/
(markup-writer '&source-bracket
   :action (lambda (n e)
	      (let* ((cc (engine-custom e 'source-bracket-color))
		     (n1 (markup-body n))
		     (n2 (if (and (engine-custom e 'source-color) cc)
			     (color :fg cc (bold n1))
			     (it n1))))
		 (evaluate-document n2 e))))

;*---------------------------------------------------------------------*/
;*    &source-type ...                                                 */
;*---------------------------------------------------------------------*/
(markup-writer '&source-type
   :action (lambda (n e)
	      (let* ((cc (engine-custom e 'source-type-color))
		     (n1 (markup-body n))
		     (n2 (if (and (engine-custom e 'source-color) cc)
			     (color :fg cc n1)
			     (it n1))))
		 (evaluate-document n2 e))))

;*---------------------------------------------------------------------*/
;*    &source-key ...                                                  */
;*---------------------------------------------------------------------*/
(markup-writer '&source-key
   :action (lambda (n e)
	      (let* ((cc (engine-custom e 'source-type-color))
		     (n1 (markup-body n))
		     (n2 (if (and (engine-custom e 'source-color) cc)
			     (color :fg cc (bold n1))
			     (it n1))))
		 (evaluate-document n2 e))))

;*---------------------------------------------------------------------*/
;*    &source-type ...                                                 */
;*---------------------------------------------------------------------*/
(markup-writer '&source-type
   :action (lambda (n e)
	      (let* ((cc (engine-custom e 'source-type-color))
		     (n1 (markup-body n))
		     (n2 (if (and (engine-custom e 'source-color) cc)
			     (color :fg "red" (bold n1))
			     (bold n1))))
		 (evaluate-document n2 e))))

;*---------------------------------------------------------------------*/
;*    Restore the base engine                                          */
;*---------------------------------------------------------------------*/
(default-engine-set! (find-engine 'base))


;;; Local Variables:
;;; coding: latin-1
;;; End:
