;;; html.scm  --  HTML engine.
;;;
;;; Copyright 2005, 2006, 2007, 2008, 2009  Ludovic Courtès <ludo@gnu.org>
;;; Copyright 2003, 2004  Manuel Serrano
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

(define-module (skribilo engine html)
  :use-module (skribilo lib)
  :use-module (skribilo ast)
  :use-module (skribilo config)
  :use-module (skribilo engine)
  :use-module (skribilo writer)
  :use-module (skribilo location)
  :use-module (skribilo utils strings)
  :use-module (skribilo utils syntax)
  :use-module (skribilo package base)
  :autoload   (skribilo utils images)  (convert-image)
  :autoload   (skribilo utils files)   (file-prefix file-suffix)
  :autoload   (skribilo parameters)    (*destination-file*)
  :autoload   (skribilo evaluator)     (evaluate-document)
  :autoload   (skribilo output)        (output)
  :autoload   (skribilo debug)         (*debug*)
  :autoload   (skribilo sui)           (document-sui)
  :autoload   (ice-9 rdelim)           (read-line)
  :autoload   (ice-9 regex)            (regexp-substitute/global)

  :use-module (srfi srfi-1)
  :use-module (srfi srfi-13)
  :use-module (srfi srfi-14)
  :use-module (srfi srfi-39)

  :export (html-engine html-title-engine html-file
           html-width html-class html-markup-class
           html-title-authors))

(skribilo-module-syntax)



;; Keep a reference to the base engine.
(define base-engine (find-engine 'base))

(if (not (engine? base-engine))
    (error "bootstrap problem: base engine broken" base-engine))

(define unspecified?
  ;; XXX: Hack to recognize the unspecified value as understood by
  ;; `engine-custom' et al.
  (let ((really-unspecified? unspecified?))
    (lambda (x)
      (or (really-unspecified? x)
          (eq? x 'unspecified)))))

;*---------------------------------------------------------------------*/
;*    html-file-default ...                                            */
;*---------------------------------------------------------------------*/
(define html-file-default
   ;; Default implementation of the `file-name-proc' custom.
   (let ((table '())
	 (filename (gensym "filename")))
      (define (get-file-name base suf)
	(let* ((c (assoc base table))
	       (n (if (pair? c)
		      (let ((n (+ 1 (cdr c))))
			 (set-cdr! c n)
			 n)
		      (begin
			 (set! table (cons (cons base 1) table))
			 1))))
	   (format #f "~a-~a.~a" base n suf)))
      (lambda (node e)
	(let ((f (markup-option node filename))
	      (file (markup-option node :file)))
	   (cond
	      ((string? f)
	       f)
	      ((string? file)
	       file)
	      ((or file
		   (and (is-markup? node 'chapter)
			(engine-custom e 'chapter-file))
		   (and (is-markup? node 'section)
			(engine-custom e 'section-file))
		   (and (is-markup? node 'subsection)
			(engine-custom e 'subsection-file))
		   (and (is-markup? node 'subsubsection)
			(engine-custom e 'subsubsection-file)))
	       (let* ((b (or (and (string? (*destination-file*))
				  (file-prefix (*destination-file*)))
			     ""))
		      (s (or (and (string? (*destination-file*))
				  (file-suffix (*destination-file*)))
			     "html"))
		      (nm (get-file-name b s)))
		  (markup-option-add! node filename nm)
		  nm))
	      ((document? node)
	       (*destination-file*))
	      (else
	       (let ((p (ast-parent node)))
		  (if (container? p)
		      (let ((file (html-file p e)))
			 (if (string? file)
			     (begin
				(markup-option-add! node filename file)
				file)
			     #f))
		      #f))))))))

;*---------------------------------------------------------------------*/
;*    html-engine ...                                                  */
;*---------------------------------------------------------------------*/
(define html-engine
   ;; setup the html engine
   (default-engine-set!
      (make-engine 'html
	 :version 1.0
	 :format "html"
	 :delegate (find-engine 'base)
	 :filter (make-string-replace '((#\< "&lt;")
					(#\> "&gt;")
					(#\& "&amp;")
					(#\" "&quot;")
					(#\@ "&#x40;")))
	 :custom `(;; the icon associated with the URL
		   (favicon #f)
		   ;; charset used
		   (charset "ISO-8859-1")
		   ;; enable/disable Javascript
		   (javascript #f)
		   ;; user html head
		   (head #f)
		   ;; user CSS
		   (css ())
		   ;; user inlined CSS
		   (inline-css ())
		   ;; user JS
		   (js ())
		   ;; emit-sui
		   (emit-sui #f)
		   ;; the body
		   (background #f)
		   (foreground #f)
		   ;; the margins
		   (margin-padding 3)
		   (left-margin #f)
		   (chapter-left-margin #f)
		   (section-left-margin #f)
		   (left-margin-font #f)
		   (left-margin-size 17.)
		   (left-margin-background #f)
		   (left-margin-foreground #f)
		   (right-margin #f)
		   (chapter-right-margin #f)
		   (section-right-margin #f)
		   (right-margin-font #f)
		   (right-margin-size 17.)
		   (right-margin-background #f)
		   (right-margin-foreground #f)
		   ;; author configuration
		   (author-font #f)
		   ;; title configuration
		   (title-font #f)
		   (title-background #f)
		   (title-foreground #f)
		   (file-title-separator ,(! " &#8212; ")) ;; an "em dash"
		   ;; html file naming
		   (file-name-proc ,html-file-default)
		   ;; index configuration
		   (index-header-font-size #f) ;; +2.
		   ;; chapter configuration
		   (chapter-number->string number->string)
		   (chapter-file #f)
		   ;; section configuration
		   (section-title-start "<h3>")
		   (section-title-stop "</h3>")
		   (section-title-background #f)
		   (section-title-foreground #f)
		   (section-title-number-separator " ")
		   (section-number->string number->string)
		   (section-file #f)
		   ;; subsection configuration
		   (subsection-title-start "<h3>")
		   (subsection-title-stop "</h3>")
		   (subsection-title-background #f)
		   (subsection-title-foreground #f)
		   (subsection-title-number-separator " ")
		   (subsection-number->string number->string)
		   (subsection-file #f)
		   ;; subsubsection configuration
		   (subsubsection-title-start "<h4>")
		   (subsubsection-title-stop "</h4>")
		   (subsubsection-title-background #f)
		   (subsubsection-title-foreground #f)
		   (subsubsection-title-number-separator " ")
		   (subsubsection-number->string number->string)
		   (subsubsection-file #f)
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
		   ;; image
		   (image-format ("png" "gif" "jpg" "jpeg")))
	 :symbol-table '(("iexcl" "&#161;")
			 ("cent" "&#162;")
			 ("pound" "&#163;")
			 ("currency" "&#164;")
			 ("yen" "&#165;")
			 ("section" "&#167;")
			 ("mul" "&#168;")
			 ("copyright" "&#169;")
			 ("female" "&#170;")
			 ("lguillemet" "&#171;")
			 ("not" "&#172;")
			 ("registered" "&#174;")
			 ("degree" "&#176;")
			 ("plusminus" "&#177;")
			 ("micro" "&#181;")
			 ("paragraph" "&#182;")
			 ("middot" "&#183;")
			 ("male" "&#184;")
			 ("rguillemet" "&#187;")
			 ("1/4" "&#188;")
			 ("1/2" "&#189;")
			 ("3/4" "&#190;")
			 ("iquestion" "&#191;")
			 ("Agrave" "&#192;")
			 ("Aacute" "&#193;")
			 ("Acircumflex" "&#194;")
			 ("Atilde" "&#195;")
			 ("Amul" "&#196;")
			 ("Aring" "&#197;")
			 ("AEligature" "&#198;")
			 ("Oeligature" "&#338;")
			 ("Ccedilla" "&#199;")
			 ("Egrave" "&#200;")
			 ("Eacute" "&#201;")
			 ("Ecircumflex" "&#202;")
			 ("Euml" "&#203;")
			 ("Igrave" "&#204;")
			 ("Iacute" "&#205;")
			 ("Icircumflex" "&#206;")
			 ("Iuml" "&#207;")
			 ("ETH" "&#208;")
			 ("Ntilde" "&#209;")
			 ("Ograve" "&#210;")
			 ("Oacute" "&#211;")
			 ("Ocurcumflex" "&#212;")
			 ("Otilde" "&#213;")
			 ("Ouml" "&#214;")
			 ("times" "&#215;")
			 ("Oslash" "&#216;")
			 ("Ugrave" "&#217;")
			 ("Uacute" "&#218;")
			 ("Ucircumflex" "&#219;")
			 ("Uuml" "&#220;")
			 ("Yacute" "&#221;")
			 ("THORN" "&#222;")
			 ("szlig" "&#223;")
			 ("agrave" "&#224;")
			 ("aacute" "&#225;")
			 ("acircumflex" "&#226;")
			 ("atilde" "&#227;")
			 ("amul" "&#228;")
			 ("aring" "&#229;")
			 ("aeligature" "&#230;")
			 ("oeligature" "&#339;")
			 ("ccedilla" "&#231;")
			 ("egrave" "&#232;")
			 ("eacute" "&#233;")
			 ("ecircumflex" "&#234;")
			 ("euml" "&#235;")
			 ("igrave" "&#236;")
			 ("iacute" "&#237;")
			 ("icircumflex" "&#238;")
			 ("iuml" "&#239;")
			 ("eth" "&#240;")
			 ("ntilde" "&#241;")
			 ("ograve" "&#242;")
			 ("oacute" "&#243;")
			 ("ocurcumflex" "&#244;")
			 ("otilde" "&#245;")
			 ("ouml" "&#246;")
			 ("divide" "&#247;")
			 ("oslash" "&#248;")
			 ("ugrave" "&#249;")
			 ("uacute" "&#250;")
			 ("ucircumflex" "&#251;")
			 ("uuml" "&#252;")
			 ("yacute" "&#253;")
			 ("thorn" "&#254;")
			 ("ymul" "&#255;")
			 ;; Greek
			 ("Alpha" "&#913;")
			 ("Beta" "&#914;")
			 ("Gamma" "&#915;")
			 ("Delta" "&#916;")
			 ("Epsilon" "&#917;")
			 ("Zeta" "&#918;")
			 ("Eta" "&#919;")
			 ("Theta" "&#920;")
			 ("Iota" "&#921;")
			 ("Kappa" "&#922;")
			 ("Lambda" "&#923;")
			 ("Mu" "&#924;")
			 ("Nu" "&#925;")
			 ("Xi" "&#926;")
			 ("Omicron" "&#927;")
			 ("Pi" "&#928;")
			 ("Rho" "&#929;")
			 ("Sigma" "&#931;")
			 ("Tau" "&#932;")
			 ("Upsilon" "&#933;")
			 ("Phi" "&#934;")
			 ("Chi" "&#935;")
			 ("Psi" "&#936;")
			 ("Omega" "&#937;")
			 ("alpha" "&#945;")
			 ("beta" "&#946;")
			 ("gamma" "&#947;")
			 ("delta" "&#948;")
			 ("epsilon" "&#949;")
			 ("zeta" "&#950;")
			 ("eta" "&#951;")
			 ("theta" "&#952;")
			 ("iota" "&#953;")
			 ("kappa" "&#954;")
			 ("lambda" "&#955;")
			 ("mu" "&#956;")
			 ("nu" "&#957;")
			 ("xi" "&#958;")
			 ("omicron" "&#959;")
			 ("pi" "&#960;")
			 ("rho" "&#961;")
			 ("sigmaf" "&#962;")
			 ("sigma" "&#963;")
			 ("tau" "&#964;")
			 ("upsilon" "&#965;")
			 ("phi" "&#966;")
			 ("chi" "&#967;")
			 ("psi" "&#968;")
			 ("omega" "&#969;")
			 ("thetasym" "&#977;")
			 ("piv" "&#982;")
			 ;; punctuation
			 ("bullet" "&#8226;")
			 ("ellipsis" "&#8230;")
			 ("weierp" "&#8472;")
			 ("image" "&#8465;")
			 ("real" "&#8476;")
			 ("tm" "&#8482;")
			 ("alef" "&#8501;")
			 ("<-" "&#8592;")
			 ("<--" "&#8592;")
			 ("uparrow" "&#8593;")
			 ("->" "&#8594;")
			 ("-->" "&#8594;")
			 ("downarrow" "&#8595;")
			 ("<->" "&#8596;")
			 ("<-->" "&#8596;")
			 ("<+" "&#8629;")
			 ("<=" "&#8656;")
			 ("<==" "&#8656;")
			 ("Uparrow" "&#8657;")
			 ("=>" "&#8658;")
			 ("==>" "&#8658;")
			 ("Downarrow" "&#8659;")
			 ("<=>" "&#8660;")
			 ("<==>" "&#8660;")
			 ;; Mathematical operators
			 ("forall" "&#8704;")
			 ("partial" "&#8706;")
			 ("exists" "&#8707;")
			 ("emptyset" "&#8709;")
			 ("infinity" "&#8734;")
			 ("nabla" "&#8711;")
			 ("in" "&#8712;")
			 ("notin" "&#8713;")
			 ("ni" "&#8715;")
			 ("prod" "&#8719;")
			 ("sum" "&#8721;")
			 ("asterisk" "&#8727;")
			 ("sqrt" "&#8730;")
			 ("propto" "&#8733;")
			 ("angle" "&#8736;")
			 ("and" "&#8743;")
			 ("or" "&#8744;")
			 ("cap" "&#8745;")
			 ("cup" "&#8746;")
			 ("integral" "&#8747;")
			 ("therefore" "&#8756;")
			 ("models" "|=")
			 ("vdash" "|-")
			 ("dashv" "-|")
			 ("sim" "&#8764;")
			 ("cong" "&#8773;")
			 ("approx" "&#8776;")
			 ("neq" "&#8800;")
			 ("equiv" "&#8801;")
			 ("le" "&#8804;")
			 ("ge" "&#8805;")
			 ("subset" "&#8834;")
			 ("supset" "&#8835;")
			 ("nsupset" "&#8835;")
			 ("subseteq" "&#8838;")
			 ("supseteq" "&#8839;")
			 ("oplus" "&#8853;")
			 ("otimes" "&#8855;")
			 ("perp" "&#8869;")
			 ("mid" "|")
			 ("lceil" "&#8968;")
			 ("rceil" "&#8969;")
			 ("lfloor" "&#8970;")
			 ("rfloor" "&#8971;")
			 ("langle" "&#9001;")
			 ("rangle" "&#9002;")
			 ;; Misc
			 ("loz" "&#9674;")
			 ("spades" "&#9824;")
			 ("clubs" "&#9827;")
			 ("hearts" "&#9829;")
			 ("diams" "&#9830;")
			 ("euro" "&#8464;")
			 ;; LaTeX
			 ("dag" "dag")
			 ("ddag" "ddag")
			 ("circ" "o")
			 ("top" "T")
			 ("bottom" "&#8869;")
			 ("lhd" "<")
			 ("rhd" ">")
			 ("parallel" "||")))))

;*---------------------------------------------------------------------*/
;*    html-file ...                                                    */
;*---------------------------------------------------------------------*/
(define (html-file n e)
  (let ((proc (or (engine-custom e 'file-name-proc) html-file-default)))
    (proc n e)))

;*---------------------------------------------------------------------*/
;*    html-title-engine ...                                            */
;*---------------------------------------------------------------------*/
(define html-title-engine
   (copy-engine 'html-title base-engine
      :filter (make-string-replace '((#\< "&lt;")
				     (#\> "&gt;")
				     (#\& "&amp;")
				     (#\" "&quot;")))))

;*---------------------------------------------------------------------*/
;*    html-browser-title ...                                           */
;*---------------------------------------------------------------------*/
(define (html-browser-title n)
   (and (markup? n)
	(or (markup-option n :html-title)
	    (if (document? n)
		(markup-option n :title)
		(html-browser-title (ast-parent n))))))


;*---------------------------------------------------------------------*/
;*    html-container-number ...                                        */
;*    -------------------------------------------------------------    */
;*    Returns a string representing the container number               */
;*---------------------------------------------------------------------*/
(define (html-container-number c e)
   (define (html-number n proc)
      (cond
	 ((string? n)
	  n)
	 ((number? n)
	  (if (procedure? proc)
	      (proc n)
	      (number->string n)))
	 (else
	  "")))
   (define (html-chapter-number c)
      (html-number (markup-option c :number)
		   (engine-custom e 'chapter-number->string)))
   (define (html-section-number c)
      (let ((p (ast-parent c))
	    (s (html-number (markup-option c :number)
			    (engine-custom e 'section-number->string))))
	 (cond
	    ((is-markup? p 'chapter)
	     (string-append (html-chapter-number p) "." s))
	    (else
	     s))))
   (define (html-subsection-number c)
      (let ((p (ast-parent c))
	    (s (html-number (markup-option c :number)
			    (engine-custom e 'subsection-number->string))))
	 (cond
	    ((is-markup? p 'section)
	     (string-append (html-section-number p) "." s))
	    (else
	     (string-append "." s)))))
   (define (html-subsubsection-number c)
      (let ((p (ast-parent c))
	    (s (html-number (markup-option c :number)
			    (engine-custom e 'subsubsection-number->string))))
	 (cond
	    ((is-markup? p 'subsection)
	     (string-append (html-subsection-number p) "." s))
	    (else
	     (string-append ".." s)))))
   (define (inner-html-container-number c)
      (html-number (markup-option c :number) #f))
   (let ((n (markup-option c :number)))
      (if (not n)
	  ""
	  (case (markup-markup c)
	     ((chapter)
	      (html-chapter-number c))
	     ((section)
	      (html-section-number c))
	     ((subsection)
	      (html-subsection-number c))
	     ((subsubsection)
	      (html-subsubsection-number c))
	     (else
	      (if (container? c)
		  (inner-html-container-number c)
		  (skribe-error 'html-container-number
				"Not a container"
				(markup-markup c))))))))


;*---------------------------------------------------------------------*/
;*    html-width ...                                                   */
;*---------------------------------------------------------------------*/
(define (html-width width)
   (cond
      ((and (integer? width) (exact? width))
       (format #f "~A" width))
      ((real? width)
       (format #f "~A%" (inexact->exact (round width))))
      ((string? width)
       width)
      (else
       (skribe-error 'html-width "bad width" width))))

;*---------------------------------------------------------------------*/
;*    html-class ...                                                   */
;*---------------------------------------------------------------------*/
(define (html-class m)
   (if (markup? m)
       (let ((c (markup-class m)))
	  (if (or (string? c) (symbol? c) (number? c))
	      (format #t " class=\"~a\"" c)))))

;*---------------------------------------------------------------------*/
;*    html-markup-class ...                                            */
;*---------------------------------------------------------------------*/
(define (html-markup-class m)
   (lambda (n e)
      (format #t "<~a" m)
      (html-class n)
      (display ">")))

;*---------------------------------------------------------------------*/
;*    html-color-spec? ...                                             */
;*---------------------------------------------------------------------*/
(define (html-color-spec? v)
   (and v
	(not (unspecified? v))
	(or (not (string? v)) (> (string-length v) 0))))

;*---------------------------------------------------------------------*/
;*    document ...                                                     */
;*---------------------------------------------------------------------*/
(markup-writer 'document
   :options '(:title :author :ending :html-title :env :keywords)

   :action (lambda (n e)
	      (let* ((id (markup-ident n))
		     (title (new markup
			       (markup '&html-document-title)
			       (parent n)
			       (ident (string-append id "-title"))
			       (class (markup-class n))
			       (options `((author ,(markup-option n :author))))
			       (body (markup-option n :title)))))

                 ;; Record the file name, for use by `html-file-default'.
                 (markup-option-add! n :file (*destination-file*))

		 (&html-generic-document n title e)))

   :after (lambda (n e)
	     (if (engine-custom e 'emit-sui)
		 (document-sui n e))))

;*---------------------------------------------------------------------*/
;*    &html-html ...                                                   */
;*---------------------------------------------------------------------*/
(markup-writer '&html-html
   :before "<!-- 95% W3C COMPLIANT, 95% CSS FREE, RAW HTML -->
<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\">
<html>\n"
   :after "</html>")

;*---------------------------------------------------------------------*/
;*    &html-head ...                                                   */
;*---------------------------------------------------------------------*/
(markup-writer '&html-head
   :before (lambda (n e)
             (display "<head>\n")
             (display "<meta http-equiv=\"Content-Type\" content=\"text/html;")
             (format #t "charset=~A\">\n" (engine-custom (find-engine 'html)
                                                      'charset)))
   :after "</head>\n\n")

;*---------------------------------------------------------------------*/
;*    &html-meta ...                                                   */
;*---------------------------------------------------------------------*/
(markup-writer '&html-meta
   :before "<meta name=\"keywords\" content=\""
   :action (lambda (n e)
             (let ((kw* (map ast->string (or (markup-body n) '()))))
               (output (keyword-list->comma-separated kw*) e)))
   :after  "\">\n")

;*---------------------------------------------------------------------*/
;*    &html-body ...                                                   */
;*---------------------------------------------------------------------*/
(markup-writer '&html-body
   :before (lambda (n e)
	      (let ((bg (engine-custom e 'background)))
		 (display "<body")
		 (html-class n)
		 (when (html-color-spec? bg) (format #t " bgcolor=\"~a\"" bg))
		 (display ">\n")))
   :after "</body>\n")

;*---------------------------------------------------------------------*/
;*    &html-page ...                                                   */
;*---------------------------------------------------------------------*/
(markup-writer '&html-page
   :action (lambda (n e)
	      (define (html-margin m fn size bg fg cla)
		 (format #t "<td align=\"left\" valign=\"top\" class=\"~a\"" cla)
		 (if size
		     (format #t " width=\"~a\"" (html-width size)))
		 (if (html-color-spec? bg)
		     (format #t " bgcolor=\"~a\">" bg)
		     (display ">"))
		 (format #t "<div class=\"~a\">\n" cla)
		 (cond
		    ((and (string? fg) (string? fn))
		     (format #t "<font color=\"~a\" \"~a\">" fg fn))
		    ((string? fg)
		     (format #t "<font color=\"~a\">" fg))
		    ((string? fn)
		     (format #t "<font \"~a\">" fn)))
		 (if (procedure? m)
		     (evaluate-document (m n e) e)
		     (output m e))
		 (if (or (string? fg) (string? fn))
		     (display "</font>"))
		 (display "</div></td>\n"))
	      (let ((body (markup-body n))
		    (lm (engine-custom e 'left-margin))
		    (lmfn (engine-custom e 'left-margin-font))
		    (lms (engine-custom e 'left-margin-size))
		    (lmbg (engine-custom e 'left-margin-background))
		    (lmfg (engine-custom e 'left-margin-foreground))
		    (rm (engine-custom e 'right-margin))
		    (rmfn (engine-custom e 'right-margin-font))
		    (rms (engine-custom e 'right-margin-size))
		    (rmbg (engine-custom e 'right-margin-background))
		    (rmfg (engine-custom e 'right-margin-foreground)))
		 (cond
		    ((and lm rm)
		     (let* ((ep (engine-custom e 'margin-padding))
			    (ac (if (number? ep) ep 0)))
			(format #t "<table cellpadding=\"~a\" cellspacing=\"0\" width=\"100%\" class=\"skribilo-margins\"><tr>\n" ac))
		     (html-margin lm lmfn lms lmbg lmfg "skribilo-left-margin")
		     (html-margin body #f #f #f #f "skribilo-body")
		     (html-margin rm rmfn rms rmbg rmfg "skribilo-right-margin")
		     (display "</tr></table>"))
		    (lm
		     (let* ((ep (engine-custom e 'margin-padding))
			    (ac (if (number? ep) ep 0)))
			(format #t "<table cellpadding=\"~a\" cellspacing=\"0\" width=\"100%\" class=\"skribilo-margins\"><tr>\n" ac))
		     (html-margin lm lmfn lms lmbg lmfg "skribilo-left-margin")
		     (html-margin body #f #f #f #f "skribilo-body")
		     (display "</tr></table>"))
		    (rm
                     (display "<table cellpadding=\"~a\" cellspacing=\"0\" width=\"100%\" class=\"skribilo-margins\"><tr>\n")
		     (html-margin body #f #f #f #f "skribilo-body")
		     (html-margin rm rmfn rms rmbg rmfg "skribilo-right-margin")
		     (display "</tr></table>"))
		    (else
		     (display "<div class=\"skribilo-body\">\n")
		     (output body e)
		     (display "</div>\n"))))))

;*---------------------------------------------------------------------*/
;*    &html-generic-header ...                                         */
;*---------------------------------------------------------------------*/
(define (&html-generic-header n e)
   (let* ((ic (engine-custom e 'favicon))
	  (id (markup-ident n)))
      (unless (string? id)
	 (skribe-error '&html-generic-header
		       (format #f "Illegal identifier `~a'" id)
		       n))
      ;; title
      (output (new markup
		 (markup '&html-header-title)
		 (parent n)
		 (ident (string-append id "-title"))
		 (class (markup-class n))
		 (body (markup-body n)))
	      e)
      ;; favicon
      (output (new markup
		 (markup '&html-header-favicon)
		 (parent n)
		 (ident (string-append id "-favicon"))
		 (body (cond
			  ((string? ic)
			   ic)
			  ((procedure? ic)
			   (ic id e))
			  (else #f))))
	      e)
      ;; style
      (output (new markup
		 (markup '&html-header-style)
		 (parent n)
		 (ident (string-append id "-style"))
		 (class (markup-class n)))
	      e)
      ;; css
      (output (new markup
		 (markup '&html-header-css)
		 (parent n)
		 (ident (string-append id "-css"))
		 (body (let ((c (engine-custom e 'css)))
			  (if (string? c)
			      (list c)
			      c))))
	      e)
      ;; javascript
      (output (new markup
		 (markup '&html-header-javascript)
		 (parent n)
		 (ident (string-append id "-javascript")))
	      e)))

(markup-writer '&html-header-title
   :before "<title>"
   :action (lambda (n e)
	      (output (markup-body n) html-title-engine))
   :after "</title>\n")

(markup-writer '&html-header-favicon
   :action (lambda (n e)
	      (let ((i (markup-body n)))
		 (when i
		    (format #t " <link rel=\"shortcut icon\" href=~s>\n" i)))))

(markup-writer '&html-header-css
   :action (lambda (n e)
	      (let ((css (markup-body n)))
		 (when (pair? css)
		    (for-each (lambda (css)
				 (format #t " <link href=~s rel=\"stylesheet\" type=\"text/css\">\n" css))
			      css)))))

(markup-writer '&html-header-style
   :before " <style type=\"text/css\">\n  <!--\n"
   :action (lambda (n e)
	      (let ((hd (engine-custom e 'head))
		    (icss (let ((ic (engine-custom e 'inline-css)))
			     (if (string? ic)
				 (list ic)
				 ic))))
		 (display "  pre { font-family: monospace }\n")
		 (display "  tt { font-family: monospace }\n")
		 (display "  code { font-family: monospace }\n")
		 (display "  p.flushright { text-align: right }\n")
		 (display "  p.flushleft { text-align: left }\n")
		 (display "  span.sc { font-variant: small-caps }\n")
		 (display "  span.sf { font-family: sans-serif }\n")
		 (display "  span.skribetitle { font-family: sans-serif; font-weight: bolder; font-size: x-large; }\n")
		 (when hd (display (format #f "  ~a\n" hd)))
		 (when (pair? icss)
		    (for-each (lambda (css)
				 (let ((p (open-input-file css)))
				    (if (not (input-port? p))
					(skribe-error
					 'html-css
					 "Can't open CSS file for input"
					 css)
					(begin
					   (let loop ((l (read-line p)))
					      (unless (eof-object? l)
						 (display l)
						 (newline)
						 (loop (read-line p))))
					   (close-input-port p)))))
			      icss))))
   :after "  -->\n </style>\n")

(markup-writer '&html-header-javascript
   :action (lambda (n e)
	      (when (engine-custom e 'javascript)
		 (display " <script language=\"JavaScript\" type=\"text/javascript\">\n")
		 (display " <!--\n")
		 (display "  function skribenospam( n, d, f ) {\n")
		 (display "    nn=n.replace( / /g , \".\" );\n" )
		 (display "    dd=d.replace( / /g , \".\" );\n" )
		 (display "    document.write( \"<a href=\\\"mailto:\" + nn + \"@\" + dd + \"\\\">\" );\n")
		 (display "    if( f ) {\n")
		 (display "      document.write( \"<tt>\" + nn + \"@\" + dd + \"</\" + \"tt><\" + \"/a>\" );\n")
		 (display "    }\n")
		 (display "  }\n")
		 (display " -->\n")
		 (display " </script>\n"))
	      (let* ((ejs (engine-custom e 'js))
		     (js (cond
			    ((string? ejs)
			     (list ejs))
			    ((list? ejs)
			     ejs)
			    (else
			     '()))))
		 (for-each (lambda (s)
			      (format #t "<script type=\"text/javascript\" src=\"~a\"></script>" s))
			   js))))


;*---------------------------------------------------------------------*/
;*    &html-header ...                                                 */
;*---------------------------------------------------------------------*/
(markup-writer '&html-document-header :action &html-generic-header)
(markup-writer '&html-chapter-header :action &html-generic-header)
(markup-writer '&html-section-header :action &html-generic-header)
(markup-writer '&html-subsection-header :action &html-generic-header)
(markup-writer '&html-subsubsection-header :action &html-generic-header)

;*---------------------------------------------------------------------*/
;*    &html-ending ...                                                 */
;*---------------------------------------------------------------------*/
(markup-writer '&html-ending
   :before "<div class=\"skribilo-ending\">"
   :action (lambda (n e)
	      (let ((body (markup-body n)))
		 (if body
		     (output body e)
		     (evaluate-document (list "(made with "
                                              (ref :text "skribilo"
                                                   :url (skribilo-url))
                                              ")")
                                        e))))
   :after "</div>\n")

;*---------------------------------------------------------------------*/
;*    &html-generic-title ...                                          */
;*---------------------------------------------------------------------*/
(define (&html-generic-title n e)
   (let* ((title (markup-body n))
	  (authors (markup-option n 'author))
	  (tbg (engine-custom e 'title-background))
	  (tfg (engine-custom e 'title-foreground))
	  (tfont (engine-custom e 'title-font)))
      (when title
	 (display "<table width=\"100%\" class=\"skribilo-title\" cellspacing=\"0\" cellpadding=\"0\"><tbody>\n<tr>")
	 (if (html-color-spec? tbg)
	     (format #t "<td align=\"center\"~A>"
                     (if (html-color-spec? tbg)
                         (string-append "bgcolor=\"" tbg "\"")
                         ""))
	     (display "<td align=\"center\">"))
	 (if (string? tfg)
	     (format #t "<font color=\"~a\">" tfg))
	 (when title
	    (if (string? tfont)
		(begin
		   (format #t "<font ~a><strong>" tfont)
		   (output title e)
		   (display "</strong></font>"))
		(begin
		   (display "<div class=\"skribilo-title\"><strong><big>")
		   (output title e)
		   (display "</big></strong></div>"))))
	 (if (not authors)
	     (display "\n")
	     (html-title-authors authors e))
	 (if (string? tfg)
	     (display "</font>"))
	 (display "</td></tr></tbody></table>\n"))))

;*---------------------------------------------------------------------*/
;*    &html-document-title ...                                         */
;*---------------------------------------------------------------------*/
(markup-writer '&html-document-title :action &html-generic-title)
(markup-writer '&html-chapter-title :action &html-generic-title)
(markup-writer '&html-section-title :action &html-generic-title)
(markup-writer '&html-subsection-title :action &html-generic-title)
(markup-writer '&html-subsubsection-title :action &html-generic-title)

;*---------------------------------------------------------------------*/
;*    &html-footnotes                                                  */
;*---------------------------------------------------------------------*/
(markup-writer '&html-footnotes
   :before (lambda (n e)
	      (let ((footnotes (markup-body n)))
		 (when (pair? footnotes)
		    (display "<div class=\"skribilo-footnote\">")
		    (display "<br><br>\n")
		    (display "<hr width='20%' size='2' align='left'>\n"))))
   :action (lambda (n e)
	      (let ((footnotes (markup-body n)))
		 (when (pair? footnotes)
		    (let loop ((fns footnotes))
		       (if (pair? fns)
			   (let ((fn (car fns)))
			      (format #t "<a name=\"footnote-~a\">"
				      (string-canonicalize
				       (container-ident fn)))
                              (format #t "<a href=\"#footnote-site-~a\">"
                                      (string-canonicalize
                                       (container-ident fn)))
			      (format #t "<sup><small>~a</small></sup></a></a> "
				      (markup-option fn :label))
			      (output (markup-body fn) e)
			      (display "\n<br>\n")
			      (loop (cdr fns)))))
		    (display "<div>")))))

;*---------------------------------------------------------------------*/
;*    html-title-authors ...                                           */
;*---------------------------------------------------------------------*/
(define (html-title-authors authors e)
   (define (html-authorsN authors cols first)
      (define (make-row authors . opt)
	 (tr (map (lambda (v)
		     (apply td :align 'center :valign 'top v opt))
		  authors)))
      (define (make-rows authors)
	 (let loop ((authors authors)
		    (rows '())
		    (row '())
		    (cnum 0))
	    (cond
	       ((null? authors)
		(reverse! (cons (make-row (reverse! row)) rows)))
	       ((= cnum cols)
		(loop authors
		      (cons (make-row (reverse! row)) rows)
		      '()
		      0))
	       (else
		(loop (cdr authors)
		      rows
		      (cons (car authors) row)
		      (+ cnum 1))))))
      (output (table :cellpadding 10
		 (if first
		     (cons (make-row (list (car authors)) :colspan cols)
			   (make-rows (cdr authors)))
		     (make-rows authors)))
	      e))
   (cond
      ((pair? authors)
       (display "<center>\n")
       (let ((len (length authors)))
	  (case len
	     ((1)
	      (output (car authors) e))
	     ((2 3)
	      (html-authorsN authors len #f))
	     ((4)
	      (html-authorsN authors 2 #f))
	     (else
	      (html-authorsN authors 3 #t))))
       (display "</center>\n"))
      (else
       (html-title-authors (list authors) e))))

;*---------------------------------------------------------------------*/
;*    author ...                                                       */
;*---------------------------------------------------------------------*/
(markup-writer 'author
   :options '(:name :title :affiliation :email :url :address :phone :photo :align)
   :before (lambda (n e)
	      (display "<table")
	      (html-class n)
	      (display "><tbody>\n"))
   :action (lambda (n e)
	      (let ((name (markup-option n :name))
		    (title (markup-option n :title))
		    (affiliation (markup-option n :affiliation))
		    (email (markup-option n :email))
		    (url (markup-option n :url))
		    (address (markup-option n :address))
		    (phone (markup-option n :phone))
		    (nfn (engine-custom e 'author-font))
		    (align (markup-option n :align)))
		 (define (row n)
		    (format #t "<tr><td align=\"~a\">" align)
		    (output n e)
		    (display "</td></tr>"))
		 ;; name
		 (format #t "<tr><td align=\"~a\">" align)
		 (if nfn (format #t "<font ~a>\n" nfn))
		 (output name e)
		 (if nfn (display "</font>\n"))
		 (display "</td></tr>")
		 ;; title
		 (if title (row title))
		 ;; affiliation
		 (if affiliation (row affiliation))
		 ;; address
		 (if (pair? address)
		     (for-each row address))
		 ;; telephone
		 (if phone (row phone))
		 ;; email
		 (if email (row email))
		 ;; url
		 (if url (row url))))
   :after "</tbody></table>")

;*---------------------------------------------------------------------*/
;*    author ...                                                       */
;*---------------------------------------------------------------------*/
(markup-writer 'author
   :options '(:name :title :affiliation :email :url :address :phone :photo :align)
   :predicate (lambda (n e) (markup-option n :photo))
   :before (lambda (n e)
	      (display "<table")
	      (html-class n)
	      (display "><tbody>\n<tr>"))
   :action (lambda (n e)
	      (let ((photo (markup-option n :photo)))
		 (display "<td>")
		 (output photo e)
		 (display "</td><td>")
		 (markup-option-add! n :photo #f)
		 (output n e)
		 (markup-option-add! n :photo photo)
		 (display "</td>")))
   :after "</tr>\n</tbody></table>")

;*---------------------------------------------------------------------*/
;*    toc ...                                                          */
;*---------------------------------------------------------------------*/
(markup-writer 'toc
   :options 'all
   :action (lambda (n e)
	      (define (col n)
		 (let loop ((i 0))
		      (if (< i n)
			  (begin
			     (display "<td></td>")
			     (loop (+ i 1))))))
	      (define (toc-entry fe level)
		 (let* ((c (car fe))
			(ch (cdr fe))
			(id (markup-ident c))
			(f (html-file c e)))
		    (unless (string? id)
		       (skribe-error 'toc
				     (format #f "illegal identifier `~a'" id)
				     c))
		    (display " <tr>")
		    ;; blank columns
		    (col level)
		    ;; number
		    (format #t "<td valign=\"top\" align=\"left\">~a</td>"
			    (html-container-number c e))
		    ;; title
		    (format #t "<td colspan=\"~a\" width=\"100%\">"
			    (- 4 level))
		    (format #t "<a href=\"~a#~a\">"
			    (if (and (*destination-file*)
				     (string=? f (*destination-file*)))
				""
				(strip-ref-base (or f (*destination-file*) "")))
			    (string-canonicalize id))
		    (output (markup-option c :title) e)
		    (display "</a></td>")
		    (display "</tr>\n")
		    ;; the children
		    (for-each (lambda (n) (toc-entry n (+ 1 level))) ch)))

	      (let* ((c (markup-option n :chapter))
		     (s (markup-option n :section))
		     (ss (markup-option n :subsection))
		     (sss (markup-option n :subsubsection))
		     (b (markup-body n))
		     (bb (if (handle? b)
			     (handle-ast b)
			     b)))
		 (if (not (container? bb))
		     (error 'toc
			    "Illegal body (container expected)"
			    (if (markup? bb)
				(markup-markup bb)
				"???"))
		     (let ((lst (find-down (lambda (x)
					     (and (markup? x)
						  (markup-option x :toc)
						  (or (and sss (is-markup? x 'subsubsection))
						      (and ss (is-markup? x 'subsection))
						      (and s (is-markup? x 'section))
						      (and c (is-markup? x 'chapter))
						      (markup-option n (symbol->keyword
									(markup-markup x))))))
					   (container-body bb))))
		       ;; avoid to produce an empty table
		       (unless (null? lst)
			  (display "<table cellspacing=\"1\" cellpadding=\"1\" width=\"100%\"")
			  (html-class n)
			  (display ">\n<tbody>\n")

			  (for-each (lambda (n) (toc-entry n 0)) lst)

			  (display "</tbody>\n</table>\n")))))))

(define (section-in-separate-file? n e)
  ;; Return true if N, a node (chapter, section, etc.), is to be put in a
  ;; separate file, according to the customs of engine E.
  (and (container? n)
       (not (document? n))
       (or (markup-option n :file)
           (let ((kind (markup-markup n)))
             (engine-custom e (string->symbol
                               (string-append (symbol->string kind)
                                              "-file")))))))

(define (section-in-current-file? n e)
  ;; Return true if N is to be output in the current file, or in the main
  ;; file.
  (and (container? n)
       (not (section-in-separate-file? n e))))

;*---------------------------------------------------------------------*/
;*    &html-generic-document ...                                       */
;*---------------------------------------------------------------------*/
(define (&html-generic-document n title e)
   (let* ((id (markup-ident n))
	  (header (new markup
		     (markup '&html-chapter-header)
		     (ident (string-append id "-header"))
		     (class (markup-class n))
		     (parent n)
		     (body (html-browser-title n))))
          (meta (new markup
                   (markup '&html-meta)
                   (ident (string-append id "-meta"))
                   (class (markup-class n))
                   (parent n)
                   (body (markup-option (ast-document n) :keywords))))
	  (head (new markup
		   (markup '&html-head)
		   (ident (string-append id "-head"))
		   (class (markup-class n))
		   (parent n)
		   (body (list header meta))))
	  (ftnote (new markup
		     (markup '&html-footnotes)
		     (ident (string-append id "-footnote"))
		     (class (markup-class n))
		     (parent n)

		     (body
                      ;; Collect the footnotes of all the sub-containers that
                      ;; are to be output in the same file.
                      (let ((subsections
                             (find-down (lambda (s)
                                          (section-in-current-file? s e))
                                        n)))
                        (reverse
                         (let loop ((subsections (cons n subsections))
                                    (footnotes   '()))
                           (cond ((pair? subsections)
                                  (fold loop footnotes subsections))
                                 ((null? subsections)
                                  footnotes)
                                 (else
                                  (container-env-get subsections
                                                     'footnote-env)))))))))
	  (page (new markup
		   (markup '&html-page)
		   (ident (string-append id "-page"))
		   (class (markup-class n))
		   (parent n)
		   (body (list (markup-body n) ftnote))))
	  (ending (new markup
		     (markup '&html-ending)
		     (ident (string-append id "-ending"))
		     (class (markup-class n))
		     (parent n)
		     (body (or (markup-option n :ending)
			       (let ((p (ast-document n)))
				  (and p (markup-option p :ending)))))))
	  (body (new markup
		   (markup '&html-body)
		   (ident (string-append id "-body"))
		   (class (markup-class n))
		   (parent n)
		   (body (list title page ending))))
	  (html (new markup
		   (markup '&html-html)
		   (ident (string-append id "-html"))
		   (class (markup-class n))
		   (parent n)
		   (body (list head body)))))
      ;; No file must be opened for documents. These files are
      ;; directly opened by Skribe
      (if (document? n)
	  (output html e)
          (parameterize ((*destination-file* (html-file n e)))
            (with-output-to-file (*destination-file*)
              (lambda ()
		(output html e)))))))

;*---------------------------------------------------------------------*/
;*    &html-generic-subdocument ...                                    */
;*---------------------------------------------------------------------*/
(define (&html-generic-subdocument n e)
   (let* ((p (ast-document n))
	  (id (markup-ident n))
	  (ti (let* ((nb (html-container-number n e))
		     (tc (markup-option n :title))
		     (ti (if (document? p)
			     (list (markup-option p :title)
				   (engine-custom e 'file-title-separator)
				   tc)
			     tc))
		     (sep (engine-custom
			     e
			     (symbol-append (markup-markup n)
					    '-title-number-separator)))
		     (nti (and tc
			       (if (and nb (not (equal? nb "")))
				   (list nb
					 (if (unspecified? sep) ". " sep)
					 ti)
				   ti))))
		 (new markup
		    (markup (symbol-append '&html- (markup-markup n) '-title))
		    (ident (string-append id "-title"))
		    (parent n)
		    (options '((author ())))
		    (body nti)))))
      (case (markup-markup n)
	 ((chapter)
	  (skribe-message "  [~s chapter: ~a]\n" (engine-ident e) id))
	 ((section)
	  (skribe-message "    [~s section: ~a]\n" (engine-ident e) id)))
      (&html-generic-document n ti e)))

;*---------------------------------------------------------------------*/
;*    chapter ... @label chapter@                                      */
;*---------------------------------------------------------------------*/
(markup-writer 'chapter
   :options '(:title :number :file :toc :html-title :env)
   :before (lambda (n e)
	      (let ((title (markup-option n :title))
		    (ident (markup-ident n)))
		 (display "<!-- ")
		 (output title html-title-engine)
		 (display " -->\n")
		 (display "<a name=\"")
		 (display (string-canonicalize ident))
		 (display "\"></a>\n")
		 (display "<center><h1")
		 (html-class n)
		 (display ">")
		 (output (html-container-number n e) e)
		 (display " ")
		 (output (markup-option n :title) e)
		 (display "</h1></center>")))
   :after "<br>")

;; This writer is invoked only for chapters rendered inside separate files!
(markup-writer 'chapter
   :options '(:title :number :file :toc :html-title :env)
   :predicate (lambda (n e)
		 (or (markup-option n :file)
		     (engine-custom e 'chapter-file)))
   :action &html-generic-subdocument)

;*---------------------------------------------------------------------*/
;*    html-section-title ...                                           */
;*---------------------------------------------------------------------*/
(define (html-section-title n e)
   (let* ((title (markup-option n :title))
	  (number (markup-option n :number))
	  (c (markup-class n))
	  (ident (markup-ident n))
	  (kind (markup-markup n))
	  (tbg (engine-custom e (symbol-append kind '-title-background)))
	  (tfg (engine-custom e (symbol-append kind '-title-foreground)))
	  (tstart (engine-custom e (symbol-append kind '-title-start)))
	  (tstop (engine-custom e (symbol-append kind '-title-stop)))
	  (nsep (engine-custom e (symbol-append kind '-title-number-separator))))
      ;; the section header
      (display "<!-- ")
      (output title html-title-engine)
      (display " -->\n")
      (display "<a name=\"")
      (display (string-canonicalize ident))
      (display "\"></a>\n")
      (if c
	  (format #t "<div class=\"~a-title\">" c)
	  (format #t "<div class=\"skribilo-~a-title\">" (markup-markup n)))
      (when (html-color-spec? tbg)
	 (display "<table width=\"100%\">")
	 (format #t "<tr><td bgcolor=\"~a\">" tbg))
      (display tstart)
      (if tfg (format #t "<font color=\"~a\">" tfg))
      (if number
	  (begin
	     (output (html-container-number n e) e)
	     (output nsep e)))
      (output title e)
      (if tfg (display "</font>\n"))
      (display tstop)
      (when (and (string? tbg) (> (string-length tbg) 0))
	 (display "</td></tr></table>\n"))
      (display "</div>")
      (display "<div")
      (html-class n)
      (display ">"))
   (newline))

;*---------------------------------------------------------------------*/
;*    section ...  @label section@                                     */
;*---------------------------------------------------------------------*/
(markup-writer 'section
   :options '(:title :html-title :number :toc :file :env)
   :before html-section-title
   :after "</div><br>\n")

;; on-file section writer
(markup-writer 'section
   :options '(:title :html-title :number :toc :file :env)
   :predicate (lambda (n e)
		 (or (markup-option n :file)
		     (engine-custom e 'section-file)))
   :action &html-generic-subdocument)

;*---------------------------------------------------------------------*/
;*    subsection ... @label subsection@                                */
;*---------------------------------------------------------------------*/
(markup-writer 'subsection
   :options '(:title :html-title :number :toc :env :file)
   :before html-section-title
   :after "</div>\n")

;; on-file subsection writer
(markup-writer 'section
   :options '(:title :html-title :number :toc :file :env)
   :predicate (lambda (n e)
		 (or (markup-option n :file)
		     (engine-custom e 'subsection-file)))
   :action &html-generic-subdocument)

;*---------------------------------------------------------------------*/
;*    subsubsection ... @label subsubsection@                          */
;*---------------------------------------------------------------------*/
(markup-writer 'subsubsection
   :options '(:title :html-title :number :toc :env :file)
   :before html-section-title
   :after "</div>\n")

;; on-file subsection writer
(markup-writer 'section
   :options '(:title :html-title :number :toc :file :env)
   :predicate (lambda (n e)
		 (or (markup-option n :file)
		     (engine-custom e 'subsubsection-file)))
   :action &html-generic-subdocument)

;*---------------------------------------------------------------------*/
;*    paragraph ...                                                    */
;*---------------------------------------------------------------------*/
(markup-writer 'paragraph
   :before (lambda (n e)
	      (when (and (>= (*debug*) 2) (location? (ast-loc n)))
		 (format #t "<span style=\"display: block; position: relative; left: -2cm; font-size: x-small; font-style: italic; color: ff8e1e;\">~a</span>"
			 (ast-loc n)))
	      ((html-markup-class "p") n e))
   :after "</p>")

;*---------------------------------------------------------------------*/
;*    ~ ...                                                            */
;*---------------------------------------------------------------------*/
(markup-writer '~
   :before "&nbsp;"
   :after #f
   :action #f)

;*---------------------------------------------------------------------*/
;*    footnote ...                                                     */
;*---------------------------------------------------------------------*/
(markup-writer 'footnote
   :options '(:label)
   :action (lambda (n e)
              (format #t "<a name=\"footnote-site-~a\">"
                      (string-canonicalize (container-ident n)))
	      (format #t "<a href=\"#footnote-~a\"><sup><small>~a</small></sup></a>"
		      (string-canonicalize (container-ident n))
		      (markup-option n :label))
              (format #t "</a>")))

;*---------------------------------------------------------------------*/
;*    linebreak ...                                                    */
;*---------------------------------------------------------------------*/
(markup-writer 'linebreak
	       :before (lambda (n e)
			  (display "<br")
			  (html-class n)
			  (display "/>")))

;*---------------------------------------------------------------------*/
;*    hrule ...                                                        */
;*---------------------------------------------------------------------*/
(markup-writer 'hrule
   :options '(:width :height)
   :before (lambda (n e)
	      (let ((width (markup-option n :width))
		    (height (markup-option n :height)))
		 (display "<hr")
		 (html-class n)
		 (if (< width 100)
		     (format #t " width=\"~a\"" (html-width width)))
		 (if (> height 1)
		     (format #t " size=\"~a\"" height))
		 (display ">"))))

;*---------------------------------------------------------------------*/
;*    color ...                                                        */
;*---------------------------------------------------------------------*/
(markup-writer 'color
   :options '(:bg :fg :width :margin)
   :before (lambda (n e)
	      (let ((m (markup-option n :margin))
		    (w (markup-option n :width))
		    (bg (markup-option n :bg))
		    (fg (markup-option n :fg)))
		 (when (html-color-spec? bg)
		    (display "<table cellspacing=\"0\"")
		    (html-class n)
		    (format #t " cellpadding=\"~a\"" (if m m 0))
		    (if w (format #t " width=\"~a\"" (html-width w)))
		    (display "><tbody>\n<tr>")
		    (display "<td bgcolor=\"")
		    (output bg e)
		    (display "\">"))
		 (when (html-color-spec? fg)
		    (display "<font color=\"")
		    (output fg e)
		    (display "\">"))))
   :after (lambda (n e)
	     (when (html-color-spec? (markup-option n :fg))
		(display "</font>"))
	     (when (html-color-spec? (markup-option n :bg))
		(display "</td></tr>\n</tbody></table>"))))

;*---------------------------------------------------------------------*/
;*    frame ...                                                        */
;*---------------------------------------------------------------------*/
(markup-writer 'frame
   :options '(:width :margin :border)
   :before (lambda (n e)
	      (let ((m (markup-option n :margin))
		    (b (markup-option n :border))
		    (w (markup-option n :width)))
		 (display "<table cellspacing=\"0\"")
		 (html-class n)
		 (format #t " cellpadding=\"~a\"" (if m m 0))
		 (format #t " border=\"~a\"" (if b b 0))
		 (if w (format #t " width=\"~a\"" (html-width w)))
		 (display "><tbody>\n<tr><td>")))
   :after "</td></tr>\n</tbody></table>")

;*---------------------------------------------------------------------*/
;*    font ...                                                         */
;*---------------------------------------------------------------------*/
(markup-writer 'font
   :options '(:size :face)
   :before (lambda (n e)
	      (let ((size (markup-option n :size))
		    (face (markup-option n :face)))
		 (when (and (number? size) (inexact? size))
		    (let ((s (if (> size 0) "<big>" "<small>"))
			  (d (if (> size 0) 1 -1)))
		       (do ((i (inexact->exact size) (- i d)))
			   ((= i 0))
			   (display s))))
		 (when (or (and (number? size) (exact? size)) face)
		    (display "<font")
		    (html-class n)
		    (when (and (number? size) (exact? size) (not (= size 0)))
		       (format #t " size=\"~a\"" size))
		    (when face (format #t " face=\"~a\"" face))
		    (display ">"))))
   :after (lambda (n e)
	     (let ((size (markup-option n :size))
		   (face (markup-option n :face)))
		(when (or (and (number? size) (exact? size) (not (= size 0)))
			  face)
		   (display "</font>"))
		(when (and (number? size) (inexact? size))
		   (let ((s (if (> size 0) "</big>" "</small>"))
			 (d (if (> size 0) 1 -1)))
		      (do ((i (inexact->exact size) (- i d)))
			  ((= i 0))
			  (display s)))))))

;*---------------------------------------------------------------------*/
;*    flush ...                                                        */
;*---------------------------------------------------------------------*/
(markup-writer 'flush
   :options '(:side)
   :before (lambda (n e)
	      (case (markup-option n :side)
		 ((center)
		  (display "<center")
		  (html-class n)
		  (display ">\n"))
		 ((left)
		  (display "<p style=\"text-align:left;\"")
		  (html-class n)
		  (display ">\n"))
		 ((right)
		  (display "<table ")
		  (html-class n)
		  (display "width=\"100%\" cellpadding=\"0\" cellspacing=\"0\" border=\"0\"><tr><td align=\"right\">"))
		 (else
		  (skribe-error 'flush
				"Illegal side"
				(markup-option n :side)))))
   :after (lambda (n e)
	     (case (markup-option n :side)
		((center)
		 (display "</center>\n"))
		((right)
		 (display "</td></tr></table>\n"))
		((left)
		 (display "</p>\n")))))

;*---------------------------------------------------------------------*/
;*    center ...                                                       */
;*---------------------------------------------------------------------*/
(markup-writer 'center
   :before (html-markup-class "center")
   :after "</center>\n")

;*---------------------------------------------------------------------*/
;*    pre ...                                                          */
;*---------------------------------------------------------------------*/
(markup-writer 'pre :before (html-markup-class "pre") :after "</pre>\n")

;*---------------------------------------------------------------------*/
;*    prog ...                                                         */
;*---------------------------------------------------------------------*/
(markup-writer 'prog
   :options '(:line :mark)
   :before (html-markup-class "pre")
   :after "</pre>\n")

;*---------------------------------------------------------------------*/
;*    itemize ...                                                      */
;*---------------------------------------------------------------------*/
(markup-writer 'itemize
   :options '(:symbol)
   :before (html-markup-class "ul")
   :action (lambda (n e)
	      (for-each (lambda (item)
			  (let ((ident (and (markup? item)
					    (markup-ident item))))
			   (display "<li")
			   (html-class item)
			   (display ">")
			    (if ident  ;; produce an anchor
				(format #t "\n<a name=\"~a\"></a>\n"
					(string-canonicalize ident)))
			   (output item e)
			    (display "</li>\n")))
			(markup-body n)))
   :after "</ul>")

;*---------------------------------------------------------------------*/
;*    enumerate ...                                                    */
;*---------------------------------------------------------------------*/
(markup-writer 'enumerate
   :options '(:symbol)
   :before (html-markup-class "ol")
   :action (lambda (n e)
	      (for-each (lambda (item)
			  (let ((ident (and (markup? item)
					    (markup-ident item))))
			   (display "<li")
			   (html-class item)
			   (display ">")
			    (if ident  ;; produce an anchor
				(format #t "\n<a name=\"~a\"></a>\n" ident))
			   (output item e)
			    (display "</li>\n")))
			(markup-body n)))
   :after "</ol>")

;*---------------------------------------------------------------------*/
;*    description ...                                                  */
;*---------------------------------------------------------------------*/
(markup-writer 'description
   :options '(:symbol)
   :before (html-markup-class "dl")
   :action (lambda (n e)
	      (for-each (lambda (item)
			   (let ((k (markup-option item :key)))
			      (for-each (lambda (i)
					   (display " <dt")
					   (html-class i)
					   (display ">")
					   (output i e)
					   (display "</dt>"))
					(if (pair? k) k (list k)))
			      (display "<dd")
			      (html-class item)
			      (display ">")
			      (output (markup-body item) e)
			      (display "</dd>\n")))
			(markup-body n)))
   :after "</dl>")

;*---------------------------------------------------------------------*/
;*    item ...                                                         */
;*---------------------------------------------------------------------*/
(markup-writer 'item
   :options '(:key)
   :action (lambda (n e)
	      (let ((k (markup-option n :key)))
		 (if k
		     (begin
			(display "<b")
			(html-class n)
			(display ">")
			(output k e)
			(display "</b> "))))
	      (output (markup-body n) e)))

;*---------------------------------------------------------------------*/
;*    blockquote ...                                                   */
;*---------------------------------------------------------------------*/
(markup-writer 'blockquote
   :options '()
   :before (lambda (n e)
	     (display "<blockquote ")
	     (html-class n)
	     (display ">\n"))
   :after "\n</blockquote>\n")

;*---------------------------------------------------------------------*/
;*    figure ... @label figure@                                        */
;*---------------------------------------------------------------------*/
(markup-writer 'figure
   :options '(:legend :number :multicolumns :legend-width)
   :before (html-markup-class "br")
   :action (lambda (n e)
	      (let ((ident (markup-ident n))
		    (number (markup-option n :number))
		    (legend (markup-option n :legend)))
		 (display "<a name=\"")
		 (display (string-canonicalize ident))
		 (display "\"></a>\n")
		 (output (markup-body n) e)
		 (display "<br>\n")
		 (output (new markup
			    (markup '&html-figure-legend)
			    (parent n)
			    (ident (string-append ident "-legend"))
			    (class (markup-class n))
			    (options `((:number ,number)))
			    (body legend))
			 e)))
   :after "<br>")

;*---------------------------------------------------------------------*/
;*    &html-figure-legend ...                                          */
;*---------------------------------------------------------------------*/
(markup-writer '&html-figure-legend
   :options '(:number)
   :before (lambda (n e)
	      (display "<center>")
	      (let ((number (markup-option n :number)))
		 (if number
		     (format #t "<strong>Fig. ~a:</strong> " number)
		     (display "<strong>Fig. :</strong> "))))
   :after "</center>")

;*---------------------------------------------------------------------*/
;*    table ...                                                        */
;*---------------------------------------------------------------------*/
(markup-writer 'table
   :options '(:border :width :frame :rules :cellstyle :cellpadding :cellspacing)
   :before (lambda (n e)
	      (let ((width (markup-option n :width))
		    (border (markup-option n :border))
		    (frame (markup-option n :frame))
		    (rules (markup-option n :rules))
		    (cstyle (markup-option n :cellstyle))
		    (cp (markup-option n :cellpadding))
		    (cs (markup-option n :cellspacing)))
		 (display "<table")
		 (html-class n)
		 (if width (format #t " width=\"~a\"" (html-width width)))
		 (if border (format #t " border=\"~a\"" border))
		 (if (and (number? cp) (>= cp 0))
		     (format #t " cellpadding=\"~a\"" cp))
		 (if (and (number? cs) (>= cs 0))
		     (format #t " cellspacing=\"~a\"" cs))
		 (cond
		    ((symbol? cstyle)
		     (format #t " style=\"border-collapse: ~a;\"" cstyle))
		    ((string? cstyle)
		     (format #t " style=\"border-collapse: separate; border-spacing=~a\"" cstyle))
		    ((number? cstyle)
		     (format #t " style=\"border-collapse: separate; border-spacing=~apt\"" cstyle)))
		 (if frame
		     (format #t " frame=\"~a\""
			     (if (eq? frame 'none) "void" frame)))
		 (if (and rules (not (eq? rules 'header)))
		     (format #t " rules=\"~a\"" rules))
		 (display "><tbody>\n")))
   :after "</tbody></table>\n")

;*---------------------------------------------------------------------*/
;*    tr ...                                                           */
;*---------------------------------------------------------------------*/
(markup-writer 'tr
   :options '(:bg)
   :before (lambda (n e)
	      (let ((bg (markup-option n :bg)))
		 (display "<tr")
		 (html-class n)
		 (when (html-color-spec? bg) (format #t " bgcolor=\"~a\"" bg))
		 (display ">")))
   :after "</tr>\n")

;*---------------------------------------------------------------------*/
;*    tc ...                                                           */
;*---------------------------------------------------------------------*/
(markup-writer 'tc
   :options '(markup :width :align :valign :colspan :rowspan :bg)
   :before (lambda (n e)
	      (let ((markup (or (markup-option n 'markup) 'td))
		    (width (markup-option n :width))
		    (align (markup-option n :align))
		    (valign (let ((v (markup-option n :valign)))
			       (cond
				  ((or (eq? v 'center)
				       (equal? v "center"))
				   "middle")
				  (else
				   v))))
		    (colspan (markup-option n :colspan))
		    (rowspan (markup-option n :rowspan))
		    (bg (markup-option n :bg)))
		 (format #t "<~a" markup)
		 (html-class n)
		 (if width (format #t " width=\"~a\"" (html-width width)))
		 (if align (format #t " align=\"~a\"" align))
		 (if valign (format #t " valign=\"~a\"" valign))
		 (if colspan (format #t " colspan=\"~a\"" colspan))
		 (if rowspan (format #t " rowspan=\"~a\"" rowspan))
		 (when (html-color-spec? bg)
		    (format #t " bgcolor=\"~a\"" bg))
		 (display ">")))
   :after (lambda (n e)
	     (let ((markup (or (markup-option n 'markup) 'td)))
		(format #t "</~a>" markup))))

;*---------------------------------------------------------------------*/
;*    image ... @label image@                                          */
;*---------------------------------------------------------------------*/
(markup-writer 'image
   :options '(:file :url :width :height)
   :action (lambda (n e)
	      (let* ((file (markup-option n :file))
		     (url (markup-option n :url))
		     (width (markup-option n :width))
		     (height (markup-option n :height))
		     (body (markup-body n))
		     (efmt (engine-custom e 'image-format))
		     (img (or url (convert-image file
						 (if (list? efmt)
						     efmt
						     '("gif" "jpg" "png"))))))
		 (if (not (string? img))
		     (skribe-error 'html "Illegal image" file)
		     (begin
			(format #t "<img src=\"~a\" border=\"0\"" img)
			(html-class n)
			(if body
			    (begin
			       (display " alt=\"")
			       (output body e)
			       (display "\""))
			    (format #t " alt=\"~a\"" file))
			(if width (format #t " width=\"~a\"" (html-width width)))
			(if height (format #t " height=\"~a\"" height))
			(display ">"))))))

;*---------------------------------------------------------------------*/
;*    Ornaments ...                                                    */
;*---------------------------------------------------------------------*/
(markup-writer 'roman :before "")
(markup-writer 'bold :before (html-markup-class "strong") :after "</strong>")
(markup-writer 'underline :before (html-markup-class "u") :after "</u>")
(markup-writer 'strike :before (html-markup-class "strike") :after "</strike>")
(markup-writer 'emph :before (html-markup-class "em") :after "</em>")
(markup-writer 'kbd :before (html-markup-class "kbd") :after "</kbd>")
(markup-writer 'it :before (html-markup-class "em") :after "</em>")
(markup-writer 'tt :before (html-markup-class "tt") :after "</tt>")
(markup-writer 'code :before (html-markup-class "code") :after "</code>")
(markup-writer 'var :before (html-markup-class "var") :after "</var>")
(markup-writer 'samp :before (html-markup-class "samp") :after "</samp>")
(markup-writer 'sc :before "<span class=\"sc\">" :after "</span>")
(markup-writer 'sf :before "<span class=\"sf\">" :after "</span>")
(markup-writer 'sub :before (html-markup-class "sub") :after "</sub>")
(markup-writer 'sup :before (html-markup-class "sup") :after "</sup>")

;*---------------------------------------------------------------------*/
;*    q ... @label q@                                                  */
;*---------------------------------------------------------------------*/
(markup-writer 'q
   :before "\""
   :after "\"")

;*---------------------------------------------------------------------*/
;*    mailto ... @label mailto@                                        */
;*---------------------------------------------------------------------*/
(markup-writer 'mailto
   :options '(:text)
   :action (lambda (n e)
	      (let ((text (markup-option n :text)))
		 (display "<a href=\"mailto:")
		 (output (markup-body n) e)
		 (display #\")
		 (html-class n)
		 (display #\>)
		 (if text
		     (output text e)
		     (evaluate-document (tt (markup-body n)) e))
		 (display "</a>"))))

;*---------------------------------------------------------------------*/
;*    mailto ... @label mailto@                                        */
;*---------------------------------------------------------------------*/
(define %non-at
  ;; Char-set not containing the `@' character.
  (char-set-complement (char-set #\@)))

(markup-writer 'mailto
   :options '(:text)
   :predicate (lambda (n e)
		 (and (engine-custom e 'javascript)
		      (or (string? (markup-body n))
			  (and (pair? (markup-body n))
			       (null? (cdr (markup-body n)))
			       (string? (car (markup-body n)))))))
   :action (lambda (n e)
	      (let* ((body (markup-body n))
		     (email (if (string? body) body (car body)))
		     (split (string-tokenize email %non-at))
		     (na (car split))
		     (do (if (pair? (cdr split)) (cadr split) ""))
		     (nn (regexp-substitute/global #f "\\." na
                                                   'pre " " 'post))
		     (dd (regexp-substitute/global #f "\\." do
                                                   'pre " " 'post))
		     (text (markup-option n :text)))
		(display "<script language=\"JavaScript\" type=\"text/javascript\"")
		(if (not text)
		    (format #t ">skribenospam( ~s, ~s, true )" nn dd)
		    (begin
		      (format #t ">skribenospam( ~s, ~s, false )" nn dd)
		      (display "</script>")
		      (output text e)
		      (display "<script language=\"JavaScript\" type=\"text/javascript\">document.write(\"</\" + \"a>\")")))
		(display "</script>\n"))))

;*---------------------------------------------------------------------*/
;*    mark ... @label mark@                                            */
;*---------------------------------------------------------------------*/
(markup-writer 'mark
   :before (lambda (n e)
	      (format #t "<a name=\"~a\"" (string-canonicalize (markup-ident n)))
	      (html-class n)
	      (display ">"))
   :after "</a>")

;*---------------------------------------------------------------------*/
;*    ref ... @label ref@                                              */
;*---------------------------------------------------------------------*/
(markup-writer 'ref
   :options '(:text :chapter :section :subsection :subsubsection :figure :mark :handle)
   :before (lambda (n e)
	      (let* ((c (handle-ast (markup-body n)))
		     (id (markup-ident c))
		     (f (html-file c e))
		     (class (if (markup-class n)
				(markup-class n)
				"skribilo-ref")))
		 (format #t "<a href=\"~a#~a\" class=\"~a\""
			 (if (and (*destination-file*) f
				  (string=? f (*destination-file*)))
			     ""
			     (strip-ref-base (or f (*destination-file*) "")))
			 (string-canonicalize id)
			 class)
		 (display ">")))
   :action (lambda (n e)
	      (let ((t (markup-option n :text))
		    (m (markup-option n 'mark))
		    (f (markup-option n :figure))
		    (c (markup-option n :chapter))
		    (s (markup-option n :section))
		    (ss (markup-option n :subsection))
		    (sss (markup-option n :subsubsection)))
		 (cond
		    (t
		     (output t e))
		    (f
		     (output (new markup
				(markup '&html-figure-ref)
				(body (markup-body n)))
			     e))
		    ((or c s ss sss)
		     (output (new markup
				(markup '&html-section-ref)
				(body (markup-body n)))
			     e))

		    ((not m)
		     (output (new markup
				(markup '&html-unmark-ref)
				(body (markup-body n)))
			     e))
		    (else
		     (display m)))))
   :after "</a>")

;*---------------------------------------------------------------------*/
;*    &html-figure-ref ...                                             */
;*---------------------------------------------------------------------*/
(markup-writer '&html-figure-ref
   :action (lambda (n e)
	      (let ((c (handle-ast (markup-body n))))
		 (if (or (not (markup? c))
			 (not (is-markup? c 'figure)))
		     (display "???")
		     (output (markup-option c :number) e)))))

;*---------------------------------------------------------------------*/
;*    &html-section-ref ...                                            */
;*---------------------------------------------------------------------*/
(markup-writer '&html-section-ref
   :action (lambda (n e)
	      (let ((c (handle-ast (markup-body n))))
		 (if (not (markup? c))
		     (display "???")
		     (output (markup-option c :title) e)))))

;*---------------------------------------------------------------------*/
;*    &html-unmark-ref ...                                             */
;*---------------------------------------------------------------------*/
(markup-writer '&html-unmark-ref
   :action (lambda (n e)
	      (let ((c (handle-ast (markup-body n))))
		 (if (not (markup? c))
		     (display "???")
		     (let ((t (markup-option c :title)))
			(if t
			    (output t e)
			    (let ((l (markup-option c :legend)))
			       (if l
				   (output t e)
				   (display
				    (string-canonicalize
				     (markup-ident c)))))))))))

;*---------------------------------------------------------------------*/
;*    bib-ref ...                                                      */
;*---------------------------------------------------------------------*/
(markup-writer 'bib-ref
   :options '(:text :bib)
   :before "["
   :action (lambda (n e)
             ;; Produce a hyperlink.
             (output n e (markup-writer-get 'ref e)))
   :after "]")

;*---------------------------------------------------------------------*/
;*    url-ref ...                                                      */
;*---------------------------------------------------------------------*/
(markup-writer 'url-ref
   :options '(:url :text)
   :before (lambda (n e)
	      (let* ((url (markup-option n :url))
		     (class (cond
			       ((markup-class n)
				(markup-class n))
			       ((not (string? url))
				#f)
			       (else
				(let ((l (string-length url)))
				   (let loop ((i 0))
				      (cond
					 ((= i l)
					  #f)
					 ((char=? (string-ref url i) #\:)
					  (substring url 0 i))
					 (else
					  (loop (+ i 1))))))))))
		 (display "<a href=\"")
		 (output url html-title-engine)
		 (display "\"")
		 (when class (format #t " class=\"~a\"" class))
		 (display ">")))
   :action (lambda (n e)
	      (let ((v (markup-option n :text)))
		 (output (or v (markup-option n :url)) e)))
   :after "</a>")


;*---------------------------------------------------------------------*/
;*    &prog-line ...                                                   */
;*---------------------------------------------------------------------*/
(markup-writer '&prog-line
   :before (lambda (n e)
             (let ((before (writer-before
                            (markup-writer-get '&prog-line base-engine))))
               (format #t "<a name=\"~a\""
                       (string-canonicalize (markup-ident n)))
               (html-class n)
               (display ">")
               (before n e)))
   :after "</a>\n")

;*---------------------------------------------------------------------*/
;*    line-ref ...                                                     */
;*---------------------------------------------------------------------*/
(markup-writer 'line-ref
   :options '(:offset)
   :before (html-markup-class "i")
   :action (lambda (n e)
	      (let ((o (markup-option n :offset))
		    (v (markup-option (handle-ast (markup-body n)) :number)))
		 (cond ((and (number? o) (number? v))
                        (markup-option-set! n :text (+ o v)))
                       ((number? v)
                        (markup-option-set! n :text v)))
		 (output n e (markup-writer-get 'ref e))
		 (if (and (number? o) (number? v))
		     (markup-option-set! n :text v))))
   :after "</i>")

;*---------------------------------------------------------------------*/
;*    page-ref ...                                                     */
;*---------------------------------------------------------------------*/
(markup-writer 'page-ref
   :options '(:mark :handle)
   :action (lambda (n e)
	      (error 'page-ref:html "Not implemented yet" n)))

;*---------------------------------------------------------------------*/
;*    &bib-entry-label ...                                             */
;*---------------------------------------------------------------------*/
(markup-writer '&bib-entry-label
   :options '(:title)
   :before (lambda (n e)
	      (format #t "<a name=\"~a\"" (string-canonicalize (markup-ident n)))
	      (html-class n)
	      (display ">"))
   :action (lambda (n e)
	      (output n e (markup-writer-get '&bib-entry-label base-engine)))
   :after "</a>")

;*---------------------------------------------------------------------*/
;*    &bib-entry-title ...                                             */
;*---------------------------------------------------------------------*/
(markup-writer '&bib-entry-title
   :action (lambda (n e)
	      (let* ((t (bold (markup-body n)))
		     (en (handle-ast (ast-parent n)))
		     (url (or (markup-option en 'url)
			      (markup-option en 'documenturl)))
		     (ht (if url (ref :url (markup-body url) :text t) t)))
		 (evaluate-document ht e))))

;*---------------------------------------------------------------------*/
;*    &bib-entry-url ...                                               */
;*---------------------------------------------------------------------*/
(markup-writer '&bib-entry-url
   :action (lambda (n e)
	      (let* ((en (handle-ast (ast-parent n)))
		     (url (markup-option en 'url))
		     (t (bold (markup-body url))))
		 (evaluate-document (ref :url (markup-body url) :text t) e))))

;*---------------------------------------------------------------------*/
;*    &the-index-header ...                                            */
;*---------------------------------------------------------------------*/
(markup-writer '&the-index-header
   :action (lambda (n e)
	      (display "<center")
	      (html-class n)
	      (display ">")
	      (for-each (lambda (h)
			   (let ((f (engine-custom e 'index-header-font-size)))
			      (if f
				  (evaluate-document (font :size f (bold (it h))) e)
				  (output h e))
			      (display " ")))
			(markup-body n))
	      (display "</center>")
	      (evaluate-document (linebreak 2) e)))

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
	      (evaluate-document (bold (markup-body n)) e)))

;*---------------------------------------------------------------------*/
;*    &source-error ...                                                */
;*---------------------------------------------------------------------*/
(markup-writer '&source-error
   :action (lambda (n e)
	      (let* ((cc (engine-custom e 'source-error-color))
		     (n1 (bold (markup-body n)))
		     (n2 (if (and (engine-custom e 'source-color) cc)
			     (color :fg cc n1)
			     n1)))
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
;*    &source-bracket ...                                               */
;*---------------------------------------------------------------------*/
(markup-writer '&source-bracket
   :action (lambda (n e)
	      (let* ((cc (engine-custom e 'source-bracket-color))
		     (n1 (markup-body n))
		     (n2 (if (and (engine-custom e 'source-color) cc)
			     (color :fg cc (bold n1))
			     (bold n1))))
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
;;; mode: scheme
;;; coding: latin-1
;;; End:
