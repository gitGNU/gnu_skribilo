;;; base.scm  --  BASE Skribe engine
;;;
;;; Copyright 2003, 2004  Manuel Serrano
;;; Copyright 2006, 2007  Ludovic Courtès <ludovic.courtes@laas.fr>
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

(define-module (skribilo engine base)
  :use-module (skribilo ast)
  :use-module (skribilo engine)
  :use-module (skribilo writer)
  :autoload   (skribilo output) (output)
  :use-module (skribilo evaluator)
  :autoload   (skribilo package base) (color it bold ref)
  :autoload   (skribilo utils keywords) (list-split)
  :autoload   (skribilo biblio template) (make-bib-entry-template/default
                                          output-bib-entry-template)
  ;; syntactic sugar
  :use-module (skribilo reader)
  :use-module (skribilo utils syntax))

(fluid-set! current-reader (make-reader 'skribe))


;*---------------------------------------------------------------------*/
;*    base-engine ...                                                  */
;*---------------------------------------------------------------------*/
(define base-engine
   (default-engine-set!
      (make-engine 'base
		   :version 'plain
		   :symbol-table '(("iexcl" "!")
				   ("cent" "c")
				   ("lguillemet" "\"")
				   ("not" "!")
				   ("registered" "(r)")
				   ("degree" "o")
				   ("plusminus" "+/-")
				   ("micro" "o")
				   ("paragraph" "p")
				   ("middot" ".")
				   ("rguillemet" "\"")
				   ("iquestion" "?")
				   ("Agrave" "À")
				   ("Aacute" "A")
				   ("Acircumflex" "Â")
				   ("Atilde" "A")
				   ("Amul" "A")
				   ("Aring" "A")
				   ("AEligature" "AE")
				   ("Oeligature" "OE")
				   ("Ccedilla" "Ç")
				   ("Egrave" "È")
				   ("Eacute" "É")
				   ("Ecircumflex" "Ê")
				   ("Euml" "E")
				   ("Igrave" "I")
				   ("Iacute" "I")
				   ("Icircumflex" "Î")
				   ("Iuml" "I")
				   ("ETH" "D")
				   ("Ntilde" "N")
				   ("Ograve" "O")
				   ("Oacute" "O")
				   ("Ocurcumflex" "O")
				   ("Otilde" "O")
				   ("Ouml" "O")
				   ("times" "x")
				   ("Oslash" "O")
				   ("Ugrave" "Ù")
				   ("Uacute" "U")
				   ("Ucircumflex" "Û")
				   ("Uuml" "Ü")
				   ("Yacute" "Y")
				   ("agrave" "à")
				   ("aacute" "a")
				   ("acircumflex" "â")
				   ("atilde" "a")
				   ("amul" "a")
				   ("aring" "a")
				   ("aeligature" "æ")
				   ("oeligature" "oe")
				   ("ccedilla" "ç")
				   ("egrave" "è")
				   ("eacute" "é")
				   ("ecircumflex" "ê")
				   ("euml" "e")
				   ("igrave" "i")
				   ("iacute" "i")
				   ("icircumflex" "î")
				   ("iuml" "i")
				   ("ntilde" "n")
				   ("ograve" "o")
				   ("oacute" "o")
				   ("ocurcumflex" "o")
				   ("otilde" "o")
				   ("ouml" "o")
				   ("divide" "/")
				   ("oslash" "o")
				   ("ugrave" "ù")
				   ("uacute" "u")
				   ("ucircumflex" "û")
				   ("uuml" "ü")
				   ("yacute" "y")
				   ("ymul" "y")
				   ;; punctuation
				   ("bullet" ".")
				   ("ellipsis" "...")
				   ("<-" "<-")
				   ("<--" "<--")
				   ("uparrow" "^;")
				   ("->" "->")
				   ("-->" "-->")
				   ("downarrow" "v")
				   ("<->" "<->")
				   ("<-->" "<-->")
				   ("<+" "<+")
				   ("<=" "<=;")
				   ("<==" "<==")
				   ("Uparrow" "^")
				   ("=>" "=>")
				   ("==>" "==>")
				   ("Downarrow" "v")
				   ("<=>" "<=>")
				   ("<==>" "<==>")
				   ;; Mathematical operators
				   ("asterisk" "*")
				   ("angle" "<")
				   ("and" "^;")
				   ("or" "v")
				   ("models" "|=")
				   ("vdash" "|-")
				   ("dashv" "-|")
				   ("sim" "~")
				   ("mid" "|")
				   ("langle" "<")
				   ("rangle" ">")
				   ;; LaTeX
				   ("circ" "o")
				   ("top" "T")
				   ("lhd" "<")
				   ("rhd" ">")
				   ("parallel" "||")))))

;*---------------------------------------------------------------------*/
;*    mark ...                                                         */
;*---------------------------------------------------------------------*/
(markup-writer 'symbol
   :action (lambda (n e)
	      (let* ((s (markup-body n))
		     (c (assoc s (engine-symbol-table e))))
		 (if (pair? c)
		     (display (cadr c))
		     (output s e)))))

;*---------------------------------------------------------------------*/
;*    unref ...                                                        */
;*---------------------------------------------------------------------*/
(markup-writer 'unref
   :options 'all
   :action (lambda (n e)
	      (let* ((s (markup-option n :skribe))
		     (k (markup-option n 'kind))
		     (f (cond
			   (s
			    (format #f "?~a@~a " k s))
			   (else
			    (format #f "?~a " k))))
		     (msg (list f (markup-body n)))
		     (n (list "[" (color :fg "red" (bold msg)) "]")))
		 (evaluate-document n e))))

;*---------------------------------------------------------------------*/
;*    &the-bibliography ...                                            */
;*---------------------------------------------------------------------*/
(markup-writer '&the-bibliography
   :before (lambda (n e)
	      (let ((w (markup-writer-get 'table e)))
		 (and (writer? w) (invoke (writer-before w) n e))))
   :action (lambda (n e)
	      (when (pair? (markup-body n))
		 (for-each (lambda (i) (output i e)) (markup-body n))))
   :after (lambda (n e)
	     (let ((w (markup-writer-get 'table e)))
		(and (writer? w) (invoke (writer-after w) n e)))))

;*---------------------------------------------------------------------*/
;*    &bib-entry ...                                                   */
;*---------------------------------------------------------------------*/
(markup-writer '&bib-entry
   :options '(:title)
   :before (lambda (n e)
	      (invoke (writer-before (markup-writer-get 'tr e)) n e))
   :action (lambda (n e)
	      (let ((wtc (markup-writer-get 'tc e)))
		 ;; the label
		 (markup-option-add! n :valign 'top)
		 (markup-option-add! n :align 'right)
		 (invoke (writer-before wtc) n e)
		 (output n e (markup-writer-get '&bib-entry-label e))
		 (invoke (writer-after wtc) n e)
		 ;; the body
		 (markup-option-add! n :valign 'top)
		 (markup-option-add! n :align 'left)
		 (invoke (writer-before wtc) n e)
		 (output n e (markup-writer-get '&bib-entry-body))
		 (invoke (writer-after wtc) n e)))
   :after (lambda (n e)
	     (invoke (writer-after (markup-writer-get 'tr e)) n e)))

;*---------------------------------------------------------------------*/
;*    &bib-entry-label ...                                             */
;*---------------------------------------------------------------------*/
(markup-writer '&bib-entry-label
   :options '(:title)
   :before "["
   :action (lambda (n e) (output (markup-option n :title) e))
   :after "]")

;*---------------------------------------------------------------------*/
;*    &bib-entry-author ...                                            */
;*---------------------------------------------------------------------*/
; (markup-writer '&bib-entry-author
;                :action (lambda (n e)
;                          (let ((names (markup-body n)))
;                            (skribe-eval
;                             (sc (abbreviate-first-names names)) e))))

;*---------------------------------------------------------------------*/
;*    &bib-entry-url ...                                               */
;*---------------------------------------------------------------------*/
(markup-writer '&bib-entry-url
               :action (lambda (n e)
                         (let ((url (markup-body n)))
                           (evaluate-document
                            (ref :text (it url) :url url) e))))

;*---------------------------------------------------------------------*/
;*    &bib-entry-body ...                                              */
;*---------------------------------------------------------------------*/
(markup-writer '&bib-entry-body
   :action (lambda (n e)
	      (let* ((kind (markup-option n 'kind))
                     (template (make-bib-entry-template/default kind)))
                (output-bib-entry-template n e template))))

;*---------------------------------------------------------------------*/
;*    &bib-entry-ident ...                                             */
;*---------------------------------------------------------------------*/
(markup-writer '&bib-entry-ident
   :action (lambda (n e)
	      (output (markup-option n 'number) e)))

;*---------------------------------------------------------------------*/
;*    &bib-entry-title ...                                             */
;*---------------------------------------------------------------------*/
(markup-writer '&bib-entry-title
   :action (lambda (n e)
	      (evaluate-document (markup-body n) e)))

;*---------------------------------------------------------------------*/
;*    &bib-entry-booktitle ...                                         */
;*---------------------------------------------------------------------*/
(markup-writer '&bib-entry-booktitle
               :action (lambda (n e)
                         (let ((title (markup-body n)))
                           (evaluate-document (it title) e))))

;*---------------------------------------------------------------------*/
;*    &bib-entry-journal ...                                           */
;*---------------------------------------------------------------------*/
(markup-writer '&bib-entry-journal
               :action (lambda (n e)
                         (evaluate-document (it (markup-body n)) e)))

;*---------------------------------------------------------------------*/
;*    &bib-entry-publisher ...                                         */
;*---------------------------------------------------------------------*/
(markup-writer '&bib-entry-publisher
   :action (lambda (n e)
	      (evaluate-document (markup-body n) e)))

;*---------------------------------------------------------------------*/
;*    &the-index ...  @label the-index@                                */
;*---------------------------------------------------------------------*/
(markup-writer '&the-index
   :options '(:column)
   :before (lambda (n e)
	      (output (markup-option n 'header) e))
   :action (lambda (n e)
	      (define (make-mark-entry n fst)
		 (let ((l (tr :class 'index-mark-entry
			     (td :colspan 2 :align 'left
				(bold (it (sf n)))))))
		    (if fst
			(list l)
			(list (tr (td :colspan 2)) l))))
	      (define (make-primary-entry n p)
		 (let* ((note (markup-option n :note))
			(b (markup-body n))
			(c (if note
			       (list b
				     (it (list " (" note ")")))
			       b)))
		    (when p
		       (markup-option-add! b :text
					   (list (markup-option b :text)
						 ", p."))
		       (markup-option-add! b :page #t))
		    (tr :class 'index-primary-entry
		       (td :colspan 2 :valign 'top :align 'left c))))
	      (define (make-secondary-entry n p)
		 (let* ((note (markup-option n :note))
			(b (markup-body n))
			(bb (markup-body b)))
		    (cond
		       ((not (or bb (is-markup? b 'url-ref)))
			(skribe-error 'the-index
				      "Illegal entry"
				      b))
		       (note
			(let ((r (if bb
				     (it (ref :class "the-index-secondary"
					    :handle bb
					    :page p
					    :text (if p
						      (list note ", p.")
						      note)))
				     (it (ref :class "the-index-secondary"
					    :url (markup-option b :url)
					    :page p
					    :text (if p
						      (list note ", p.")
						      note))))))
			   (tr :class 'index-secondary-entry
			      (td :valign 'top :align 'right :width 1. " ...")
			      (td :valign 'top :align 'left r))))
		       (else
			(let ((r (if bb
				     (ref :class "the-index-secondary"
					:handle bb
					:page p
					:text (if p " ..., p." " ..."))
				     (ref :class "the-index-secondary"
					:url (markup-option b :url)
					:page p
					:text (if p " ..., p." " ...")))))
			   (tr :class 'index-secondary-entry
			      (td :valign 'top :align 'right :width 1.)
			      (td :valign 'top :align 'left r)))))))
	      (define (make-column ie p)
		 (let loop ((ie ie)
			    (f #t))
		    (cond
		       ((null? ie)
			'())
		       ((not (pair? (car ie)))
			(append (make-mark-entry (car ie) f)
				(loop (cdr ie) #f)))
		       (else
			(cons (make-primary-entry (caar ie) p)
			      (append (map (lambda (x)
					      (make-secondary-entry x p))
					   (cdar ie))
				      (loop (cdr ie) #f)))))))
	      (define (make-sub-tables ie nc p)
		 (let* ((l (length ie))
			(w (/ 100. nc))
			(iepc (let ((d (/ l nc)))
				 (if (integer? d)
				     (inexact->exact d)
				     (+ 1 (inexact->exact (truncate d))))))
			(split (list-split ie iepc)))
		    (tr (map (lambda (ies)
				(td :valign 'top :width w
				   (if (pair? ies)
				       (table :width 100. (make-column ies p))
				       "")))
			     split))))
	      (let* ((ie (markup-body n))
		     (nc (markup-option n :column))
		     (loc (ast-loc n))
		     (pref (eq? (engine-custom e 'index-page-ref) #t))
		     (t (cond
			   ((null? ie)
			    "")
			   ;; FIXME: Since we don't support
			   ;; `:&skribe-eval-location', we could set up a
			   ;; `parameterize' thing around `skribe-eval' to
			   ;; provide it with the right location information.
			   ((or (not (integer? nc)) (= nc 1))
			    (table :width 100.
			       ;;:&skribe-eval-location loc
			       :class "index-table"
			       (make-column ie pref)))
			   (else
			    (table :width 100.
			       ;;:&skribe-eval-location loc
			       :class "index-table"
			       (make-sub-tables ie nc pref))))))
		 (output (evaluate-document t e) e))))

;*---------------------------------------------------------------------*/
;*    &the-index-header ...                                            */
;*    -------------------------------------------------------------    */
;*    The index header is only useful for targets that support         */
;*    hyperlinks such as HTML.                                         */
;*---------------------------------------------------------------------*/
(markup-writer '&the-index-header
   :action (lambda (n e) #f))

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
   :after "\n")

;*---------------------------------------------------------------------*/
;*    line-ref ...                                                     */
;*---------------------------------------------------------------------*/
(markup-writer 'line-ref
   :options '(:offset)
   :action (lambda (n e)
	      (let ((o (markup-option n :offset))
		    (n (markup-ident (handle-body (markup-body n)))))
		 (evaluate-document (it (if (integer? o) (+ o n) n)) e))))

