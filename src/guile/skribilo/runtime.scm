;;;
;;; runtime.stk	-- Skribe runtime system
;;;
;;; Copyright © 2003-2004 Erick Gallesio - I3S-CNRS/ESSI <eg@unice.fr>
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
;;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
;;; USA.
;;;
;;;           Author: Erick Gallesio [eg@essi.fr]
;;;    Creation date: 13-Aug-2003 18:47 (eg)
;;; Last file update: 15-Nov-2004 14:03 (eg)
;;;

(define-module (skribilo runtime)
  :export (;; Utilities
	   strip-ref-base ast->file-location string-canonicalize

	   ;; Markup functions
	   markup-option markup-option-add! markup-output

	   ;; Container functions
	   container-env-get

	   ;; Images
	   convert-image

	   ;; String writing
	   make-string-replace

	   ;; AST
	   ast->string))

(use-modules (skribilo debug)
	     (skribilo types)
	     (skribilo verify)
	     (skribilo resolve)
	     (skribilo output)
	     (skribilo evaluator)
	     (oop goops))



;;; ======================================================================
;;;
;;;				U T I L I T I E S
;;;
;;; ======================================================================


;;FIXME:  Remonter cette fonction
(define (strip-ref-base file)
  (if (not (string? *skribe-ref-base*))
      file
      (let ((l (string-length *skribe-ref-base*)))
	(cond
	  ((not (> (string-length file) (+ l 2)))
	   file)
	  ((not (substring=? file *skribe-ref-base* l))
	   file)
	  ((not (char=? (string-ref file l) (file-separator)))
	   file)
	  (else
	   (substring file (+ l 1) (string-length file)))))))


(define (ast->file-location ast)
   (let ((l (ast-loc ast)))
     (if (location? l)
	 (format "~a:~a:" (location-file l) (location-line l))
	 "")))

;; FIXME: Remonter cette fonction
(define (string-canonicalize old)
   (let* ((l (string-length old))
	  (new (make-string l)))
      (let loop ((r 0)
		 (w 0)
		 (s #f))
	 (cond
	    ((= r l)
	     (cond
		((= w 0)
		 "")
		((char-whitespace? (string-ref new (- w 1)))
		 (substring new 0 (- w 1)))
		((= w r)
		 new)
		(else
		 (substring new 0 w))))
	    ((char-whitespace? (string-ref old r))
	     (if s
		 (loop (+ r 1) w #t)
		 (begin
		    (string-set! new w #\-)
		    (loop (+ r 1) (+ w 1) #t))))
	    ((or (char=? (string-ref old r) #\#)
		 (>= (char->integer (string-ref old r)) #x7f))
	     (string-set! new w #\-)
	     (loop (+ r 1) (+ w 1) #t))
	    (else
	     (string-set! new w (string-ref old r))
	     (loop (+ r 1) (+ w 1) #f))))))


;;; ======================================================================
;;;
;;;			M A R K U P S   F U N C T I O N S
;;;
;;; ======================================================================
;; (define (markup-output markup
;;		       :optional (engine    #f)
;;		       :key	 (predicate #f)
;;				 (options  '())
;;				 (before    #f)
;;				 (action    #f)
;;				 (after     #f))
;;   (let ((e (or engine (use-engine))))
;;     (cond
;;       ((not (is-a? e <engine>))
;;           (skribe-error 'markup-writer "illegal engine" e))
;;       ((and (not before)
;;	    (not action)
;;	    (not after))
;;           (%find-markup-output e markup))
;;       (else
;;	  (let ((mp (if (procedure? predicate)
;;			(lambda (n e) (and (is-markup? n markup) (predicate n e)))
;;			(lambda (n e) (is-markup? n markup)))))
;;	    (engine-output e markup mp options
;;			   (or before (slot-ref e 'default-before))
;;			   (or action (slot-ref e 'default-action))
;;			   (or after  (slot-ref e 'default-after))))))))

(define (markup-option m opt)
  (if (markup? m)
      (let ((c (assq opt (slot-ref m 'options))))
	(and (pair? c) (pair? (cdr c))
	     (cadr c)))
      (skribe-type-error 'markup-option "Illegal markup: " m "markup")))


(define (markup-option-add! m opt val)
  (if (markup? m)
      (slot-set! m 'options (cons (list opt val)
				  (slot-ref m 'options)))
      (skribe-type-error 'markup-option "Illegal markup: " m "markup")))

;;; ======================================================================
;;;
;;;				C O N T A I N E R S
;;;
;;; ======================================================================
(define (container-env-get m key)
  (let ((c (assq key (slot-ref m 'env))))
    (and (pair? c) (cadr c))))


;;; ======================================================================
;;;
;;;				I M A G E S
;;;
;;; ======================================================================
(define (builtin-convert-image from fmt dir)
  (let* ((s  (suffix from))
	 (f  (string-append (prefix (basename from)) "." fmt))
	 (to (string-append dir "/" f)))   ;; FIXME:
    (cond
      ((string=? s fmt)
       to)
      ((file-exists? to)
       to)
      (else
       (let ((c (if (string=? s "fig")
		    (string-append "fig2dev -L " fmt " " from " > " to)
		    (string-append "convert " from " " to))))
	 (cond
	   ((> *skribe-verbose* 1)
	    (format (current-error-port) "  [converting image: ~S (~S)]" from c))
	   ((> *skribe-verbose* 0)
	    (format (current-error-port) "  [converting image: ~S]" from)))
	 (and (zero? (system c))
	      to))))))

(define (convert-image file formats)
  (let ((path (search-path (skribe-image-path) file)))
    (if (not path)
	(skribe-error 'convert-image
		      (format "Can't find `~a' image file in path: " file)
		      (skribe-image-path))
	(let ((suf (suffix file)))
	  (if (member suf formats)
	      (let* ((dir (if (string? *skribe-dest*)
			      (dirname *skribe-dest*)
			      #f)))
		(if dir
		    (let ((dest (basename path)))
		      (copy-file path (make-path dir dest))
		      dest)
		    path))
	      (let loop ((fmts formats))
		(if (null? fmts)
		    #f
		     (let* ((dir (if (string? *skribe-dest*)
				     (dirname *skribe-dest*)
				     "."))
			    (p (builtin-convert-image path (car fmts) dir)))
		       (if (string? p)
			   p
			   (loop (cdr fmts)))))))))))

;;; ======================================================================
;;;
;;;			S T R I N G - W R I T I N G
;;;
;;; ======================================================================

;;
;; (define (%make-html-replace)
;;   ;; Ad-hoc version for HTML, a little bit faster than the
;;   ;; make-general-string-replace define later (particularily if there
;;   ;; is nothing to replace since, it does not allocate a new string
;;   (let ((specials (string->regexp "&|\"|<|>")))
;;     (lambda (str)
;;       (if (regexp-match specials str)
;;	  (begin
;;	    (let ((out (open-output-string)))
;;	      (dotimes (i (string-length str))
;;		(let ((ch (string-ref str i)))
;;		  (case ch
;;		    ((#\") (display "&quot;" out))
;;		    ((#\&) (display "&amp;" out))
;;		    ((#\<) (display "&lt;" out))
;;		    ((#\>) (display "&gt;" out))
;;		    (else  (write-char ch out)))))
;;	      (get-output-string out)))
;;	  str))))


(define (%make-general-string-replace lst)
  ;; The general version
  (lambda (str)
    (let ((out (open-output-string)))
      (dotimes (i (string-length str))
	(let* ((ch  (string-ref str i))
	       (res (assq ch lst)))
	  (display (if res (cadr res) ch) out)))
      (get-output-string out))))

(define string->html
  (%make-general-string-replace '((#\" "&quot;") (#\& "&amp;") (#\< "&lt;")
				  (#\> "&gt;"))))

(define (make-string-replace lst)
  (let ((l (sort lst (lambda (r1 r2) (char<? (car r1) (car r2))))))
    (cond
      ((equal? l '((#\" "&quot;") (#\& "&amp;") (#\< "&lt;") (#\> "&gt;")))
       string->html)
      (else
       (%make-general-string-replace lst)))))




;;; ======================================================================
;;;
;;;				O P T I O N S
;;;
;;; ======================================================================

;;NEW ;;
;;NEW ;; GET-OPTION
;;NEW ;;
;;NEW (define (get-option obj key)
;;NEW   ;; This function either searches inside an a-list or a markup.
;;NEW   (cond
;;NEW     ((pair? obj)   (let ((c (assq key obj)))
;;NEW		     (and (pair? c) (pair? (cdr c)) (cadr c))))
;;NEW     ((markup? obj) (get-option (slot-ref obj 'option*) key))
;;NEW     (else          #f)))
;;NEW
;;NEW ;;
;;NEW ;; BIND-OPTION!
;;NEW ;;
;;NEW (define (bind-option! obj key value)
;;NEW   (slot-set! obj 'option* (cons (list key value)
;;NEW				(slot-ref obj 'option*))))
;;NEW
;;NEW
;;NEW ;;
;;NEW ;; GET-ENV
;;NEW ;;
;;NEW (define (get-env obj key)
;;NEW   ;;  This function either searches inside an a-list or a container
;;NEW   (cond
;;NEW     ((pair? obj)	(let ((c (assq key obj)))
;;NEW			  (and (pair? c) (cadr c))))
;;NEW     ((container? obj)   (get-env (slot-ref obj 'env) key))
;;NEW     (else		#f)))
;;NEW




;;; ======================================================================
;;;
;;;				    A S T
;;;
;;; ======================================================================

(define-generic ast->string)


(define-method (ast->string (ast <top>))     "")
(define-method (ast->string (ast <string>))  ast)
(define-method (ast->string (ast <number>))  (number->string ast))

(define-method (ast->string (ast <pair>))
  (let ((out (open-output-string)))
    (let Loop ((lst ast))
      (cond
	((null? lst)
	   (get-output-string out))
	(else
	   (display (ast->string (car lst)) out)
	   (unless (null? (cdr lst))
	     (display #\space out))
	   (Loop (cdr lst)))))))

(define-method (ast->string (ast <node>))
  (ast->string (slot-ref ast 'body)))


;;NEW ;;
;;NEW ;; AST-PARENT
;;NEW ;;
;;NEW (define (ast-parent n)
;;NEW   (slot-ref n 'parent))
;;NEW
;;NEW ;;
;;NEW ;; MARKUP-PARENT
;;NEW ;;
;;NEW (define (markup-parent m)
;;NEW   (let ((p (slot-ref m 'parent)))
;;NEW     (if (eq? p 'unspecified)
;;NEW	(skribe-error 'markup-parent "Unresolved parent reference" m)
;;NEW	p)))
;;NEW
;;NEW
;;NEW ;;
;;NEW ;; MARKUP-DOCUMENT
;;NEW ;;
;;NEW (define (markup-document m)
;;NEW   (let Loop ((p m)
;;NEW	     (l #f))
;;NEW     (cond
;;NEW       ((is-markup? p 'document)           p)
;;NEW       ((or (eq? p 'unspecified) (not p))  l)
;;NEW       (else			          (Loop (slot-ref p 'parent) p)))))
;;NEW
;;NEW ;;
;;NEW ;; MARKUP-CHAPTER
;;NEW ;;
;;NEW (define (markup-chapter m)
;;NEW   (let loop ((p m)
;;NEW	     (l #f))
;;NEW     (cond
;;NEW       ((is-markup? p 'chapter)           p)
;;NEW       ((or (eq? p 'unspecified) (not p)) l)
;;NEW       (else				 (loop (slot-ref p 'parent) p)))))
;;NEW
;;NEW
;;NEW ;;;; ======================================================================
;;NEW ;;;;
;;NEW ;;;;				H A N D L E S
;;NEW ;;;;
;;NEW ;;;; ======================================================================
;;NEW (define (handle-body h)
;;NEW   (slot-ref h 'body))
;;NEW
;;NEW
;;NEW ;;;; ======================================================================
;;NEW ;;;;
;;NEW ;;;;				F I N D
;;NEW ;;;;
;;NEW ;;;; ======================================================================
;;NEW (define (find pred obj)
;;NEW   (with-debug 4 'find
;;NEW     (debug-item "obj=" obj)
;;NEW     (let loop ((obj (if (is-a? obj <container>) (container-body obj) obj)))
;;NEW       (cond
;;NEW	((pair? obj)
;;NEW	 (apply append (map (lambda (o) (loop o)) obj)))
;;NEW	((is-a? obj <container>)
;;NEW	 (debug-item "loop=" obj " " (slot-ref obj 'ident))
;;NEW	 (if (pred obj)
;;NEW	     (list (cons obj (loop (container-body obj))))
;;NEW	     '()))
;;NEW	(else
;;NEW	 (if (pred obj)
;;NEW	     (list obj)
;;NEW	     '()))))))
;;NEW

;;NEW ;;;; ======================================================================
;;NEW ;;;;
;;NEW ;;;;		M A R K U P   A R G U M E N T   P A R S I N G
;;NEW ;;;
;;NEW ;;;; ======================================================================
;;NEW (define (the-body opt)
;;NEW   ;; Filter out the options
;;NEW   (let loop ((opt* opt)
;;NEW	     (res '()))
;;NEW     (cond
;;NEW       ((null? opt*)
;;NEW        (reverse! res))
;;NEW       ((not (pair? opt*))
;;NEW        (skribe-error 'the-body "Illegal body" opt))
;;NEW       ((keyword? (car opt*))
;;NEW        (if (null? (cdr opt*))
;;NEW	   (skribe-error 'the-body "Illegal option" (car opt*))
;;NEW	   (loop (cddr opt*) res)))
;;NEW       (else
;;NEW        (loop (cdr opt*) (cons (car opt*) res))))))
;;NEW
;;NEW
;;NEW
;;NEW (define (the-options opt+ . out)
;;NEW   ;; Returns an list made of options.The OUT argument contains
;;NEW   ;; keywords that are filtered out.
;;NEW   (let loop ((opt* opt+)
;;NEW	     (res '()))
;;NEW     (cond
;;NEW       ((null? opt*)
;;NEW        (reverse! res))
;;NEW       ((not (pair? opt*))
;;NEW        (skribe-error 'the-options "Illegal options" opt*))
;;NEW       ((keyword? (car opt*))
;;NEW        (cond
;;NEW	 ((null? (cdr opt*))
;;NEW	  (skribe-error 'the-options "Illegal option" (car opt*)))
;;NEW	 ((memq (car opt*) out)
;;NEW	  (loop (cdr opt*) res))
;;NEW	 (else
;;NEW	  (loop (cdr opt*)
;;NEW		(cons (list (car opt*) (cadr opt*)) res)))))
;;NEW       (else
;;NEW        (loop (cdr opt*) res)))))
;;NEW
