;;; lisp.scm -- Lisp Family Fontification
;;;
;;; Copyright 2003, 2004  Erick Gallesio - I3S-CNRS/ESSI <eg@essi.fr>
;;; Copyright 2005, 2006, 2007  Ludovic Courtès  <ludovic.courtes@laas.fr>
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


(define-module (skribilo coloring lisp)
  :use-module (skribilo utils syntax)
  :use-module (skribilo source)
  :use-module (skribilo coloring parameters)
  :use-module (skribilo lib)
  :use-module (srfi srfi-39)
  :use-module (ice-9 match)
  :autoload   (skribilo reader)            (make-reader)
  :autoload   (skribilo coloring lisp-lex) (lexer-init)
  :export (skribe scheme stklos bigloo lisp))


(define %lisp-keys	    #f)
(define %scheme-keys        #f)
(define %skribe-keys	    #f)
(define %stklos-keys	    #f)
(define %lisp-keys	    #f)



;;;
;;; definition-search
;;;

(define (definition-search inp read tab def?)
  (let Loop ((exp (read inp)))
    (unless (eof-object? exp)
      (if (def? exp)
	  (let ((start (and (pair? exp) (source-property exp 'line)))
		(stop  (port-line inp)))
	    (source-read-lines (port-filename inp) start stop tab))
	  (Loop (read inp))))))

(define (lisp-family-fontifier s)
  (lexer-init 'port (open-input-string s))
  (let loop ((token (lexer))
	     (res   '()))
    (if (eq? token 'eof)
	(reverse! res)
	(loop (lexer)
	      (cons token res)))))


;;;
;;; Lisp.
;;;

(define (lisp-extractor iport def tab)
  (definition-search
    iport
    read
    tab
    (lambda (exp)
      (match exp
	 (((or 'defun 'defmacro) fun _ . _)
	  (and (eq? def fun) exp))
	 (('defvar var . _)
	  (and (eq? var def) exp))
	 (else #f)))))

(define (init-lisp-keys)
  (unless %lisp-keys
    (set! %lisp-keys
      (append  ;; key
	       (map (lambda (x) (cons x '&source-keyword))
		    '(setq if let let* letrec cond case else progn lambda))
	       ;; define
	       (map (lambda (x) (cons x '&source-define))
		    '(defun defclass defmacro)))))
  %lisp-keys)

(define (lisp-fontifier s)
  (parameterize ((*the-keys*	   (init-lisp-keys))
		 (*bracket-highlight* #f)
		 (*class-highlight*   #f))
    (lisp-family-fontifier s)))


(define lisp
  (new language
       (name "lisp")
       (fontifier lisp-fontifier)
       (extractor lisp-extractor)))


;;;
;;; Scheme.
;;;

(define (scheme-extractor iport def tab)
  (definition-search
    iport
    %skribilo-module-reader
    tab
    (lambda (exp)
      (match exp
	 (((or 'define 'define-macro) (fun . _) . _)
	  (and (eq? def fun) exp))
	 (('define (? symbol? var) . _)
	  (and (eq? var def) exp))
	 (else #f)))))


(define (init-scheme-keys)
  (unless %scheme-keys
    (set! %scheme-keys
      (append ;; key
	      (map (lambda (x) (cons x '&source-keyword))
		   '(set! if let let* letrec quote cond case else begin do lambda))
	      ;; define
	      (map (lambda (x) (cons x '&source-define))
		 '(define define-syntax)))))
  %scheme-keys)


(define (scheme-fontifier s)
  (parameterize ((*the-keys*	   (init-scheme-keys))
		 (*bracket-highlight* #f)
		 (*class-highlight*   #f))
    (lisp-family-fontifier s)))


(define scheme
  (new language
       (name "scheme")
       (fontifier scheme-fontifier)
       (extractor scheme-extractor)))


;;;
;;; STkLos.
;;;

(define (stklos-extractor iport def tab)
  (definition-search
    iport
    %skribilo-module-reader
    tab
    (lambda (exp)
      (match exp
	 (((or 'define 'define-generic 'define-method 'define-macro)
	   (fun . _) . _)
	  (and (eq? def fun) exp))
	 (((or 'define 'define-module) (? symbol? var) . _)
	  (and (eq? var def) exp))
	 (else
		#f)))))


(define (init-stklos-keys)
  (unless %stklos-keys
    (init-scheme-keys)
    (set! %stklos-keys (append %scheme-keys
				;; Markups
				(map (lambda (x) (cons x '&source-key))
				     '(select-module import export))
				;; Key
				(map (lambda (x) (cons x '&source-keyword))
				     '(case-lambda dotimes match-case match-lambda))
				;; Define
				(map (lambda (x) (cons x '&source-define))
				     '(define-generic define-class
				       define-macro define-method define-module))
				;; error
				(map (lambda (x) (cons x '&source-error))
				     '(error call/cc)))))
  %stklos-keys)


(define (stklos-fontifier s)
  (parameterize ((*the-keys*	   (init-stklos-keys))
		 (*bracket-highlight* #t)
		 (*class-highlight*   #t))
    (lisp-family-fontifier s)))


(define stklos
  (new language
       (name "stklos")
       (fontifier stklos-fontifier)
       (extractor stklos-extractor)))


;;;
;;; Skribe.
;;;

(define (skribe-extractor iport def tab)
  (definition-search
    iport
    (make-reader 'skribe)
    tab
    (lambda (exp)
      (match exp
	(((or 'define 'define-macro 'define-markup 'define-public)
	  (fun . _) . _)
	 (and (eq? def fun) exp))
	(('define (? symbol? var) . _)
	 (and (eq? var def) exp))
	(('markup-output (quote mk) . _)
	 (and (eq? mk def) exp))
	(else #f)))))


(define (init-skribe-keys)
  (unless %skribe-keys
    (init-stklos-keys)
    (set! %skribe-keys (append %stklos-keys
				;; Markups
				(map (lambda (x) (cons x '&source-markup))
				     '(bold it emph tt color ref index underline
				       roman figure center pre flush hrule
				       linebreak image kbd code var samp
				       sc sf sup sub
				       itemize description enumerate item
				       table tr td th item prgm author
				       prgm hook font
				       document chapter section subsection
				       subsubsection paragraph p handle resolve
				       processor abstract margin toc
				       table-of-contents current-document
				       current-chapter current-section
				       document-sections* section-number
				       footnote print-index include skribe-load
				       slide))
				;; Define
				(map (lambda (x) (cons x '&source-define))
				     '(define-markup)))))
  %skribe-keys)


(define (skribe-fontifier s)
  (parameterize ((*the-keys*	   (init-skribe-keys))
		 (*bracket-highlight* #t)
		 (*class-highlight*   #t))
    (lisp-family-fontifier s)))


(define skribe
  (new language
       (name "skribe")
       (fontifier skribe-fontifier)
       (extractor skribe-extractor)))


;;;
;;; Bigloo.
;;;

(define (bigloo-extractor iport def tab)
  (definition-search
    iport
    %skribilo-module-reader
    tab
    (lambda (exp)
      (match exp
	 (((or 'define 'define-inline 'define-generic
	       'define-method 'define-macro 'define-expander)
	   (fun . _) . _)
	  (and (eq? def fun) exp))
	 (((or 'define 'define-struct 'define-library)
	   (? symbol? var) . _)
	  (and (eq? var def) exp))
	 (else #f)))))

(define bigloo
  (new language
       (name "bigloo")
       (fontifier scheme-fontifier)
       (extractor bigloo-extractor)))
