;;;;
;;;; lisp.scm	-- Lisp Family Fontification
;;;;
;;;; Copyright © 2003-2004 Erick Gallesio - I3S-CNRS/ESSI <eg@essi.fr>
;;;; Copyright © 2005      Ludovic Courtès  <ludovic.courtes@laas.fr>
;;;;
;;;;
;;;; This program is free software; you can redistribute it and/or modify
;;;; it under the terms of the GNU General Public License as published by
;;;; the Free Software Foundation; either version 2 of the License, or
;;;; (at your option) any later version.
;;;;
;;;; This program is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU General Public License
;;;; along with this program; if not, write to the Free Software
;;;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
;;;; USA.
;;;;
;;;;           Author: Erick Gallesio [eg@essi.fr]
;;;;    Creation date: 16-Oct-2003 22:17 (eg)
;;;; Last file update: 28-Oct-2004 21:14 (eg)
;;;;

(define-module (skribilo coloring lisp)
  :use-module (skribilo utils syntax)
  :use-module (skribilo source)
  :use-module (skribilo lib)
  :use-module (skribilo runtime)
  :export (skribe scheme stklos bigloo lisp))


(define *bracket-highlight* (make-fluid))
(define *class-highlight*   (make-fluid))
(define *the-keys*	    (make-fluid))

(define *lisp-keys*	    (make-fluid))
(define *scheme-keys*       (make-fluid))
(define *skribe-keys*	    (make-fluid))
(define *stklos-keys*	    (make-fluid))
(define *lisp-keys*	    (make-fluid))


;;;
;;; DEFINITION-SEARCH
;;;
(define (definition-search inp tab test)
  (let Loop ((exp (%read inp)))
    (unless (eof-object? exp)
      (if (test exp)
	  (let ((start (and (%epair? exp) (%epair-line exp)))
		(stop  (port-current-line inp)))
	    (source-read-lines (port-file-name inp) start stop tab))
	  (Loop (%read inp))))))


(define (lisp-family-fontifier s)
  (let ((lisp-input (open-input-string s)))
    (let loop ((token (read lisp-input))
	       (res   '()))
      (if (eof-object? token)
	  (reverse! res)
	  (loop (read lisp-input)
		(cons token res))))))

;;;; ======================================================================
;;;;
;;;;				LISP
;;;;
;;;; ======================================================================
(define (lisp-extractor iport def tab)
  (definition-search
    iport
    tab
    (lambda (exp)
      (match-case exp
	 (((or defun defmacro) ?fun ?- . ?-)
		(and (eq? def fun) exp))
	 ((defvar ?var . ?-)
		(and (eq? var def) exp))
	 (else
		#f)))))

(define (init-lisp-keys)
  (unless *lisp-keys*
    (set! *lisp-keys*
      (append  ;; key
	       (map (lambda (x) (cons x '&source-keyword))
		    '(setq if let let* letrec cond case else progn lambda))
	       ;; define
	       (map (lambda (x) (cons x '&source-define))
		    '(defun defclass defmacro)))))
  *lisp-keys*)

(define (lisp-fontifier s)
  (with-fluids ((*the-keys*	   (init-lisp-keys))
		(*bracket-highlight* #f)
		(*class-highlight*   #f))
    (lisp-family-fontifier s)))


(define lisp
  (new language
       (name "lisp")
       (fontifier lisp-fontifier)
       (extractor lisp-extractor)))

;;;; ======================================================================
;;;;
;;;;				SCHEME
;;;;
;;;; ======================================================================
(define (scheme-extractor iport def tab)
  (definition-search
    iport
    tab
    (lambda (exp)
      (match-case exp
	 (((or define define-macro) (?fun . ?-) . ?-)
	     (and (eq? def fun) exp))
	 ((define (and (? symbol?) ?var) . ?-)
	     (and (eq? var def) exp))
	 (else
	     #f)))))


(define (init-scheme-keys)
  (unless *scheme-keys*
    (set! *scheme-keys*
      (append ;; key
	      (map (lambda (x) (cons x '&source-keyword))
		   '(set! if let let* letrec quote cond case else begin do lambda))
	      ;; define
	      (map (lambda (x) (cons x '&source-define))
		 '(define define-syntax)))))
  *scheme-keys*)


(define (scheme-fontifier s)
  (with-fluids ((*the-keys*	   (init-scheme-keys))
		(*bracket-highlight* #f)
		(*class-highlight*   #f))
    (lisp-family-fontifier s)))


(define scheme
  (new language
       (name "scheme")
       (fontifier scheme-fontifier)
       (extractor scheme-extractor)))

;;;; ======================================================================
;;;;
;;;;				STKLOS
;;;;
;;;; ======================================================================
(define (stklos-extractor iport def tab)
  (definition-search
    iport
    tab
    (lambda (exp)
      (match-case exp
	 (((or define define-generic define-method define-macro)
	   (?fun . ?-) . ?-)
		(and (eq? def fun) exp))
	 (((or define define-module) (and (? symbol?) ?var) . ?-)
		(and (eq? var def) exp))
	 (else
		#f)))))


(define (init-stklos-keys)
  (unless *stklos-keys*
    (init-scheme-keys)
    (set! *stklos-keys* (append *scheme-keys*
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
  *stklos-keys*)


(define (stklos-fontifier s)
  (with-fluids ((*the-keys*	   (init-stklos-keys))
		(*bracket-highlight* #t)
		(*class-highlight*   #t))
    (lisp-family-fontifier s)))


(define stklos
  (new language
       (name "stklos")
       (fontifier stklos-fontifier)
       (extractor stklos-extractor)))

;;;; ======================================================================
;;;;
;;;;				SKRIBE
;;;;
;;;; ======================================================================
(define (skribe-extractor iport def tab)
  (definition-search
    iport
    tab
    (lambda (exp)
      (match-case exp
	(((or define define-macro define-markup) (?fun . ?-) . ?-)
	   (and (eq? def fun) exp))
	((define (and (? symbol?) ?var) . ?-)
	   (and (eq? var def) exp))
	((markup-output (quote ?mk) . ?-)
	   (and (eq? mk def) exp))
	(else
	   #f)))))


(define (init-skribe-keys)
  (unless *skribe-keys*
    (init-stklos-keys)
    (set! *skribe-keys* (append *stklos-keys*
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
  *skribe-keys*)


(define (skribe-fontifier s)
  (with-fluids ((*the-keys*	   (init-skribe-keys))
		(*bracket-highlight* #t)
		(*class-highlight*   #t))
    (lisp-family-fontifier s)))


(define skribe
  (new language
       (name "skribe")
       (fontifier skribe-fontifier)
       (extractor skribe-extractor)))

;;;; ======================================================================
;;;;
;;;;				BIGLOO
;;;;
;;;; ======================================================================
(define (bigloo-extractor iport def tab)
  (definition-search
    iport
    tab
    (lambda (exp)
      (match-case exp
	 (((or define define-inline define-generic
	       define-method define-macro define-expander)
	   (?fun . ?-) . ?-)
		(and (eq? def fun) exp))
	 (((or define define-struct define-library) (and (? symbol?) ?var) . ?-)
		(and (eq? var def) exp))
	 (else
		#f)))))

(define bigloo
  (new language
       (name "bigloo")
       (fontifier scheme-fontifier)
       (extractor bigloo-extractor)))
