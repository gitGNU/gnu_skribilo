;;;;
;;;; writer.stk	-- Skribe Writer Stuff
;;;;
;;;; Copyright © 2003-2004 Erick Gallesio - I3S-CNRS/ESSI <eg@essi.fr>
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
;;;;    Creation date: 15-Sep-2003 22:21 (eg)
;;;; Last file update:  4-Mar-2004 10:48 (eg)
;;;;


(define-module (skribilo writer)
  :export (invoke markup-writer markup-writer-get markup-writer-get*
	   lookup-markup-writer copy-markup-writer))


(use-modules (skribilo debug)
	     (skribilo engine)
	     (skribilo output)
	     (skribilo types)
	     (skribilo lib)

	     (oop goops)
	     (ice-9 optargs))


;;;; ======================================================================
;;;;
;;;;				INVOKE
;;;;
;;;; ======================================================================
(define (invoke proc node e)
  (with-debug 5 'invoke
     (debug-item "e=" (engine-ident e))
     (debug-item "node=" node " " (if (markup? node) (markup-markup node) ""))

     (if (string? proc)
	 (display proc)
	 (if (procedure? proc)
	     (proc node e)))))


;;;; ======================================================================
;;;;
;;;;				LOOKUP-MARKUP-WRITER
;;;;
;;;; ======================================================================
(define (lookup-markup-writer node e)
  (let ((writers (slot-ref e 'writers))
	(delegate (slot-ref e 'delegate)))
    (let loop ((w* writers))
      (cond
	((pair? w*)
	   (let ((pred (slot-ref (car w*) 'pred)))
	     (if (pred node e)
		 (car w*)
		 (loop (cdr w*)))))
	((engine? delegate)
	   (lookup-markup-writer node delegate))
	(else
	   #f)))))

;;;; ======================================================================
;;;;
;;;;				MAKE-WRITER-PREDICATE
;;;;
;;;; ======================================================================
(define (make-writer-predicate markup predicate class)
  (let* ((t1 (if (symbol? markup)
		 (lambda (n e) (is-markup? n markup))
		 (lambda (n e) #t)))
	 (t2 (if class
		 (lambda (n e)
		   (and (t1 n e) (equal? (markup-class n) class)))
		 t1)))
    (if predicate
	(cond
	  ((not (procedure? predicate))
	     (skribe-error 'markup-writer
			   "Illegal predicate (procedure expected)"
			   predicate))
	  ((not (eq? (%procedure-arity predicate) 2))
	     (skribe-error 'markup-writer
			   "Illegal predicate arity (2 arguments expected)"
			   predicate))
	  (else
	     (lambda (n e)
	       (and (t2 n e) (predicate n e)))))
	t2)))

;;;; ======================================================================
;;;;
;;;;				MARKUP-WRITER
;;;;
;;;; ======================================================================
; (define-macro (lambda** arglist . body)
;   (let ((parse-arglist (module-ref (resolve-module '(ice-9 optargs))
; 				   'parse-arglist)))
;     (parse-arglist
;      arglist
;      (lambda (mandatory-args optionals keys aok? rest-arg)
;        (let ((l**-rest-arg (gensym "L**-rest"))
; 	     (l**-loop (gensym "L**-loop")))
; 	 `(lambda (,@mandatory-args . ,l**-rest-arg)
; 	    `(let ,l**-loop ((,l**-rest-arg ,l**-rest-arg)
; 			     (,rest-arg '())
; 			     ,@optionals
; 			     ,@keys)
; 		  (if (null? ,l**-rest-arg)
; 		      (begin
; 			,@body)

(define* (markup-writer markup ;; #:optional (engine #f)
			#:key (predicate #f) (class #f) (options '())
			      (validate #f)
			      (before #f)
			      (action 'unspecified)
			      (after #f)
			#:rest engine)
  ;;; FIXME:  `lambda*' sucks and fails when both optional arguments and
  ;;; keyword arguments are used together.  In particular, if ENGINE is not
  ;;; specified by the caller but other keyword arguments are specified, it
  ;;; will consider the value of ENGINE to be the first keyword found.

;  (let ((e (or engine (default-engine))))
  (let ((e (or (and (list? engine)
		    (not (keyword? (car engine))))
	       (default-engine))))

    (cond
      ((and (not (symbol? markup)) (not (eq? markup #t)))
       (skribe-error 'markup-writer "illegal markup" markup))
      ((not (engine? e))
	  (skribe-error 'markup-writer "illegal engine" e))
      ((and (not predicate)
	    (not class)
	    (null? options)
	    (not before)
	    (eq? action 'unspecified)
	    (not after))
       (skribe-error 'markup-writer "illegal writer" markup))
      (else
       (let ((m  (make-writer-predicate markup predicate class))
	     (ac (if (eq? action 'unspecified)
		     (lambda (n e) (output (markup-body n) e))
		     action)))
	 (engine-add-writer! e markup m predicate
			     options before ac after class validate))))))


;;;; ======================================================================
;;;;
;;;;				MARKUP-WRITER-GET
;;;;
;;;; ======================================================================
(define* (markup-writer-get markup :optional engine :key (class #f) (pred #f))
  (let ((e (or engine (default-engine))))
    (cond
      ((not (symbol? markup))
	 (skribe-error 'markup-writer-get "Illegal symbol" markup))
      ((not (engine? e))
	 (skribe-error 'markup-writer-get "Illegal engine" e))
      (else
       (let liip ((e e))
	 (let loop ((w* (slot-ref e 'writers)))
	   (cond
	     ((pair? w*)
		(if (and (eq? (writer-ident (car w*)) markup)
			 (equal? (writer-class (car w*)) class)
			 (or (unspecified? pred)
			     (eq? (slot-ref (car w*) 'upred) pred)))
		    (car w*)
		    (loop (cdr w*))))
	     ((engine? (slot-ref e 'delegate))
		(liip (slot-ref e 'delegate)))
	     (else
		#f))))))))

;;;; ======================================================================
;;;;
;;;;				MARKUP-WRITER-GET*
;;;;
;;;; ======================================================================

;; Finds all writers that matches MARKUP with optional CLASS attribute.

(define* (markup-writer-get* markup #:optional engine #:key (class #f))
  (let ((e (or engine (default-engine))))
    (cond
      ((not (symbol? markup))
       (skribe-error 'markup-writer "Illegal symbol" markup))
      ((not (engine? e))
       (skribe-error 'markup-writer "Illegal engine" e))
      (else
       (let liip ((e e)
		  (res '()))
	 (let loop ((w* (slot-ref e 'writers))
		    (res res))
	   (cond
	     ((pair? w*)
	      (if (and (eq? (slot-ref (car w*) 'ident) markup)
		       (equal? (slot-ref (car w*) 'class) class))
		  (loop (cdr w*) (cons (car w*) res))
		  (loop (cdr w*) res)))
	     ((engine? (slot-ref e 'delegate))
	      (liip (slot-ref e 'delegate) res))
	     (else
	      (reverse! res)))))))))

;;; ======================================================================
;;;;
;;;;				COPY-MARKUP-WRITER
;;;;
;;;; ======================================================================
(define* (copy-markup-writer markup old-engine :optional new-engine
			      :key (predicate 'unspecified)
				   (class 'unspecified)
				   (options 'unspecified)
				   (validate 'unspecified)
				   (before 'unspecified)
				   (action 'unspecified)
				   (after 'unspecified))
    (let ((old        (markup-writer-get markup old-engine))
	  (new-engine (or new-engine old-engine)))
      (markup-writer markup new-engine
	 :pred      (if (unspecified? predicate) (slot-ref old 'pred) predicate)
	 :class     (if (unspecified? class)     (slot-ref old 'class) class)
	 :options   (if (unspecified? options)   (slot-ref old 'options) options)
	 :validate  (if (unspecified? validate)  (slot-ref old 'validate) validate)
	 :before    (if (unspecified? before)    (slot-ref old 'before) before)
	 :action    (if (unspecified? action)    (slot-ref old 'action) action)
	 :after     (if (unspecified? after)     (slot-ref old 'after) after))))
