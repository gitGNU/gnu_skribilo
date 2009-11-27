;;; writer.scm  --  Markup writers.
;;; -*- coding: iso-8859-1 -*-
;;;
;;; Copyright 2005, 2006, 2008, 2009  Ludovic Courtès <ludo@gnu.org>
;;; Copyright 2003, 2004  Erick Gallesio - I3S-CNRS/ESSI <eg@essi.fr>
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

(define-module (skribilo writer)
  :export (<writer> writer? writer-options writer-ident
	            writer-before writer-action writer-after writer-class

	   invoke markup-writer markup-writer-get markup-writer-get*
	   lookup-markup-writer copy-markup-writer)

  :use-module (skribilo utils syntax)
  :autoload (srfi srfi-1)     (find filter)
  :autoload (srfi srfi-34)    (raise)
  :autoload (skribilo engine) (engine? engine-ident? default-engine)

  :use-module (srfi srfi-35)
  :use-module (skribilo condition)
  :use-module (skribilo debug)
  :use-module (skribilo ast)

  :use-module (oop goops)
  :use-module (ice-9 optargs))


(skribilo-module-syntax)



;;;
;;; Class definition.
;;;

(define-class <writer> ()
  (ident	:init-keyword :ident	 :init-value '??? :getter writer-ident)
  (class	:init-keyword :class	 :init-value 'unspecified
		:getter writer-class)
  (pred	:init-keyword :pred	 :init-value 'unspecified)
  (upred	:init-keyword :upred	 :init-value 'unspecified)
  (options	:init-keyword :options	 :init-value '()  :getter writer-options)
  (verified?	:init-keyword :verified? :init-value #f)
  (validate	:init-keyword :validate  :init-value #f)
  (before	:init-keyword :before	 :init-value #f   :getter writer-before)
  (action	:init-keyword :action	 :init-value #f   :getter writer-action)
  (after	:init-keyword :after	 :init-value #f   :getter writer-after))

(define (writer? obj)
  (is-a? obj <writer>))

(define-method (write (obj <writer>) port)
  (format port "#[~A (~A) ~A]"
	  (class-name (class-of obj))
	  (slot-ref obj 'ident)
	  (object-address obj)))



;;;
;;; Writer methods.
;;;

(define (invoke proc node e)
  (with-debug 5 'invoke
     (debug-item "e=" (engine-ident e))
     (debug-item "node=" node " " (if (markup? node) (markup-markup node) ""))

     (if (string? proc)
	 (display proc)
	 (if (procedure? proc)
	     (proc node e)))))


(define (%procedure-arity proc)
  ;; Return the number of required arguments for PROC.  This technique is
  ;; known to work with Guile 1.8 and Guile 1.9.5.
  (car (procedure-property proc 'arity)))

(define (make-writer-predicate markup predicate class)
  (let* ((t2 (if class
		 (lambda (n e)
		   (equal? (markup-class n) class))
		 #f)))
    (if predicate
	(cond
	  ((or (not (procedure? predicate))
               (not (eq? (%procedure-arity predicate) 2)))
           (raise (condition (&invalid-argument-error
                              (proc-name 'make-writer-predicate)
                              (argument  predicate)))))
	  (else
	   (if (procedure? t2)
	       (lambda (n e)
		 (and (t2 n e) (predicate n e)))
	       predicate)))
	t2)))


;;;
;;; `markup-writer'
;;;

(define* (markup-writer markup ;; #:optional (engine #f)
			#:key (predicate #f) (class #f) (options '())
			      (validate #f)
			      (before #f)
			      (action 'unspecified)
			      (after #f)
			#:rest engine)
  ;;; XXX:  `lambda*' sucks and fails when both optional arguments and
  ;;; keyword arguments are used together.  In particular, if ENGINE is not
  ;;; specified by the caller but other keyword arguments are specified, it
  ;;; will consider the value of ENGINE to be the first keyword found.

;  (let ((e (or engine (default-engine))))
  (let ((e (or (if (and (list? engine) (not (keyword? (car engine))))
		   (car engine)
		   #f)
	       (default-engine))))

    (cond
      ((and (not (symbol? markup)) (not (eq? markup #t)))
       (raise (condition (&invalid-argument-error
                          (proc-name 'markup-writer)
                          (argument  markup)))))
      ((not (engine? e))
       (raise (condition (&invalid-argument-error
                          (proc-name 'markup-writer)
                          (argument  e)))))
      ((and (not predicate)
	    (not class)
	    (null? options)
	    (not before)
	    (eq? action 'unspecified)
	    (not after))
       (raise (condition (&invalid-argument-error
                          (proc-name 'markup-writer)
                          (argument  (list predicate class options
                                           before action after))))))
      (else
       (let ((m  (make-writer-predicate markup predicate class))
	     (ac (if (eq? action 'unspecified)
		     (lambda (n e)
                       ((@ (skribilo output) output)
                        (markup-body n) e))
		     action)))
	 (engine-add-writer! e markup m predicate
			     options before ac after class validate))))))

(define (engine-add-writer! e ident pred upred opt before action
			    after class valid)
  ;; Add a writer to engine E.  If IDENT is a symbol, then it should denote
  ;; a markup name and the writer being added is specific to that markup.  If
  ;; IDENT is `#t' (for instance), then it is assumed to be a ``free writer''
  ;; that may apply to any kind of markup for which PRED returns true.

  (define (check-procedure name proc arity)
    (if (or (not (procedure? proc))
            (not (equal? (%procedure-arity proc) arity)))
        (raise (condition (&invalid-argument-error
                           (proc-name 'engine-add-writer!)
                           (argument  proc))))
        #t))

  (define (check-output name proc)
    (and proc (or (string? proc) (check-procedure name proc 2))))

  ;;
  ;; Engine-add-writer! starts here
  ;;
  (or (engine? e)
      (raise (condition (&invalid-argument-error
                         (proc-name 'engine-add-writer!)
                         (argument  e)))))

  ;; check the options
  (or (eq? opt 'all) (list? opt)
      (raise (condition (&invalid-argument-error
                         (proc-name 'engine-add-writer!)
                         (argument  opt)))))

  ;; check the correctness of the predicate
  (if pred
      (check-procedure "predicate" pred 2))

  ;; check the correctness of the validation proc
  (if valid
      (check-procedure "validate" valid 2))

  ;; check the correctness of the three actions
  (check-output "before" before)
  (check-output "action" action)
  (check-output "after" after)

  ;; create a new writer and bind it
  (let ((n (make <writer>
	     :ident (if (symbol? ident) ident 'all)
	     :class class :pred pred :upred upred :options opt
	     :before before :action action :after after
	     :validate valid)))
    (if (symbol? ident)
	(let ((writers (slot-ref e 'writers)))
	  (hashq-set! writers ident
		      (cons n (hashq-ref writers ident '()))))
	(slot-set! e 'free-writers
		   (cons n (slot-ref e 'free-writers))))
    n))


;;;
;;; Finding a markup writer.
;;;

(define (lookup-markup-writer node e)
  ;; Find the writer that applies best to NODE.  See also `markup-writer-get'
  ;; and `markup-writer-get*'.

  (define (matching-writer writers)
    (find (lambda (w)
	    (let ((pred (slot-ref w 'pred)))
	      (if (procedure? pred)
		  (pred node e)
		  #t)))
	  writers))

  (let* ((writers (slot-ref e 'writers))
	 (node-writers (hashq-ref writers (markup-markup node) '()))
	 (delegate (slot-ref e 'delegate)))

    (or (matching-writer node-writers)
	(matching-writer (slot-ref e 'free-writers))
	(and (engine? delegate)
	     (lookup-markup-writer node delegate)))))


(define* (markup-writer-get markup :optional engine :key (class #f) (pred #f))
  ;; Get a markup writer for MARKUP (a symbol) in ENGINE, with class CLASS
  ;; and user predicate PRED.  [FIXME: Useless since PRED is a procedure and
  ;; therefore not comparable?]

  (define (matching-writer writers)
    (find (lambda (w)
	    (and (if class (equal? (writer-class w) class) #t)
		 (or (eq? pred 'unspecified)
		     (eq? (slot-ref w 'upred) pred))))
	  writers))

  (let ((e (or engine (default-engine))))
    (cond
      ((not (symbol? markup))
       (raise (condition (&invalid-argument-error
                          (proc-name 'markup-writer-get)
                          (argument  markup)))))
      ((not (engine? e))
       (raise (condition (&invalid-argument-error
                          (proc-name 'markup-writer-get)
                          (argument  e)))))
      (else
       (let* ((writers (slot-ref e 'writers))
	      (markup-writers (hashq-ref writers markup '()))
	      (delegate (slot-ref e 'delegate)))

	 (or (matching-writer markup-writers)
	     (and (engine? delegate)
		  (markup-writer-get markup delegate
				     :class class :pred pred))))))))


(define* (markup-writer-get* markup #:optional engine #:key (class #f))
  ;; Finds all writers, recursively going through the engine hierarchy, that
  ;; match MARKUP with optional CLASS attribute.

  (define (matching-writers writers)
    (filter (lambda (w)
	      (or (not class)
		  (equal? (writer-class w) class)))
	    writers))

  (let ((e (or engine (default-engine))))
    (cond
      ((not (symbol? markup))
       (raise (condition (&invalid-argument-error
                          (proc-name 'markup-writer-get*)
                          (argument  markup)))))
      ((not (engine? e))
       (raise (condition (&invalid-argument-error
                          (proc-name 'markup-writer-get*)
                          (argument  e)))))
      (else
       (let* ((writers (slot-ref e 'writers))
	      (delegate (slot-ref e 'delegate)))

	 (append (matching-writers writers)
		 (if (engine? delegate)
		     (markup-writer-get* markup delegate :class class)
		     '())))))))


(define* (copy-markup-writer markup old-engine ;; #:optional new-engine
			      :key (predicate *unspecified*)
				   (class     *unspecified*)
				   (options   *unspecified*)
				   (validate  *unspecified*)
				   (before    *unspecified*)
				   (action    *unspecified*)
				   (after     *unspecified*)
                              :rest args)
    (define new-engine
      ;; XXX: Work around `lambda*' suckingness (see `markup-writer').
      (and (not (null? args))
           (car args)))

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

;;; writer.scm ends here
