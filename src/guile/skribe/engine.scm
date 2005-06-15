;;;;
;;;; engines.stk	-- Skribe Engines Stuff
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
;;;;    Creation date: 24-Jul-2003 20:33 (eg)
;;;; Last file update: 28-Oct-2004 21:21 (eg)
;;;;

(define-module (skribe engine)
  :use-module (skribe debug)
;  :use-module (skribe eval)
  :use-module (skribe writer)
  :use-module (skribe types)

  :use-module (oop goops)
  :use-module (ice-9 optargs)
 
  :export (default-engine default-engine-set!
	   make-engine copy-engine find-engine
	   engine-custom engine-custom-set!
	   engine-format? engine-add-writer!
	   processor-get-engine
	   push-default-engine pop-default-engine))




;;; Module definition is split here because this file is read by the documentation
;;; Should be changed.
;(select-module SKRIBE-ENGINE-MODULE)

(define *engines*		'())
(define *default-engine* 	#f)
(define *default-engines* 	'())


(define (default-engine)
   *default-engine*)


(define (default-engine-set! e)
  (unless (engine? e)
    (skribe-error 'default-engine-set! "bad engine ~S" e))
  (set! *default-engine* e)
  (set! *default-engines* (cons e *default-engines*))
  e)


(define (push-default-engine e)
   (set! *default-engines* (cons e *default-engines*))
   (default-engine-set! e))

(define (pop-default-engine)
   (if (null? *default-engines*)
       (skribe-error 'pop-default-engine "Empty engine stack" '())
       (begin
	  (set! *default-engines* (cdr *default-engines*))
	  (if (pair? *default-engines*)
	      (default-engine-set! (car *default-engines*))
	      (set! *default-engine* #f)))))


(define (processor-get-engine combinator newe olde)
  (cond
    ((procedure? combinator)
     (combinator newe olde))
    ((engine? newe)
     newe)
    (else
     olde)))


(define (engine-format? fmt . e)
  (let ((e (cond
	     ((pair? e) (car e))
	     ((engine? *skribe-engine*) *skribe-engine*)
	     (else (find-engine *skribe-engine*)))))
    (if (not (engine? e))
	(skribe-error 'engine-format? "No engine" e)
	(string=? fmt (engine-format e)))))

;;;
;;; MAKE-ENGINE
;;; 
(define* (make-engine ident #:key (version 'unspecified)
		     		(format "raw")
				(filter #f)
				(delegate #f)
				(symbol-table '())
				(custom '())
				(info '()))
  (let ((e (make <engine> :ident ident :version version :format format
		 	  :filter filter :delegate delegate
			  :symbol-table symbol-table
			  :custom custom :info info)))
    ;; store the engine in the global table
    (set! *engines* (cons e *engines*))
    ;; return it
    e))


;;;
;;; COPY-ENGINE
;;;
(define* (copy-engine ident e #:key (version 'unspecified)
				  (filter #f)
				  (delegate #f)
				  (symbol-table #f)
				  (custom #f))
  (let ((new (shallow-clone e)))
    (slot-set! new 'ident 	 ident)
    (slot-set! new 'version 	 version)
    (slot-set! new 'filter	 (or filter (slot-ref e 'filter)))
    (slot-set! new 'delegate	 (or delegate (slot-ref e 'delegate)))
    (slot-set! new 'symbol-table (or symbol-table (slot-ref e 'symbol-table)))
    (slot-set! new 'customs	 (or custom (slot-ref e 'customs)))

    (set! *engines* (cons new *engines*))
    new))


;;;
;;; 	FIND-ENGINE
;;;
(define (%find-loaded-engine id version)
  (let Loop ((es *engines*))
    (cond
      ((null? es) #f)
      ((eq? (slot-ref (car es) 'ident) id)
       (cond
	   ((eq? version 'unspecified) 		       (car es))
	   ((eq? version (slot-ref (car es) 'version)) (car es))
	   (else			 	       (Loop (cdr es)))))
      (else (loop (cdr es))))))


(define* (find-engine id #:key (version 'unspecified))
  (with-debug 5 'find-engine
     (debug-item "id=" id " version=" version)

     (or (%find-loaded-engine id version)
	 (let ((c (assq id *skribe-auto-load-alist*)))
	   (debug-item "c=" c)
	   (if (and c (string? (cdr c)))
	       (begin
		 (skribe-load (cdr c) :engine 'base)
		 (%find-loaded-engine id version))
	       #f)))))

;;;
;;; ENGINE-CUSTOM
;;;
(define (engine-custom e id)
  (let* ((customs (slot-ref e 'customs))
	 (c       (assq id customs)))
    (if (pair? c)
	(cadr c)
	'unspecified)))


;;;
;;; ENGINE-CUSTOM-SET!
;;;
(define (engine-custom-set! e id val)
  (let* ((customs (slot-ref e 'customs))
	 (c       (assq id customs)))
    (if (pair? c)
	(set-car! (cdr c) val)
	(slot-set! e 'customs (cons (list id val) customs)))))


;;;
;;; ENGINE-ADD-WRITER!
;;;
(define (engine-add-writer! e ident pred upred opt before action after class valid)
  (define (check-procedure name proc arity)
    (cond
      ((not (procedure? proc))
         (skribe-error ident "Illegal procedure" proc))
      ((not (equal? (%procedure-arity proc) arity))
         (skribe-error ident
		       (format #f "Illegal ~S procedure" name)
		       proc))))

  (define (check-output name proc)
    (and proc (or (string? proc) (check-procedure name proc 2))))

  ;;
  ;; Engine-add-writer! starts here
  ;;
  (unless (is-a? e <engine>)
    (skribe-error ident "Illegal engine" e))
  
  ;; check the options
  (unless (or (eq? opt 'all) (list? opt))
    (skribe-error ident "Illegal options" opt))
  
  ;; check the correctness of the predicate
  (check-procedure "predicate" pred 2)

  ;; check the correctness of the validation proc
  (when valid
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
    (slot-set! e 'writers (cons n (slot-ref e 'writers)))
    n))

;;;; ======================================================================
;;;;
;;;;   				    I N I T S
;;;;
;;;; ======================================================================

;; A base engine must pre-exist before anything is loaded. In
;; particular, this dummy base engine is used to load the actual
;; definition of base. 

(make-engine 'base :version 'bootstrap)


