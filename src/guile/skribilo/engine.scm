;;; engine.scm	-- Skribilo engines.
;;;
;;; Copyright 2003-2004  Erick Gallesio - I3S-CNRS/ESSI <eg@essi.fr>
;;; Copyright 2005  Ludovic Courtès  <ludovic.courtes@laas.fr>
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

(define-module (skribilo engine)
  :use-module (skribilo debug)
  :use-module (skribilo utils syntax)
  :use-module (skribilo lib)

  ;; `(skribilo writer)' depends on this module so it needs to be loaded
  ;; after we defined `<engine>' and the likes.
  :autoload (skribilo writer) (<writer>)

  :use-module (oop goops)
  :use-module (ice-9 optargs)
  :autoload   (srfi srfi-39)  (make-parameter)

  :export (<engine> engine? engine-ident engine-format
		    engine-customs engine-filter engine-symbol-table

	   *current-engine*
	   default-engine default-engine-set!
	   make-engine copy-engine find-engine lookup-engine
	   engine-custom engine-custom-set!
	   engine-format? engine-add-writer!
	   processor-get-engine
	   push-default-engine pop-default-engine))


(fluid-set! current-reader %skribilo-module-reader)


;;;
;;; Class definition.
;;;

(define-class <engine> ()
  (ident		:init-keyword :ident		:init-value '???)
  (format		:init-keyword :format		:init-value "raw")
  (info		:init-keyword :info		:init-value '())
  (version		:init-keyword :version
			:init-value 'unspecified)
  (delegate		:init-keyword :delegate		:init-value #f)
  (writers		:init-keyword :writers		:init-value '())
  (filter		:init-keyword :filter		:init-value #f)
  (customs		:init-keyword :custom		:init-value '())
  (symbol-table	:init-keyword :symbol-table	:init-value '()))


(define (engine? obj)
  (is-a? obj <engine>))

(define (engine-ident obj)
  (slot-ref obj 'ident))

(define (engine-format obj)
  (slot-ref obj 'format))

(define (engine-customs obj)
  (slot-ref obj 'customs))

(define (engine-filter obj)
  (slot-ref obj 'filter))

(define (engine-symbol-table obj)
  (slot-ref obj 'symbol-table))



;;;
;;; Default engines.
;;;

(define *engines*		'())
(define *default-engine*	#f)
(define *default-engines*	'())


(define (default-engine)
   *default-engine*)


(define (default-engine-set! e)
  (with-debug 5 'default-engine-set!
     (debug-item "engine=" e)

     (if (not (engine? e))
	 (skribe-error 'default-engine-set! "bad engine ~S" e))
     (set! *default-engine* e)
     (set! *default-engines* (cons e *default-engines*))
     e))


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
	     (else (*current-engine*)))))
    (if (not (engine? e))
	(skribe-error 'engine-format? "no engine" e)
	(string=? fmt (engine-format e)))))

;;;
;;; MAKE-ENGINE
;;;
(define* (make-engine ident :key (version 'unspecified)
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
(define* (copy-engine ident e :key (version 'unspecified)
				  (filter #f)
				  (delegate #f)
				  (symbol-table #f)
				  (custom #f))
  (let ((new (shallow-clone e)))
    (slot-set! new 'ident	 ident)
    (slot-set! new 'version	 version)
    (slot-set! new 'filter	 (or filter (slot-ref e 'filter)))
    (slot-set! new 'delegate	 (or delegate (slot-ref e 'delegate)))
    (slot-set! new 'symbol-table (or symbol-table (slot-ref e 'symbol-table)))
    (slot-set! new 'customs	 (or custom (slot-ref e 'customs)))

    (set! *engines* (cons new *engines*))
    new))


;;;
;;;	FIND-ENGINE
;;;

(define* (lookup-engine id :key (version 'unspecified))
  "Look for an engine named @var{name} (a symbol) in the @code{(skribilo
engine)} module hierarchy.  If no such engine was found, an error is raised,
otherwise the requested engine is returned."
  (with-debug 5 'lookup-engine
     (debug-item "id=" id " version=" version)

     (let* ((engine (symbol-append id '-engine))
	    (m (resolve-module `(skribilo engine ,id))))
       (if (module-bound? m engine)
	   (module-ref m engine)
	   (error "no such engine" id)))))

(define* (find-engine id :key (version 'unspecified))
  (false-if-exception (apply lookup-engine (list id version))))



;;;
;;; Engine methods.
;;;

(define (engine-custom e id)
  (let* ((customs (slot-ref e 'customs))
	 (c       (assq id customs)))
    (if (pair? c)
	(cadr c)
	'unspecified)))


(define (engine-custom-set! e id val)
  (let* ((customs (slot-ref e 'customs))
	 (c       (assq id customs)))
    (if (pair? c)
	(set-car! (cdr c) val)
	(slot-set! e 'customs (cons (list id val) customs)))))


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
  (if (not (is-a? e <engine>))
      (skribe-error ident "Illegal engine" e))

  ;; check the options
  (if (not (or (eq? opt 'all) (list? opt)))
      (skribe-error ident "Illegal options" opt))

  ;; check the correctness of the predicate
  (check-procedure "predicate" pred 2)

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
    (slot-set! e 'writers (cons n (slot-ref e 'writers)))
    n))



;;;
;;; Current engine.
;;;

;;; `(skribilo module)' must be loaded before the first `find-engine' call.
(use-modules (skribilo module))

;; At this point, we're almost done with the bootstrap process.
(format #t "base engine: ~a~%" (lookup-engine 'base))

(define *current-engine*
  ;; By default, use the HTML engine.
  (make-parameter (lookup-engine 'html)
		  (lambda (val)
		    (cond ((symbol? val) (lookup-engine val))
			  ((engine? val) val)
			  (else
			   (error "invalid value for `*current-engine*'"
				  val))))))


;;; engine.scm ends here
