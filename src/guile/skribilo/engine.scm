;;; engine.scm	-- Skribilo engines.
;;; -*- coding: iso-8859-1 -*-
;;;
;;; Copyright 2005, 2007, 2008, 2009, 2010  Ludovic Courtès <ludo@gnu.org>
;;; Copyright 2003, 2004  Erick Gallesio - I3S-CNRS/ESSI <eg@essi.fr>
;;;
;;;
;;; This file is part of Skribilo.
;;;
;;; Skribilo is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; Skribilo is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with Skribilo.  If not, see <http://www.gnu.org/licenses/>.

(define-module (skribilo engine)
  :use-module (skribilo debug)
  :use-module (skribilo utils syntax)
  :use-module (skribilo condition)

  :use-module (oop goops)
  :use-module (ice-9 optargs)
  :use-module (ice-9 format)

  :autoload   (srfi srfi-34)  (raise guard)
  :use-module (srfi srfi-35)
  :autoload   (srfi srfi-39)  (make-parameter)

  :export (<engine> engine? engine-ident engine-format
		    engine-customs engine-filter engine-symbol-table

	   *current-engine*
	   default-engine default-engine-set!
	   make-engine copy-engine find-engine lookup-engine
	   engine-custom engine-custom-set! engine-custom-add!
	   engine-format?
	   processor-get-engine
	   push-default-engine pop-default-engine

	   engine-loaded? when-engine-is-loaded

           &engine-error &unknown-engine-error
           engine-error? unknown-engine-error?
           unknown-engine-error:engine-name))


(skribilo-module-syntax)


;;;
;;; Error conditions.
;;;

(define-condition-type &engine-error &skribilo-error
  engine-error?)

(define-condition-type &unknown-engine-error &engine-error
  unknown-engine-error?
  (engine-name unknown-engine-error:engine-name))



(define (handle-engine-error c)
  ;; Issue a user-friendly error message for error condition C.
  (cond ((unknown-engine-error? c)
         (format (current-error-port)
                 (_ "unknown engine `~a'~%")
                 (unknown-engine-error:engine-name c)))

	(else
	 (format (current-error-port)
                 (_ "undefined engine error: ~A~%")
		 c))))

(register-error-condition-handler! engine-error? handle-engine-error)


;;;
;;; Class definition.
;;;

;; Note on writers
;; ---------------
;;
;; `writers' here is an `eq?' hash table where keys are markup names
;; (symbols) and values are lists of markup writers (most of the time, the
;; list will only contain one writer).  Each of these writer may define a
;; predicate or class that may further restrict its applicability.
;;
;; `free-writers' is a list of writers that may apply to *any* kind of
;; markup.  These are typically define by passing `#t' to `markup-writer'
;; instead of a symbol:
;;
;;   (markup-writer #f (find-engine 'xml)
;;     :before ...
;;     ...)
;;
;; The XML engine contains an example of such free writers.  Again, these
;; writers may define a predicate or a class restricting their applicability.
;;
;; The distinction between these two kinds of writers is mostly performance:
;; "free writers" are rarely used and markup-specific are the most common
;; case which we want to be fast.  Therefore, for the latter case, we can't
;; afford traversing a list of markups, evaluating each and every markup
;; predicate.
;;
;; For more details, see `markup-writer-get' and `lookup-markup-writer' in
;; `(skribilo writer)'.

(define-class <engine> ()
  (ident		:init-keyword :ident		:init-value '???)
  (format		:init-keyword :format		:init-value "raw")
  (info		        :init-keyword :info		:init-value '())
  (version		:init-keyword :version
			:init-value 'unspecified)
  (delegate		:init-keyword :delegate		:init-value #f)
  (writers              :init-thunk make-hash-table)
  (free-writers         :init-value '())
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

(define-method (write (e <engine>) (p <port>))
  (format p "#<~a ~a ~x>"
          (class-name (class-of e))
          (engine-ident e)
          (object-address e)))



;;;
;;; Default engines.
;;;

(define *default-engine*	#f)
(define *default-engines*	'())


(define (default-engine)
   *default-engine*)


(define (default-engine-set! e)
  (with-debug 5 'default-engine-set!
     (debug-item "engine=" e)

     (if (not (engine? e))
	 (raise (condition (&invalid-argument-error
                            (proc-name 'default-engine-set!)
                            (argument  e)))))
     (set! *default-engine* e)
     (set! *default-engines* (cons e *default-engines*))
     e))


(define (push-default-engine e)
   (set! *default-engines* (cons e *default-engines*))
   (default-engine-set! e))

(define (pop-default-engine)
   (if (null? *default-engines*)
       (raise (condition (&invalid-argument-error
                          (proc-name 'pop-default-engine)
                          (argument  *default-engines*))))
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
        (raise (condition (&invalid-argument-error
                           (proc-name 'engine-format?)
                           (argument  e))))
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

    ;; XXX: We don't use `list-copy' here because writer lists are only
    ;; consed, never mutated.

    ;(slot-set! new 'free-writers (list-copy (slot-ref e 'free-writers)))

    (let ((new-writers (make-hash-table)))
      (hash-for-each (lambda (m w*)
		       (hashq-set! new-writers m w*))
		     (slot-ref e 'writers))
      (slot-set! new 'writers new-writers))

    new))



;;;
;;; Engine loading.
;;;

;; Each engine is to be stored in its own module with the `(skribilo engine)'
;; hierarchy.  The `engine-id->module-name' procedure returns this module
;; name based on the engine name.

(define (engine-id->module-name id)
  `(skribilo engine ,id))

(define (engine-loaded? id)
  "Check whether engine @var{id} is already loaded."
  ;; Trick taken from `resolve-module' in `boot-9.scm'.
  (cond-expand (guile-2
                (nested-ref-module (resolve-module '() #f)
                                   (engine-id->module-name id)))
               (else
                ;; This method works for 1.8 but is deprecated in 1.9/2.0 and
                ;; doesn't work with 1.9.11 anyway.
                (nested-ref the-root-module
                            `(%app modules ,@(engine-id->module-name id))))))

;; A mapping of engine names to hooks.
(define %engine-load-hook (make-hash-table))

(define (consume-load-hook! id)
  (with-debug 5 'consume-load-hook!
    (let ((hook (hashq-ref %engine-load-hook id)))
      (if hook
	  (begin
	    (debug-item "running hook " hook " for engine " id)
	    (hashq-remove! %engine-load-hook id)
	    (run-hook hook))))))

(define (when-engine-is-loaded id thunk)
  "Run @var{thunk} only when engine with identifier @var{id} is loaded."
  (if (engine-loaded? id)
      (begin
	;; Maybe the engine had already been loaded via `use-modules'.
	(consume-load-hook! id)
	(thunk))
      (let ((hook (or (hashq-ref %engine-load-hook id)
		      (let ((hook (make-hook)))
			(hashq-set! %engine-load-hook id hook)
			hook))))
	(add-hook! hook thunk))))


(define* (lookup-engine id :key (version 'unspecified))
  "Look for an engine named @var{name} (a symbol) in the @code{(skribilo
engine)} module hierarchy.  If no such engine was found, an error is raised,
otherwise the requested engine is returned."
  (with-debug 5 'lookup-engine
     (debug-item "id=" id " version=" version)

     (let* ((engine (symbol-append id '-engine))
	    (m (resolve-module (engine-id->module-name id))))
       (if (module-bound? m engine)
	   (let ((e (module-ref m engine)))
	     (if e (consume-load-hook! id))
	     e)
	   (raise (condition (&unknown-engine-error
                              (engine-name id))))))))

(define* (find-engine id :key (version 'unspecified))
  (guard (c ((unknown-engine-error? c)
             #f))
    (lookup-engine id :version version)))





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

(define (engine-custom-add! e id val)
   (let ((old (engine-custom e id)))
      (if (eq? old 'unspecified)
	  (engine-custom-set! e id (list val))
	  (engine-custom-set! e id (cons val old)))))



;;;
;;; Current engine.
;;;

(define *current-engine*
  (make-parameter #f
		  (lambda (val)
		    (cond ((symbol? val) (lookup-engine val))
			  ((engine? val) val)
                          ((not val)     val)
			  (else
			   (raise (condition (&invalid-argument-error
                                              (proc-name '*current-engine*)
                                              (argument val)))))))))


;;; engine.scm ends here
