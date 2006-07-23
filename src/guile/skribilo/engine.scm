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
	   engine-custom engine-custom-set! engine-custom-add!
	   engine-format? engine-add-writer!
	   processor-get-engine
	   push-default-engine pop-default-engine

	   engine-loaded? when-engine-is-loaded))


(fluid-set! current-reader %skribilo-module-reader)


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
  (nested-ref the-root-module
	      `(%app modules ,@(engine-id->module-name id))))

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

(define (engine-custom-add! e id val)
   (let ((old (engine-custom e id)))
      (if (unspecified? old)
	  (engine-custom-set! e id (list val))
	  (engine-custom-set! e id (cons val old)))))

(define (engine-add-writer! e ident pred upred opt before action
			    after class valid)
  ;; Add a writer to engine E.  If IDENT is a symbol, then it should denote
  ;; a markup name and the writer being added is specific to that markup.  If
  ;; IDENT is `#t' (for instance), then it is assumed to be a ``free writer''
  ;; that may apply to any kind of markup for which PRED returns true.  The
  ;; order in which writers are added matters (it should be the same as the
  ;; lookup order), hence the use of `append' below.

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
		      (append (hashq-ref writers ident '())
			      (list n))))
	(slot-set! e 'free-writers
		   (append (slot-ref e 'free-writers) (list n))))
    n))



;;;
;;; Current engine.
;;;

;;; `(skribilo module)' must be loaded before the first `find-engine' call.
(use-modules (skribilo module))

;; At this point, we're almost done with the bootstrap process.
;(format #t "base engine: ~a~%" (lookup-engine 'base))

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
