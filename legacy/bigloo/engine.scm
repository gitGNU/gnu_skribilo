;*=====================================================================*/
;*    serrano/prgm/project/skribe/src/bigloo/engine.scm                */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Sep  9 08:01:30 2003                          */
;*    Last change :  Fri May 21 16:12:32 2004 (serrano)                */
;*    Copyright   :  2003-04 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Skribe engines                                                   */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module skribe_engine
   
   (option  (set! dsssl-symbol->keyword 
		  (lambda (s)
		     (string->keyword
		      (string-append ":" (symbol->string s))))))
   
   (include "debug.sch")
   
   (import  skribe_types
	    skribe_eval
	    skribe_param
	    skribe_output)
   
   (export  (make-engine::%engine ::symbol #!key v fmt in fi cu st if)
	    (copy-engine::%engine ::symbol ::%engine #!key v in fi cu st)
	    (find-engine ::symbol #!key version)

	    (default-engine::obj)
	    (default-engine-set! ::%engine)
	    (push-default-engine ::%engine)
	    (pop-default-engine)

	    (processor-get-engine ::obj ::obj ::%engine)
	    
	    (engine-format? ::bstring . e)

	    (engine-custom::obj ::%engine ::symbol)
	    (engine-custom-set! ::%engine ::symbol ::obj)

	    (engine-add-writer! ::%engine ::obj ::procedure ::obj
				::obj ::obj ::obj ::obj ::obj ::obj)))
   
;*---------------------------------------------------------------------*/
;*    *engines* ...                                                    */
;*---------------------------------------------------------------------*/
(define *engines* '())

;*---------------------------------------------------------------------*/
;*    *default-engine* ...                                             */
;*---------------------------------------------------------------------*/
(define *default-engine* #f)
(define *default-engines* '())

;*---------------------------------------------------------------------*/
;*    default-engine-set! ...                                          */
;*---------------------------------------------------------------------*/
(define (default-engine-set! e)
   (if (not (engine? e))
       (skribe-type-error 'default-engine-set! "engine" e (find-runtime-type e))
       (begin
	  (set! *default-engine* e)
	  (set! *default-engines* (cons *default-engine* *default-engines*))
	  e)))

;*---------------------------------------------------------------------*/
;*    default-engine ...                                               */
;*---------------------------------------------------------------------*/
(define (default-engine)
   *default-engine*)

;*---------------------------------------------------------------------*/
;*    push-default-engine ...                                          */
;*---------------------------------------------------------------------*/
(define (push-default-engine e)
   (set! *default-engines* (cons e *default-engines*))
   (default-engine-set! e))

;*---------------------------------------------------------------------*/
;*    pop-default-engine ...                                           */
;*---------------------------------------------------------------------*/
(define (pop-default-engine)
   (if (null? *default-engines*)
       (skribe-error 'pop-default-engine "Empty engine stack" '())
       (begin
	  (set! *default-engines* (cdr *default-engines*))
	  (if (pair? *default-engines*)
	      (default-engine-set! (car *default-engines*))
	      (set! *default-engine* #f)))))

;*---------------------------------------------------------------------*/
;*    processor-get-engine ...                                         */
;*---------------------------------------------------------------------*/
(define (processor-get-engine combinator newe olde)
   (cond
      ((procedure? combinator)
       (combinator newe olde))
      ((engine? newe)
       newe)
      (else
       olde)))

;*---------------------------------------------------------------------*/
;*    engine-format? ...                                               */
;*---------------------------------------------------------------------*/
(define (engine-format? fmt . e)
   (let ((e (cond
	       ((pair? e) (car e))
	       ((%engine? *skribe-engine*) *skribe-engine*)
	       (else (find-engine *skribe-engine*)))))
      (if (not (%engine? e))
	  (skribe-error 'engine-format? "No engine" e)
	  (string=? fmt (%engine-format e)))))

;*---------------------------------------------------------------------*/
;*    make-engine ...                                                  */
;*---------------------------------------------------------------------*/
(define (make-engine ident
		     #!key
		     (version #unspecified)
		     (format "raw")
		     (filter #f)
		     (delegate #f)
		     (symbol-table '())
		     (custom '())
		     (info '()))
   (let ((e (instantiate::%engine
	       (ident ident)
	       (version version)
	       (format format)
	       (filter filter)
	       (delegate delegate)
	       (symbol-table symbol-table)
	       (customs custom)
	       (info info))))
      ;; store the engine in the global table
      (set! *engines* (cons e *engines*))
      ;; return it
      e))

;*---------------------------------------------------------------------*/
;*    copy-engine ...                                                  */
;*---------------------------------------------------------------------*/
(define (copy-engine ident
		     e
		     #!key
		     (version #unspecified)
		     (filter #f)
		     (delegate #f)
		     (symbol-table #f)
		     (custom #f))
   (let ((e (duplicate::%engine e
	       (ident ident)
	       (version version)
	       (filter (or filter (%engine-filter e)))
	       (delegate (or delegate (%engine-delegate e)))
	       (symbol-table (or symbol-table (%engine-symbol-table e)))
	       (customs (or custom (%engine-customs e))))))
      (set! *engines* (cons e *engines*))
      e))

;*---------------------------------------------------------------------*/
;*    find-loaded-engine ...                                           */
;*---------------------------------------------------------------------*/
(define (find-loaded-engine id version)
   (let loop ((es *engines*))
      (cond
	 ((null? es)
	  #f)
	 ((eq? (%engine-ident (car es)) id)
	  (cond
	     ((eq? version #unspecified)
	      (car es))
	     ((eq? version (%engine-version (car es)))
	      (car es))
	     (else
	      (loop (cdr es)))))
	 (else
	  (loop (cdr es))))))

;*---------------------------------------------------------------------*/
;*    find-engine ...                                                  */
;*---------------------------------------------------------------------*/
(define (find-engine id #!key (version #unspecified))
   (with-debug 5 'find-engine
      (debug-item "id=" id " version=" version)
      (or (find-loaded-engine id version)
	  (let ((c (assq id *skribe-auto-load-alist*)))
	     (debug-item "c=" c)
	     (if (and (pair? c) (string? (cdr c)))
		 (begin
		    (skribe-load (cdr c) :engine 'base)
		    (find-loaded-engine id version))
		 #f)))))

;*---------------------------------------------------------------------*/
;*    engine-custom ...                                                */
;*---------------------------------------------------------------------*/
(define (engine-custom e id)
   (with-access::%engine e (customs)
      (let ((c (assq id customs)))
	 (if (pair? c)
	     (cadr c)
	     #unspecified))))

;*---------------------------------------------------------------------*/
;*    engine-custom-set! ...                                           */
;*---------------------------------------------------------------------*/
(define (engine-custom-set! e id val)
   (with-access::%engine e (customs)
      (let ((c (assq id customs)))
	 (if (pair? c)
	     (set-car! (cdr c) val)
	     (set! customs (cons (list id val) customs))))))

;*---------------------------------------------------------------------*/
;*    engine-add-writer! ...                                           */
;*---------------------------------------------------------------------*/
(define (engine-add-writer! e id pred upred opt before action after class va)
   ;; check the arity of a procedure
   (define (check-procedure name proc arity)
      (cond
	 ((not (procedure? proc))
	  (skribe-error id "Illegal procedure" proc))
	 ((not (correct-arity? proc arity))
	  (skribe-error id
			(string-append "Illegal `" name "'procedure")
			proc))))
   (define (check-output name proc)
      (and proc (or (string? proc) (check-procedure name proc 2))))
   ;; check the engine
   (if (not (engine? e))
       (skribe-error id "Illegal engine" e))
   ;; check the options
   (if (not (or (eq? opt 'all) (list? opt)))
       (skribe-error id "Illegal options" opt))
   ;; check the correctness of the predicate and the validator
   (check-procedure "predicate" pred 2)
   (when va (check-procedure "validate" va 2))
   ;; check the correctness of the three actions
   (check-output "before" before)
   (check-output "action" action)
   (check-output "after" after)
   ;; create a new writer...
   (let ((n (instantiate::%writer
	       (ident (if (symbol? id) id 'all))
	       (class class)
	       (pred pred)
	       (upred upred)
	       (options opt)
	       (before before)
	       (action action)
	       (after after)
	       (validate va))))
      ;; ...and bind it
      (with-access::%engine e (writers)
	 (set! writers (cons n writers))
	 n)))
