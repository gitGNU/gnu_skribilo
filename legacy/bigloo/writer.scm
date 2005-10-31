;*=====================================================================*/
;*    serrano/prgm/project/skribe/src/bigloo/writer.scm                */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Sep  9 06:19:57 2003                          */
;*    Last change :  Tue Nov  2 14:33:59 2004 (serrano)                */
;*    Copyright   :  2003-04 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Skribe writer management                                         */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module skribe_writer
   
   (option  (set! dsssl-symbol->keyword 
		  (lambda (s)
		     (string->keyword
		      (string-append ":" (symbol->string s))))))

   (include "debug.sch")
   
   (import  skribe_types
	    skribe_eval
	    skribe_param
	    skribe_engine
	    skribe_output
	    skribe_lib)
   
   (export  (invoke proc node e)

	    (lookup-markup-writer ::%markup ::%engine)
	    
	    (markup-writer ::obj #!optional e #!key p class opt va bef aft act)
	    (copy-markup-writer ::obj ::obj #!optional e #!key p c o v b ac a)
	    (markup-writer-get ::obj #!optional e #!key class pred)
	    (markup-writer-get*::pair-nil ::obj #!optional e #!key class)))
	    
;*---------------------------------------------------------------------*/
;*    invoke ...                                                       */
;*---------------------------------------------------------------------*/
(define (invoke proc node e)
   (let ((id (if (markup? node)
		   (string->symbol
		    (format "~a#~a"
			    (%engine-ident e)
			    (%markup-markup node)))
		   (%engine-ident e))))
      (with-push-trace id
         (with-debug 5 'invoke
	    (debug-item "e=" (%engine-ident e))
	    (debug-item "node=" (find-runtime-type node)
			" " (if (markup? node) (%markup-markup node) ""))
	    (if (string? proc)
		(display proc)
		(if (procedure? proc)
		    (proc node e)))))))

;*---------------------------------------------------------------------*/
;*    lookup-markup-writer ...                                         */
;*---------------------------------------------------------------------*/
(define (lookup-markup-writer node e)
   (with-access::%engine e (writers delegate)
      (let loop ((w* writers))
	 (cond
	    ((pair? w*)
	     (with-access::%writer (car w*) (pred)
		(if (pred node e)
		    (car w*)
		    (loop (cdr w*)))))
	    ((engine? delegate)
	     (lookup-markup-writer node delegate))
	    (else
	     #f)))))

;*---------------------------------------------------------------------*/
;*    make-writer-predicate ...                                        */
;*---------------------------------------------------------------------*/
(define (make-writer-predicate markup predicate class)
   (let* ((t1 (if (symbol? markup)
		  (lambda (n e) (is-markup? n markup))
		  (lambda (n e) #t)))
	  (t2 (if class
		  (lambda (n e)
		     (and (t1 n e) (equal? (%markup-class n) class)))
		  t1)))
      (if predicate
	  (cond
	     ((not (procedure? predicate))
	      (skribe-error 'markup-writer
			    "Illegal predicate (procedure expected)"
			    predicate))
	     ((not (correct-arity? predicate 2))
	      (skribe-error 'markup-writer
			    "Illegal predicate arity (2 arguments expected)"
			    predicate))
	     (else
	      (lambda (n e)
		 (and (t2 n e) (predicate n e)))))
	  t2)))

;*---------------------------------------------------------------------*/
;*    markup-writer ...                                                */
;*---------------------------------------------------------------------*/
(define (markup-writer markup
		       #!optional
		       engine
		       #!key
		       (predicate #f)
		       (class #f)
		       (options '())
		       (validate #f)
		       (before #f)
		       (action #unspecified)
		       (after #f))
   (let ((e (or engine (default-engine))))
      (cond
 	 ((and (not (symbol? markup)) (not (eq? markup #t)))
	  (skribe-error 'markup-writer "Illegal markup" markup))
	 ((not (engine? e))
	  (skribe-error 'markup-writer "Illegal engine" e))
	 ((and (not predicate)
	       (not class)
	       (null? options)
	       (not before)
	       (eq? action #unspecified)
	       (not after))
	  (skribe-error 'markup-writer "Illegal writer" markup))
	 (else
	  (let ((m (make-writer-predicate markup predicate class))
		(ac (if (eq? action #unspecified)
			 (lambda (n e)
			    (output (markup-body n) e))
			 action)))
	     (engine-add-writer! e markup m predicate
				 options before ac after class validate))))))

;*---------------------------------------------------------------------*/
;*    copy-markup-writer ...                                           */
;*---------------------------------------------------------------------*/
(define (copy-markup-writer markup old-engine
			    #!optional new-engine
			    #!key
			    (predicate #unspecified) 
			    (class #unspecified) 
			    (options #unspecified)
			    (validate #unspecified) 
			    (before #unspecified) 
			    (action #unspecified) 
			    (after #unspecified))
   (let ((old (markup-writer-get markup old-engine))
	 (new-engine (or new-engine old-engine)))
      (markup-writer markup new-engine
		     :pred (if (unspecified? predicate)
			       (%writer-pred old)
			       predicate)
		     :class (if (unspecified? class)
				(%writer-class old)
				class)
		     :options (if (unspecified? options)
				  (%writer-options old)
				  options)
		     :validate (if (unspecified? validate)
				   (%writer-validate old)
				   validate)
		     :before (if (unspecified? before)
				 (%writer-before old)
				 before)
		     :action (if (unspecified? action)
				 (%writer-action old)
				 action)
		     :after (if (unspecified? after)
				(%writer-after old) after))))

;*---------------------------------------------------------------------*/
;*    markup-writer-get ...                                            */
;*    -------------------------------------------------------------    */
;*    Finds the writer that matches MARKUP with optional CLASS         */
;*    attribute.                                                       */
;*---------------------------------------------------------------------*/
(define (markup-writer-get markup #!optional engine #!key (class #f) (pred #f))
   (let ((e (or engine (default-engine))))
      (cond
	 ((not (symbol? markup))
	  (skribe-error 'markup-writer "Illegal symbol" markup))
	 ((not (engine? e))
	  (skribe-error 'markup-writer "Illegal engine" e))
	 (else
	  (let liip ((e e))
	     (let loop ((w* (%engine-writers e)))
		(cond
		   ((pair? w*)
		    (if (and (eq? (%writer-ident (car w*)) markup)
			     (equal? (%writer-class (car w*)) class)
			     (or (eq? pred #unspecified)
				 (eq? (%writer-upred (car w*)) pred)))
			(car w*)
			(loop (cdr w*))))
		   ((engine? (%engine-delegate e))
		    (liip (%engine-delegate e)))
		   (else
		    #f))))))))

;*---------------------------------------------------------------------*/
;*    markup-writer-get* ...                                           */
;*    -------------------------------------------------------------    */
;*    Finds alll writers that matches MARKUP with optional CLASS       */
;*    attribute.                                                       */
;*---------------------------------------------------------------------*/
(define (markup-writer-get* markup #!optional engine #!key (class #f))
   (let ((e (or engine (default-engine))))
      (cond
	 ((not (symbol? markup))
	  (skribe-error 'markup-writer "Illegal symbol" markup))
	 ((not (engine? e))
	  (skribe-error 'markup-writer "Illegal engine" e))
	 (else
	  (let liip ((e e)
		     (res '()))
	     (let loop ((w* (%engine-writers e))
			(res res))
		(cond
		   ((pair? w*)
		    (if (and (eq? (%writer-ident (car w*)) markup)
			     (equal? (%writer-class (car w*)) class))
			(loop (cdr w*) (cons (car w*) res))
			(loop (cdr w*) res)))
		   ((engine? (%engine-delegate e))
		    (liip (%engine-delegate e) res))
		   (else
		    (reverse! res)))))))))
