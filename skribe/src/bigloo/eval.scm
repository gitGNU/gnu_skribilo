;*=====================================================================*/
;*    serrano/prgm/project/skribe/src/bigloo/eval.scm                  */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Jul 23 12:48:11 2003                          */
;*    Last change :  Wed May 18 15:52:01 2005 (serrano)                */
;*    Copyright   :  2003-05 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    The Skribe evaluator                                             */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module skribe_eval

   (option  (set! dsssl-symbol->keyword 
		  (lambda (s)
		     (string->keyword
		      (string-append ":" (symbol->string s))))))

   (include "debug.sch")
   
   (import skribe_param
	   skribe_types
	   skribe_resolve
	   skribe_verify
	   skribe_output
	   skribe_read
	   skribe_lib
	   skribe_engine)

   (export (skribe-eval-location)
	   (skribe-error ::obj ::obj ::obj)
	   (skribe-type-error ::obj ::obj ::obj ::bstring)
	   (skribe-warning ::int . obj)
	   (skribe-warning/ast ::int ::%ast . obj)
	   (skribe-message ::bstring . obj)
	   (skribe-load ::bstring #!rest opt #!key engine path)
	   (skribe-load-options)
	   (skribe-include ::bstring . rest)
	   (skribe-open-bib-file ::bstring ::obj)
	   (skribe-eval-port ::input-port ::obj #!key env)
	   (skribe-eval ::obj ::%engine #!key env)
	   (skribe-path::pair-nil)
	   (skribe-path-set! ::obj)
	   (skribe-image-path::pair-nil)
	   (skribe-image-path-set! ::obj)
	   (skribe-bib-path::pair-nil)
	   (skribe-bib-path-set! ::obj)
	   (skribe-source-path::pair-nil)
	   (skribe-source-path-set! ::obj)))

;*---------------------------------------------------------------------*/
;*    skribe-eval-location ...                                         */
;*---------------------------------------------------------------------*/
(define (skribe-eval-location)
   (evmeaning-location))

;*---------------------------------------------------------------------*/
;*    skribe-error ...                                                 */
;*---------------------------------------------------------------------*/
(define (skribe-error proc msg obj)
   (if (ast? obj)
       (skribe-ast-error proc msg obj)
       (error/evloc proc msg obj)))

;*---------------------------------------------------------------------*/
;*    skribe-type-error ...                                            */
;*---------------------------------------------------------------------*/
(define (skribe-type-error proc msg obj etype)
   (let ((ty (if (%markup? obj)
		 (format "~a#~a" (markup-markup obj) (markup-ident obj))
		 (find-runtime-type obj))))
      (skribe-error proc
		    (bigloo-type-error-msg msg etype ty)
		    obj)))

;*---------------------------------------------------------------------*/
;*    skribe-ast-error ...                                             */
;*---------------------------------------------------------------------*/
(define (skribe-ast-error proc msg obj)
   (let ((l (ast-loc obj))
	 (shape (if (%markup? obj)
		    (%markup-markup obj)
		    (find-runtime-type obj))))
      (if (location? l)
	  (error/location proc msg shape (location-file l) (location-pos l))
	  (error/evloc proc msg shape))))

;*---------------------------------------------------------------------*/
;*    error/evloc ...                                                  */
;*---------------------------------------------------------------------*/
(define (error/evloc proc msg obj)
   (let ((l (evmeaning-location)))
      (if (location? l)
	  (error/location proc msg obj (location-file l) (location-pos l))
	  ((begin error) proc msg obj))))

;*---------------------------------------------------------------------*/
;*    skribe-warning ...                                               */
;*---------------------------------------------------------------------*/
(define (skribe-warning level . obj)
   (if (>= *skribe-warning* level)
       (let ((l (evmeaning-location)))
	  (if (location? l)
	      (apply warning/location (location-file l) (location-pos l) obj)
	      (apply warning obj)))))

;*---------------------------------------------------------------------*/
;*    skribe-warning/ast ...                                           */
;*---------------------------------------------------------------------*/
(define (skribe-warning/ast level ast . obj)
   (if (>= *skribe-warning* level)
       (let ((l (%ast-loc ast)))
	  (if (location? l)
	      (apply warning/location (location-file l) (location-pos l) obj)
	      (apply skribe-warning level obj)))))

;*---------------------------------------------------------------------*/
;*    skribe-message ...                                               */
;*---------------------------------------------------------------------*/
(define (skribe-message fmt . obj)
   (if (> *skribe-verbose* 0)
       (apply fprintf (current-error-port) fmt obj)))

;*---------------------------------------------------------------------*/
;*    *skribe-loaded* ...                                              */
;*    -------------------------------------------------------------    */
;*    This hash table stores the list of loaded files in order         */
;*    to avoid one file to be loaded twice.                            */
;*---------------------------------------------------------------------*/
(define *skribe-loaded* (make-hashtable))

;*---------------------------------------------------------------------*/
;*    *skribe-load-options* ...                                        */
;*---------------------------------------------------------------------*/
(define *skribe-load-options* '())

;*---------------------------------------------------------------------*/
;*    skribe-load ...                                                  */
;*---------------------------------------------------------------------*/
(define (skribe-load file #!rest opt #!key engine path)
   (with-debug 4 'skribe-load
      (debug-item "  engine=" engine)
      (debug-item "  path=" path)
      (debug-item "  opt" opt)
      (let* ((ei (cond
		    ((not engine)
		     *skribe-engine*)
		    ((engine? engine)
		     engine)
		    ((not (symbol? engine))
		     (skribe-error 'skribe-load "Illegal engine" engine))
		    (else
		     engine)))
	     (path (cond
		      ((not path)
		       (skribe-path))
		      ((string? path)
		       (list path))
		      ((not (and (list? path) (every? string? path)))
		       (skribe-error 'skribe-load "Illegal path" path))
		      (else
		       path)))
	     (filep (find-file/path file path)))
	 (set! *skribe-load-options* opt)
	 (if (and (string? filep) (file-exists? filep))
	     (if (not (hashtable-get *skribe-loaded* filep))
		 (begin
		    (hashtable-put! *skribe-loaded* filep #t)
		    (cond
		       ((>fx *skribe-verbose* 1)
			(fprint (current-error-port)
				"  [loading file: " filep " " opt "]"))
		       ((>fx *skribe-verbose* 0)
			(fprint (current-error-port)
				"  [loading file: " filep "]")))
		    (with-input-from-file filep
		       (lambda ()
			  (skribe-eval-port (current-input-port) ei)))))
	     (skribe-error 'skribe-load
			   (format "Can't find file `~a' in path" file)
			   path)))))

;*---------------------------------------------------------------------*/
;*    skribe-load-options ...                                          */
;*---------------------------------------------------------------------*/
(define (skribe-load-options)
   *skribe-load-options*)

;*---------------------------------------------------------------------*/
;*    evaluate ...                                                     */
;*---------------------------------------------------------------------*/
(define (evaluate exp)
   (try (eval exp)
	(lambda (a p m o)
	   (evmeaning-notify-error p m o)
	   (flush-output-port (current-error-port)))))

;*---------------------------------------------------------------------*/
;*    skribe-include ...                                               */
;*---------------------------------------------------------------------*/
(define (skribe-include file . rest)
   (let* ((path (cond
		   ((or (null? rest) (null? (cdr rest)))
		    (skribe-path))
		   ((not (every? string? (cdr rest)))
		    (skribe-error 'skribe-include "Illegal path" (cdr rest)))
		   (else
		    (cdr rest))))
	  (filep (find-file/path file (if (null? path) (skribe-path) path))))
      (if (and (string? filep) (file-exists? filep))
	  (begin
	     (if (>fx *skribe-verbose* 0)
		 (fprint (current-error-port)
			 "  [including file: " filep "]"))
	     (with-input-from-file filep
		(lambda ()
		   (let loop ((exp (skribe-read (current-input-port)))
			      (res '()))
		      (if (eof-object? exp)
			  (if (and (pair? res) (null? (cdr res)))
			      (car res)
			      (reverse! res))
			  (loop (skribe-read (current-input-port))
				(cons (evaluate exp) res)))))))
	  (skribe-error 'skribe-include
			(format "Can't find file `~a 'in path" file)
			path))))

;*---------------------------------------------------------------------*/
;*    skribe-open-bib-file ...                                         */
;*---------------------------------------------------------------------*/
(define (skribe-open-bib-file file command)
   (let ((filep (find-file/path file *skribe-bib-path*)))
      (if (string? filep)
	  (begin
	     (if (>fx *skribe-verbose* 0)
		 (fprint (current-error-port) "  [loading bib: " filep "]"))
	     (open-input-file (if (string? command)
				  (string-append "| "
						 (format command filep))
				  filep)))
	  (begin
	     (skribe-warning 1
			     'bibliography
			     "Can't find bibliography -- " file)
	     #f))))
      
;*---------------------------------------------------------------------*/
;*    skribe-eval-port ...                                             */
;*---------------------------------------------------------------------*/
(define (skribe-eval-port port ei #!key (env '()))
   (with-debug 2 'skribe-eval-port
      (debug-item "ei=" ei)
      (let ((e (if (symbol? ei) (find-engine ei) ei)))
	 (debug-item "e=" e)
	 (if (not (%engine? e))
	     (skribe-error 'find-engine "Can't find engine" ei)
	     (let loop ((exp (skribe-read port)))
		(with-debug 10 'skribe-eval-port
		   (debug-item "exp=" exp))
		(if (not (eof-object? exp))
		    (begin
		       (skribe-eval (evaluate exp) e :env env)
		       (loop (skribe-read port)))))))))

;*---------------------------------------------------------------------*/
;*    skribe-eval ...                                                  */
;*---------------------------------------------------------------------*/
(define (skribe-eval a e #!key (env '()))
   (with-debug 2 'skribe-eval
      (debug-item "a=" a " e=" (%engine-ident e))
      (let ((a2 (resolve! a e env)))
	 (debug-item "resolved a=" a)
	 (let ((a3 (verify a2 e)))
	    (debug-item "verified a=" a3)
	    (output a3 e)))))

;*---------------------------------------------------------------------*/
;*    skribe-path ...                                                  */
;*---------------------------------------------------------------------*/
(define (skribe-path)
   *skribe-path*)

;*---------------------------------------------------------------------*/
;*    skribe-path-set! ...                                             */
;*---------------------------------------------------------------------*/
(define (skribe-path-set! path)
   (if (not (and (list? path) (every? string? path)))
       (skribe-error 'skribe-path-set! "Illegal path" path)
       (set! *skribe-path* path)))

;*---------------------------------------------------------------------*/
;*    skribe-image-path ...                                            */
;*---------------------------------------------------------------------*/
(define (skribe-image-path)
   *skribe-image-path*)

;*---------------------------------------------------------------------*/
;*    skribe-image-path-set! ...                                       */
;*---------------------------------------------------------------------*/
(define (skribe-image-path-set! path)
   (if (not (and (list? path) (every? string? path)))
       (skribe-error 'skribe-image-path-set! "Illegal path" path)
       (set! *skribe-image-path* path)))

;*---------------------------------------------------------------------*/
;*    skribe-bib-path ...                                              */
;*---------------------------------------------------------------------*/
(define (skribe-bib-path)
   *skribe-bib-path*)

;*---------------------------------------------------------------------*/
;*    skribe-bib-path-set! ...                                         */
;*---------------------------------------------------------------------*/
(define (skribe-bib-path-set! path)
   (if (not (and (list? path) (every? string? path)))
       (skribe-error 'skribe-bib-path-set! "Illegal path" path)
       (set! *skribe-bib-path* path)))

;*---------------------------------------------------------------------*/
;*    skribe-source-path ...                                           */
;*---------------------------------------------------------------------*/
(define (skribe-source-path)
   *skribe-source-path*)

;*---------------------------------------------------------------------*/
;*    skribe-source-path-set! ...                                      */
;*---------------------------------------------------------------------*/
(define (skribe-source-path-set! path)
   (if (not (and (list? path) (every? string? path)))
       (skribe-error 'skribe-source-path-set! "Illegal path" path)
       (set! *skribe-source-path* path)))
