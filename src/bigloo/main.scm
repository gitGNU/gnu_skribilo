;*=====================================================================*/
;*    serrano/prgm/project/skribe/src/bigloo/main.scm                  */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Jul 22 16:51:49 2003                          */
;*    Last change :  Wed May 18 15:45:27 2005 (serrano)                */
;*    Copyright   :  2003-05 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    The Skribe main entry point                                      */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module skribe_main
   
   (include "debug.sch")
   
   (import  skribe_types
	    skribe_parse-args
	    skribe_param
	    skribe_lib
	    skribe_eval
	    skribe_read
	    skribe_engine
	    skribe_evapi)
   
   (main    main))

;*---------------------------------------------------------------------*/
;*    main ...                                                         */
;*---------------------------------------------------------------------*/
(define (main args)
   (with-debug 2 'main
	       (debug-item "parse env variables...")
	       (parse-env-variables)
	       
	       (debug-item "load rc file...")
	       (load-rc)
	       
	       (debug-item "parse command line...")
	       (parse-args args)
	       
	       (debug-item "load base...")
	       (skribe-load "base.skr" :engine 'base)
	       
	       (debug-item "preload... (" *skribe-engine* ")")
	       (for-each (lambda (f)
			    (skribe-load f :engine *skribe-engine*))
			 *skribe-preload*)
	       
	       ;; Load the specified variants
	       (debug-item "variant... (" *skribe-variants* ")")
	       (for-each (lambda (x)
			    (skribe-load (format "~a.skr" x) :engine *skribe-engine*))
			 (reverse! *skribe-variants*))
	       
	       (debug-item "body..." *skribe-engine*)
	       (if (string? *skribe-dest*)
		   (cond-expand
		      (bigloo2.6
		       (try (with-output-to-file *skribe-dest* doskribe)
			    (lambda (e a b c)
			       (delete-file *skribe-dest*)
			       (let ((s (with-output-to-string
					   (lambda () (write c)))))
				  (notify-error a b s))
			       (exit -1))))
		      (else
		       (with-exception-handler
			  (lambda (e)
			     (if (&warning? e)
				 (raise e)
				 (begin
				    (delete-file *skribe-dest*)
				    (if (&error? e)
					(error-notify e)
					(raise e))
				    (exit 1))))
			  (lambda ()
			     (with-output-to-file *skribe-dest* doskribe)))))
		   (doskribe))))

;*---------------------------------------------------------------------*/
;*    doskribe ...                                                     */
;*---------------------------------------------------------------------*/
(define (doskribe)
   (let ((e (find-engine *skribe-engine*)))
      (if (and (engine? e) (pair? *skribe-precustom*))
	  (for-each (lambda (cv)
		       (engine-custom-set! e (car cv) (cdr cv)))
		    *skribe-precustom*))
      (if (pair? *skribe-src*)
	  (for-each (lambda (f) (skribe-load f :engine *skribe-engine*))
		    *skribe-src*)
	  (skribe-eval-port (current-input-port) *skribe-engine*))))
