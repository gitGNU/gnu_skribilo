;*=====================================================================*/
;*    serrano/prgm/project/skribe/src/bigloo/parseargs.scm             */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Jul 22 16:52:53 2003                          */
;*    Last change :  Wed Nov 10 10:57:40 2004 (serrano)                */
;*    Copyright   :  2003-04 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Argument parsing                                                 */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module skribe_parse-args
   
   (include "debug.sch")

   (import  skribe_configure
	    skribe_param
	    skribe_read
	    skribe_types
	    skribe_eval)
   
   (export  (parse-env-variables)
	    (parse-args ::pair)
	    (load-rc)))

;*---------------------------------------------------------------------*/
;*    parse-env-variables ...                                          */
;*---------------------------------------------------------------------*/
(define (parse-env-variables)
   (let ((e (getenv "SKRIBEPATH")))
      (if (string? e)
	  (skribe-path-set! (append (unix-path->list e) (skribe-path))))))

;*---------------------------------------------------------------------*/
;*    parse-args ...                                                   */
;*---------------------------------------------------------------------*/
(define (parse-args args)
   (define (usage args-parse-usage)
      (print "usage: skribe [options] [input]")
      (newline)
      (args-parse-usage #f)
      (newline)
      (print "Rc file:")
      (newline)
      (print "   *skribe-rc* (searched in \".\" then $HOME)")
      (newline)
      (print "Target formats:")
      (for-each (lambda (f) (print "   - " (car f))) *skribe-auto-mode-alist*)
      (newline)
      (print "Shell Variables:")
      (newline)
      (for-each (lambda (var)
		   (print "   - " (car var) " " (cdr var)))
		'(("SKRIBEPATH" . "Skribe input path (all files)"))))
   (define (version)
      (print "skribe v" (skribe-release)))
   (define (query)
      (version)
      (newline)
      (for-each (lambda (x)
		   (let ((s (keyword->string (car x))))
		      (printf "  ~a: ~a\n"
			      (substring s 1 (string-length s))
			      (cadr x))))
		(skribe-configure)))
   (let ((np  '())
	 (engine #f))
      (args-parse (cdr args)
	 ((("-h" "--help") (help "This message"))
	  (usage args-parse-usage)
	  (exit 0))
	 (("--options" (help "Display the skribe options and exit"))
	  (args-parse-usage #t)
	  (exit 0))
	 (("--version" (help "The version of Skribe"))
	  (version)
	  (exit 0))
	 ((("-q" "--query") (help "Display informations about the Skribe configuration"))
	  (query)
	  (exit 0))
	 ((("-c" "--custom") ?key=val (synopsis "Preset custom value"))
	  (let ((l (string-length key=val)))
	     (let loop ((i 0))
		(cond
		   ((= i l)
		    (skribe-error 'skribe "Illegal option" key=val))
		   ((char=? (string-ref key=val i) #\=)
		    (let ((key (substring key=val 0 i))
			  (val (substring key=val (+ i 1) l)))
		       (set! *skribe-precustom*
			     (cons (cons (string->symbol key) val)
				   *skribe-precustom*))))
		   (else
		    (loop (+ i 1)))))))
	 (("-v?level" (help "Increase or set verbosity level (-v0 for crystal silence)"))
	  (if (string=? level "")
	      (set! *skribe-verbose* (+fx 1 *skribe-verbose*))
	      (set! *skribe-verbose* (string->integer level))))
	 (("-w?level" (help "Increase or set warning level (-w0 for crystal silence)"))
	  (if (string=? level "")
	      (set! *skribe-warning* (+fx 1 *skribe-warning*))
	      (set! *skribe-warning* (string->integer level))))
	 (("-g?level" (help "Increase or set debug level"))
	  (if (string=? level "")
	      (set! *skribe-debug* (+fx 1 *skribe-debug*))
	      (let ((l (string->integer level)))
		 (if (= l 0)
		     (begin
			(set! *skribe-debug* 1)
			(set! *skribe-debug-symbols*
			      (cons (string->symbol level)
				    *skribe-debug-symbols*)))
		     (set! *skribe-debug* l)))))
	 (("--no-color" (help "Disable coloring for debug"))
	  (set! *skribe-debug-color* #f))
	 ((("-t" "--target") ?e (help "The output target format"))
	  (set! engine (string->symbol e)))
	 (("-I" ?path (help "Add <path> to skribe path"))
	  (set! np (cons path np)))
	 (("-B" ?path (help "Add <path> to skribe bibliography path"))
	  (skribe-bib-path-set! (cons path (skribe-bib-path))))
	 (("-S" ?path (help "Add <path> to skribe source path"))
	  (skribe-source-path-set! (cons path (skribe-source-path))))
	 (("-P" ?path (help "Add <path> to skribe image path"))
	  (skribe-image-path-set! (cons path (skribe-image-path))))
	 ((("-C" "--split-chapter") ?chapter (help "Emit chapter's sections in separate files"))
	  (set! *skribe-chapter-split* (cons chapter *skribe-chapter-split*)))
	 (("--eval" ?expr (help "Evaluate expression"))
	  (with-input-from-string expr
	     (lambda ()
		(eval (skribe-read)))))
	 (("--no-init-file" (help "Dont load rc Skribe file"))
	  (set! *load-rc* #f))
	 ((("-p" "--preload") ?file (help "Preload file"))
	  (set! *skribe-preload* (cons file *skribe-preload*)))
	 ((("-u" "--use-variant") ?variant (help "use <variant> output format"))
	  (set! *skribe-variants* (cons variant *skribe-variants*)))
	 ((("-o" "--output") ?o (help "The output target name"))
	  (set! *skribe-dest* o)
	  (let* ((s (suffix o))
		 (c (assoc s *skribe-auto-mode-alist*)))
	     (if (and (pair? c) (symbol? (cdr c)))
		 (set! *skribe-engine* (cdr c)))))
	 ((("-b" "--base") ?base (help "The base prefix to be removed from hyperlinks"))
	  (set! *skribe-ref-base* base))
	 ;; skribe rc directory
	 ((("-d" "--rc-dir") ?dir (synopsis "Set the skribe RC directory"))
	  (set! *skribe-rc-directory* dir))
	 (else
	  (set! *skribe-src* (cons else *skribe-src*))))
      ;; we have to configure according to the environment variables
      (if engine (set! *skribe-engine* engine))
      (set! *skribe-src* (reverse! *skribe-src*))
      (skribe-path-set! (append (build-path-from-shell-variable "SKRIBEPATH")
				(reverse! np)
				(skribe-path)))))

;*---------------------------------------------------------------------*/
;*    build-path-from-shell-variable ...                               */
;*---------------------------------------------------------------------*/
(define (build-path-from-shell-variable var)
   (let ((val (getenv var)))
      (if (string? val)
	  (string-case val
	     ((+ (out #\:))
	      (let* ((str (the-string))
		     (res (ignore)))
		 (cons str res)))
	     (#\:
	      (ignore))
	     (else
	      '()))
	  '())))

;*---------------------------------------------------------------------*/
;*    load-rc ...                                                      */
;*---------------------------------------------------------------------*/
(define (load-rc)
   (if *load-rc*
       (let ((file (make-file-name *skribe-rc-directory* *skribe-rc-file*)))
	  (if (and (string? file) (file-exists? file))
	      (loadq file)))))
      
