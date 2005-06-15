;*=====================================================================*/
;*    serrano/prgm/project/skribe/src/bigloo/source.scm                */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Aug 29 07:27:25 2003                          */
;*    Last change :  Tue Nov  2 14:25:50 2004 (serrano)                */
;*    Copyright   :  2003-04 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    The Bigloo handling of Skribe programs.                          */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module skribe_source
    
   (include "new.sch")
   
   (import  skribe_types
	    skribe_lib
	    skribe_resolve
	    skribe_eval
	    skribe_api
	    skribe_param)

   (export  (source-read-chars::bstring ::bstring ::int ::int ::obj)
	    (source-read-lines::bstring ::bstring ::obj ::obj ::obj)
	    (source-read-definition::bstring ::bstring ::obj ::obj ::obj)
	    (source-fontify ::obj ::obj)
	    (split-string-newline::pair-nil ::bstring)))

;*---------------------------------------------------------------------*/
;*    source-read-lines ...                                            */
;*---------------------------------------------------------------------*/
(define (source-read-chars file start stop tab)
   (define (readl p)
      (read/rp (regular-grammar ()
		  ((: (* (out #\Newline)) (? #\Newline))
		   (the-string))
		  (else
		   (the-failure)))
	       p))
   (let ((p (find-file/path file (skribe-source-path))))
      (if (or (not (string? p)) (not (file-exists? p)))
	  (skribe-error 'source
			(format "Can't find `~a' source file in path" file)
			(skribe-source-path))
	  (with-input-from-file p
	     (lambda ()
		(if (>fx *skribe-verbose* 0)
		    (fprint (current-error-port) "  [source file: " p "]"))
		(let loop ((c -1)
			   (s (readl (current-input-port)))
			   (r '()))
		   (let ((p (input-port-position (current-input-port))))
		      (cond
			 ((eof-object? s)
			  (apply string-append (reverse! r)))
			 ((>=fx p stop)
			  (let* ((len (-fx (-fx stop start) c))
				 (line (untabify (substring s 0 len) tab)))
			     (apply string-append
				    (reverse! (cons line r)))))
			 ((>=fx c 0)
			  (loop (+fx (string-length s) c)
				(readl (current-input-port))
				(cons (untabify s tab) r)))
			 ((>=fx p start)
			  (let* ((len (string-length s))
				 (nc (-fx p start)))
			     (if (>fx p stop)
				 (untabify
				  (substring s
					     (-fx len (-fx p start))
					     (-fx (-fx p stop) 1))
				  tab)
				 (loop nc
				       (readl (current-input-port))
				       (list 
					(untabify
					 (substring s
						    (-fx len (-fx p start))
						    len)
					 tab))))))
			 (else
			  (loop c (readl (current-input-port)) r))))))))))

;*---------------------------------------------------------------------*/
;*    source-read-lines ...                                            */
;*---------------------------------------------------------------------*/
(define (source-read-lines file start stop tab)
   (let ((p (find-file/path file (skribe-source-path))))
      (if (or (not (string? p)) (not (file-exists? p)))
	  (skribe-error 'source
			(format "Can't find `~a' source file in path" file)
			(skribe-source-path))
	  (with-input-from-file p
	     (lambda ()
		(if (>fx *skribe-verbose* 0)
		    (fprint (current-error-port) "  [source file: " p "]"))
		(let ((startl (if (string? start) (string-length start) -1))
		      (stopl (if (string? stop) (string-length stop) -1)))
		   (let loop ((l 1)
			      (armedp (not (or (integer? start)
					       (string? start))))
			      (s (read-line))
			      (r '()))
		      (cond
			 ((or (eof-object? s)
			      (and (integer? stop) (> l stop))
			      (and (string? stop) (substring=? stop s stopl)))
			  (apply string-append (reverse! r)))
			 (armedp
			  (loop (+fx l 1)
				#t
				(read-line)
				(cons* "\n" (untabify s tab) r)))
			 ((and (integer? start) (>= l start))
			  (loop (+fx l 1)
				#t
				(read-line)
				(cons* "\n" (untabify s tab) r)))
			 ((and (string? start) (substring=? start s startl))
			  (loop (+fx l 1) #t (read-line) r))
			 (else
			  (loop (+fx l 1) #f (read-line) r))))))))))

;*---------------------------------------------------------------------*/
;*    untabify ...                                                     */
;*---------------------------------------------------------------------*/
(define (untabify obj tab)
   (if (not tab)
       obj
       (let ((len (string-length obj))
	     (tabl tab))
	  (let loop ((i 0)
		     (col 1))
	     (cond
		((=fx i len)
		 (let ((nlen (-fx col 1)))
		    (if (=fx len nlen)
			obj
			(let ((new (make-string col #\space)))
			   (let liip ((i 0)
				      (j 0)
				      (col 1))
			      (cond
				 ((=fx i len)
				  new)
				 ((char=? (string-ref obj i) #\tab)
				  (let ((next-tab (*fx (/fx (+fx col tabl)
							    tabl)
						       tabl)))
				     (liip (+fx i 1)
					   next-tab
					   next-tab)))
				 (else
				  (string-set! new j (string-ref obj i))
				  (liip (+fx i 1) (+fx j 1) (+fx col 1)))))))))
		((char=? (string-ref obj i) #\tab)
		 (loop (+fx i 1)
		       (*fx (/fx (+fx col tabl) tabl) tabl)))
		(else
		 (loop (+fx i 1) (+fx col 1))))))))

;*---------------------------------------------------------------------*/
;*    source-read-definition ...                                       */
;*---------------------------------------------------------------------*/
(define (source-read-definition file definition tab lang)
   (let ((p (find-file/path file (skribe-source-path))))
      (cond
	 ((not (%language-extractor lang))
	  (skribe-error 'source
			"The specified language has not defined extractor"
			lang))
	 ((or (not p) (not (file-exists? p)))
	  (skribe-error 'source
			(format "Can't find `~a' program file in path" file)
			(skribe-source-path)))
	 (else
	  (let ((ip (open-input-file p)))
	     (if (>fx *skribe-verbose* 0)
		 (fprint (current-error-port) "  [source file: " p "]"))
	     (if (not (input-port? ip))
		 (skribe-error 'source "Can't open file for input" p)
		 (unwind-protect
		    (let ((s ((%language-extractor lang) ip definition tab)))
		       (if (not (string? s))
			   (skribe-error 'source
					 "Can't find definition"
					 definition)
			   s))
		    (close-input-port ip))))))))

;*---------------------------------------------------------------------*/
;*    source-fontify ...                                               */
;*---------------------------------------------------------------------*/
(define (source-fontify o language)
   (define (fontify f o)
      (cond
	 ((string? o) (f o))
	 ((pair? o) (map (lambda (s) (if (string? s) (f s) (fontify f s))) o))
	 (else o)))
   (let ((f (%language-fontifier language)))
      (if (procedure? f)
	  (fontify f o)
	  o)))

;*---------------------------------------------------------------------*/
;*    split-string-newline ...                                         */
;*---------------------------------------------------------------------*/
(define (split-string-newline str)
   (let ((l (string-length str)))
      (let loop ((i 0)
		 (j 0)
		 (r '()))
	 (cond
	    ((=fx i l)
	     (if (=fx i j)
		 (reverse! r)
		 (reverse! (cons (substring str j i) r))))
	    ((char=? (string-ref str i) #\Newline)
	     (loop (+fx i 1)
		   (+fx i 1)
		   (if (=fx i j)
		       (cons 'eol r)
		       (cons* 'eol (substring str j i) r))))
	    ((and (char=? (string-ref str i) #a013)
		  (<fx (+fx i 1) l)
		  (char=? (string-ref str (+fx i 1)) #\Newline))
	     (loop (+fx i 2)
		   (+fx i 2)
		   (if (=fx i j)
		       (cons 'eol r)
		       (cons* 'eol (substring str j i) r))))
	    (else
	     (loop (+fx i 1) j r))))))
	    
