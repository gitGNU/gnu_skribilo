;*=====================================================================*/
;*    serrano/prgm/project/skribe/src/bigloo/lisp.scm                  */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Aug 29 08:14:59 2003                          */
;*    Last change :  Mon Nov  8 14:32:22 2004 (serrano)                */
;*    Copyright   :  2003-04 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Handling of lispish source files.                                */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module skribe_lisp
   
   (include "new.sch")
   
   (import  skribe_types
	    skribe_lib
	    skribe_resolve
	    skribe_eval
	    skribe_api
	    skribe_param
	    skribe_source)

   (export  bigloo
	    scheme
	    lisp
	    skribe))

;*---------------------------------------------------------------------*/
;*    keys ...                                                         */
;*---------------------------------------------------------------------*/
(define *the-key* #f)
(define *bracket-highlight* #t)
(define *bigloo-key* #f)
(define *scheme-key* #f)
(define *lisp-key* #f)
(define *skribe-key* #f)

;*---------------------------------------------------------------------*/
;*    init-bigloo-fontifier! ...                                       */
;*---------------------------------------------------------------------*/
(define (init-bigloo-fontifier!)
   (if (not *bigloo-key*)
       (begin
	  (set! *bigloo-key* (gensym))
	  ;; language keywords
	  (for-each (lambda (symbol)
		       (putprop! symbol *bigloo-key* 'symbol))
		    '(set! if let cond case quote begin letrec let*
			   lambda export extern class generic inline
			   static import foreign type with-access instantiate
			   duplicate labels
			   match-case match-lambda
			   syntax-rules pragma widen! shrink!
			   wide-class profile profile/gc 
			   regular-grammar lalr-grammar apply))
	  ;; define
	  (for-each (lambda (symbol)
		       (putprop! symbol *bigloo-key* 'define))
		    '(define define-inline define-struct define-macro
			define-generic define-method define-syntax
			define-expander))  
	  ;; error
	  (for-each (lambda (symbol)
		       (putprop! symbol *bigloo-key* 'error))
		    '(bind-exit unwind-protect call/cc error warning))
	  ;; module
	  (for-each (lambda (symbol)
		       (putprop! symbol *bigloo-key* 'module))
		    '(module import export library))
	  ;; thread
	  (for-each (lambda (symbol)
		       (putprop! symbol *bigloo-key* 'thread))
		    '(make-thread thread-start! thread-yield!
				  thread-await! thread-await*!
				  thread-sleep! thread-join!
				  thread-terminate! thread-suspend!
				  thread-resume! thread-yield!
				  thread-specific thread-specific-set!
				  thread-name thread-name-set!
				  scheduler-react! scheduler-start!
				  broadcast! scheduler-broadcast!
				  current-thread thread?
				  current-scheduler scheduler? make-scheduler
				  make-input-signal make-output-signal
				  make-connect-signal make-process-signal
				  make-accept-signal make-timer-signal
				  thread-get-values! thread-get-values*!)))))

;*---------------------------------------------------------------------*/
;*    init-lisp-fontifier! ...                                         */
;*---------------------------------------------------------------------*/
(define (init-lisp-fontifier!)
   (if (not *lisp-key*)
       (begin
	  (set! *lisp-key* (gensym))
	  ;; language keywords
	  (for-each (lambda (symbol)
		       (putprop! symbol *lisp-key* 'symbol))
		    '(setq if let cond case else progn letrec let*
			   lambda labels try unwind-protect apply funcall))
	  ;; defun
	  (for-each (lambda (symbol)
		       (putprop! symbol *lisp-key* 'define))
		    '(define defun defvar defmacro)))))

;*---------------------------------------------------------------------*/
;*    init-skribe-fontifier! ...                                       */
;*---------------------------------------------------------------------*/
(define (init-skribe-fontifier!)
   (if (not *skribe-key*)
       (begin
	  (set! *skribe-key* (gensym))
	  ;; language keywords
	  (for-each (lambda (symbol)
		       (putprop! symbol *skribe-key* 'symbol))
		    '(set! bold it emph tt color ref index underline
			   figure center pre flush hrule linebreak
			   image kbd code var samp sc sf sup sub
			   itemize description enumerate item
			   table tr td th item prgm author
			   prgm hook font lambda))
	  ;; define
	  (for-each (lambda (symbol)
		       (putprop! symbol *skribe-key* 'define))
		    '(define define-markup))
	  ;; markup
	  (for-each (lambda (symbol)
		       (putprop! symbol *skribe-key* 'markup))
		    '(document chapter section subsection subsubsection
			       paragraph p handle resolve processor
			       abstract margin toc table-of-contents
			       current-document current-chapter current-section
			       document-sections* section-number
			       footnote print-index include skribe-load
			       slide)))))

;*---------------------------------------------------------------------*/
;*    bigloo ...                                                       */
;*---------------------------------------------------------------------*/
(define bigloo
   (new language
      (name "bigloo")
      (fontifier bigloo-fontifier)
      (extractor bigloo-extractor)))

;*---------------------------------------------------------------------*/
;*    scheme ...                                                       */
;*---------------------------------------------------------------------*/
(define scheme
   (new language
      (name "scheme")
      (fontifier scheme-fontifier)
      (extractor scheme-extractor)))

;*---------------------------------------------------------------------*/
;*    lisp ...                                                         */
;*---------------------------------------------------------------------*/
(define lisp
   (new language
      (name "lisp")
      (fontifier lisp-fontifier)
      (extractor lisp-extractor)))

;*---------------------------------------------------------------------*/
;*    bigloo-fontifier ...                                             */
;*---------------------------------------------------------------------*/
(define (bigloo-fontifier s)
   (init-bigloo-fontifier!)
   (set! *the-key* *bigloo-key*)
   (set! *bracket-highlight* #f)
   (fontify-lisp (open-input-string s)))

;*---------------------------------------------------------------------*/
;*    bigloo-extractor ...                                             */
;*---------------------------------------------------------------------*/
(define (bigloo-extractor iport def tab)
   (definition-search iport
      tab
      (lambda (exp)
	 (match-case exp
	    (((or define define-inline define-generic
		  define-method define-macro define-expander)
	      (?fun . ?-) . ?-)
	     (eq? def fun))
	    (((or define define-struct define-library) (and (? symbol?) ?var) . ?-)
	     (eq? var def))
	    (else
	     #f)))))

;*---------------------------------------------------------------------*/
;*    skribe ...                                                       */
;*---------------------------------------------------------------------*/
(define skribe
   (new language
      (name "skribe")
      (fontifier skribe-fontifier)
      (extractor skribe-extractor)))

;*---------------------------------------------------------------------*/
;*    skribe-fontifier ...                                             */
;*---------------------------------------------------------------------*/
(define (skribe-fontifier s)
   (init-skribe-fontifier!)
   (set! *the-key* *skribe-key*)
   (set! *bracket-highlight* #t)
   (fontify-lisp (open-input-string s)))

;*---------------------------------------------------------------------*/
;*    skribe-extractor ...                                             */
;*---------------------------------------------------------------------*/
(define (skribe-extractor iport def tab)
   (definition-search iport
      tab
      (lambda (exp)
	 (match-case exp
	    (((or define define-macro define-markup) (?fun . ?-) . ?-)
	     (eq? def fun))
	    ((define (and (? symbol?) ?var) . ?-)
	     (eq? var def))
	    ((markup-output (quote ?mk) . ?-)
	     (eq? mk def))
	    (else
	     #f)))))

;*---------------------------------------------------------------------*/
;*    scheme-fontifier ...                                             */
;*---------------------------------------------------------------------*/
(define (scheme-fontifier s) s)

;*---------------------------------------------------------------------*/
;*    scheme-extractor ...                                             */
;*---------------------------------------------------------------------*/
(define (scheme-extractor iport def tab)
   (definition-search iport
      tab
      (lambda (exp)
	 (match-case exp
	    (((or define define-macro) (?fun . ?-) . ?-)
	     (eq? def fun))
	    ((define (and (? symbol?) ?var) . ?-)
	     (eq? var def))
	    (else
	     #f)))))

;*---------------------------------------------------------------------*/
;*    lisp-fontifier ...                                               */
;*---------------------------------------------------------------------*/
(define (lisp-fontifier s)
   (init-lisp-fontifier!)
   (set! *the-key* *lisp-key*)
   (set! *bracket-highlight* #f)
   (fontify-lisp (open-input-string s)))
 
;*---------------------------------------------------------------------*/
;*    lisp-extractor ...                                               */
;*---------------------------------------------------------------------*/
(define (lisp-extractor iport def tab)
   (definition-search iport
      tab
      (lambda (exp)
	 (match-case exp
	    (((or defun defmacro) ?fun ?- . ?-)
	     (eq? def fun))
	    ((defvar ?var . ?-)
	     (eq? var def))
	    (else
	     #f)))))

;*---------------------------------------------------------------------*/
;*    definition-search ...                                            */
;*    -------------------------------------------------------------    */
;*    This function seeks a Bigloo definition. If it finds it, it      */
;*    returns two values the starting char number of the definition    */
;*    and the stop char.                                               */
;*---------------------------------------------------------------------*/
(define (definition-search ip tab semipred)
   (cond-expand
      (bigloo2.6
       (define (reader-current-line-number)
	  (let* ((port (open-input-string "(9)"))
		 (exp  (read port #t)))
	     (close-input-port port)
	     (line-number exp)))
       (define (line-number expr)
	  (and (epair? expr)
	       (match-case (cer expr)
		  ((at ?- ?pos ?line)
		   line))))
       (reader-reset!)
       (let loop ((exp (read ip #t)))
	  (if (not (eof-object? exp))
	      (let ((v (semipred exp)))
		 (if (not v)
		     (loop (read ip #t))
		     (let* ((b (line-number exp))
			    (e (reader-current-line-number)))
			(source-read-lines (input-port-name ip) b e tab)))))))
      (else
       (define (char-number expr)
	  (and (epair? expr)
	       (match-case (cer expr)
		  ((at ?- ?pos)
		   pos))))
       (let loop ((exp (read ip #t)))
	  (if (not (eof-object? exp))
	      (let ((v (semipred exp)))
		 (if (not v)
		     (loop (read ip #t))
		     (let* ((b (char-number exp))
			    (e (input-port-position ip)))
			(source-read-chars (input-port-name ip)
					   b
					   e
					   tab)))))))))


;*---------------------------------------------------------------------*/
;*    fontify-lisp ...                                                 */
;*---------------------------------------------------------------------*/
(define (fontify-lisp port::input-port)
   (let ((g (regular-grammar ()
	       ((: ";;" (* all))
		;; italic comments
		(let ((c (new markup
			    (markup '&source-comment)
			    (body (the-string)))))
		   (cons c (ignore))))
	       ((: ";*" (* all))
		;; bold comments
		(let ((c (new markup
			    (markup '&source-line-comment)
			    (body (the-string)))))
		   (cons c (ignore))))
	       ((: ";" (out #\; #\*) (* all))
		;; plain comments
		(let ((str (the-string)))
		   (cons str (ignore))))
	       ((: #\\ (* (in #\space #\tab)) ";" (out #\; #\*) (* all))
		;; plain comments
		(let ((str (the-substring 1 (the-length))))
		   (cons str (ignore))))
	       ((+ #\Space)
		;; separators
		(let ((str (the-string)))
		   (cons (highlight str) (ignore))))
	       (#\(
		;; open parenthesis
		(let ((str (highlight (the-string))))
		   (pupush-highlight)
		   (cons str (ignore))))
	       (#\)
		;; close parenthesis
		(let ((str (highlight (the-string) -1)))
		   (cons str (ignore))))
	       ((+ (in "[]"))
		;; brackets
		(let ((s (the-string)))
		   (if *bracket-highlight*
		       (let ((c (new markup
				   (markup '&source-bracket)
				   (body s))))
			  (cons c (ignore)))
		       (cons s (ignore)))))
	       ((+ #\Tab)
		(let ((str (the-string)))
		   (cons (highlight str) (ignore))))
	       ((: #\( (+ (out "; \t()[]:\"\n")))
		;; keywords
		(let* ((string (the-substring 1 (the-length)))
		       (symbol (string->symbol string))
		       (key (getprop symbol *the-key*)))
		   (cons
		    "("
		    (case key
		       ((symbol)
			(let ((c (new markup
				    (markup '&source-keyword)
				    (ident (symbol->string (gensym)))
				    (body string))))
			   (cons c (ignore))))
		       ((define)
			(let ((c (new markup
				    (markup '&source-define)
				    (body string))))
			   (push-highlight (lambda (e)
					      (new markup
						 (markup '&source-define)
						 (ident (symbol->string (gensym)))
						 (body e)))
					   1)
			   (cons c (ignore))))
		       ((error)
			(let ((c (new markup
				    (markup '&source-error)
				    (ident (symbol->string (gensym)))
				    (body string))))
			   (cons c (ignore))))
		       ((module)
			(let ((c (new markup
				    (markup '&source-module)
				    (ident (symbol->string (gensym)))
				    (body string))))
			   (push-highlight (lambda (e)
					      (new markup
						 (markup '&source-module)
						 (ident (symbol->string (gensym)))
						 (body e)))
					   1)
			   (cons c (ignore))))
		       ((markup)
			(let ((c (new markup
				    (markup '&source-markup)
				    (ident (symbol->string (gensym)))
				    (body string))))
			   (cons c (ignore))))
		       ((thread)
			(let ((c (new markup
				    (markup '&source-thread)
				    (ident (symbol->string (gensym)))
				    (body string))))
			   (cons c (ignore))))
		       (else
			(cons (highlight string 1) (ignore)))))))
	       ((+ (out "; \t()[]:\"\n"))
		(let ((string (the-string)))
		   (cons (highlight string 1) (ignore))))
	       ((+ #\Newline)
		;; newline
		(let ((str (the-string)))
		   (cons (highlight str) (ignore))))
	       ((or (: "\"" (* (or (out #a000 #\\ #\") (: #\\ all))) "\"")
		    (: "#\"" (* (or (out #a000 #\\ #\") (: #\\ all))) "\""))
		;; strings
		(let ((str (split-string-newline (the-string))))
		   (append (map (lambda (s)
				   (if (eq? s 'eol)
				       "\n"
				       (new markup
					  (markup '&source-string)
					  (ident (symbol->string (gensym)))
					  (body s))))
				str)
			   (ignore))))
	       ((: "::" (+ (out ";\n \t()[]:\"")))
		;; type annotations
		(let ((c (new markup
			    (markup '&source-type)
			    (ident (symbol->string (gensym)))
			    (body (the-string)))))
		   (cons c (ignore))))
	       ((: ":" (out ":()[] \n\t\"") (* (out ";\n \t()[]:\"")))
		;; keywords annotations
		(let ((c (new markup
			    (markup '&source-key)
			    (ident (symbol->string (gensym)))
			    (body (the-string)))))
		   (cons c (ignore))))
	       ((+ (or #\: #\; #\"))
		(let ((str (the-string)))
		   (cons (highlight str 1) (ignore))))
	       ((: #\# #\\ (+ (out " \n\t")))
		;; characters
		(let ((str (the-string)))
		   (cons (highlight str 1) (ignore))))
	       (else
		(let ((c (the-failure)))
		   (if (eof-object? c)
		       '()
		       (error "source(lisp)" "Unexpected character" c)))))))
      (reset-highlight!)
      (read/rp g port)))

;*---------------------------------------------------------------------*/
;*    *highlight* ...                                                  */
;*---------------------------------------------------------------------*/
(define *highlight* '())

;*---------------------------------------------------------------------*/
;*    reset-highlight! ...                                             */
;*---------------------------------------------------------------------*/
(define (reset-highlight!)
   (set! *highlight* '()))

;*---------------------------------------------------------------------*/
;*    push-highlight ...                                               */
;*---------------------------------------------------------------------*/
(define (push-highlight col pv)
   (set! *highlight* (cons (cons col pv) *highlight*)))

;*---------------------------------------------------------------------*/
;*    pupush-highlight ...                                             */
;*---------------------------------------------------------------------*/
(define (pupush-highlight)
   (if (pair? *highlight*)
       (let ((c (car *highlight*)))
	  (set-cdr! c 100000))))

;*---------------------------------------------------------------------*/
;*    pop-highlight ...                                                */
;*---------------------------------------------------------------------*/
(define (pop-highlight pv)
   (case pv
      ((-1)
       (set! *highlight* (cdr *highlight*)))
      ((0)
       'nop)
      (else
       (let ((c (car *highlight*)))
	  (if (>fx (cdr c) 1)
	      (set-cdr! c (-fx (cdr c) 1))
	      (set! *highlight* (cdr *highlight*)))))))

;*---------------------------------------------------------------------*/
;*    highlight ...                                                    */
;*---------------------------------------------------------------------*/
(define (highlight exp . pop)
   (if (pair? *highlight*)
       (let* ((c (car *highlight*))
	      (r (if (>fx (cdr c) 0)
		     ((car c) exp)
		     exp)))
	  (if (pair? pop) (pop-highlight (car pop)))
	  r)
       exp))

		       
