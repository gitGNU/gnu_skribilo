;*=====================================================================*/
;*    serrano/prgm/project/skribe/src/bigloo/c.scm                     */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Sep  1 12:08:39 2003                          */
;*    Last change :  Thu May 27 10:11:24 2004 (serrano)                */
;*    Copyright   :  2003-04 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    C fontification                                                  */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module skribe_c

   (include "new.sch")
   
   (import  skribe_types
	    skribe_lib
	    skribe_resolve
	    skribe_eval
	    skribe_api
	    skribe_param
	    skribe_source)

   (export  C))

;*---------------------------------------------------------------------*/
;*    C stamps                                                         */
;*---------------------------------------------------------------------*/
(define *keyword* (gensym))
(define *cpp* (gensym))

;*---------------------------------------------------------------------*/
;*    C keywords                                                       */
;*---------------------------------------------------------------------*/
(for-each (lambda (symbol)
	     (putprop! symbol *keyword* #t))
	  '(for class template while return try catch break continue
		do if else typedef struct union goto switch case
		static extern default finally throw))
(let ((sharp (string->symbol "#")))
   (for-each (lambda (symbol)
		(putprop! (symbol-append sharp symbol) *cpp* #t))
	     '(include define if ifdef ifdef else endif)))

;*---------------------------------------------------------------------*/
;*    C ...                                                            */
;*---------------------------------------------------------------------*/
(define C 
   (new language
      (name "C")
      (fontifier c-fontifier)
      (extractor #f)))

;*---------------------------------------------------------------------*/
;*    c-fontifier ...                                                  */
;*---------------------------------------------------------------------*/
(define (c-fontifier s)
   (let ((g (regular-grammar ()
	       ((: "/*" (* (or (out #\*) (: (+ #\*) (out #\/ #\*))))
			(+ #\*) "/")
		;; bold comments
		(let ((str (split-string-newline (the-string))))
		   (append (map (lambda (s)
				   (if (eq? s 'eol)
				       "\n"
				       (new markup
					  (markup '&source-line-comment)
					  (body s))))
				str)
			   (ignore))))
	       ((: "//" (* all))
		;; italic comments
		(let ((c (new markup
			    (markup '&source-comment)
			    (body (the-string)))))
		   (cons c (ignore))))
	       ((+ (or #\Newline #\Space))
		;; separators
		(let ((str (the-string)))
		   (cons str (ignore))))
	       ((in "{}")
		;; brackets
		(let ((str (the-string)))
		   (let ((c (new markup
			       (markup '&source-bracket)
			       (body (the-string)))))
		      (cons c (ignore)))))
	       ((+ (out #\; #\Space #\Tab #\Newline #\( #\) #\{ #\} #\[ #\] #\" #\< #\> #\= #\! #\/ #\/ #\+ #\* #\-))
		;; keywords
		(let* ((string (the-string))
		       (symbol (the-symbol)))
		   (cond
		      ((getprop symbol *keyword*)
		       (let ((c (new markup
				   (markup '&source-keyword)
				   (ident (symbol->string (gensym)))
				   (body string))))
			  (cons c (ignore))))
		      ((getprop symbol *cpp*)
		       (let ((c (new markup
				   (markup '&source-module)
				   (ident (symbol->string (gensym)))
				   (body string))))
			  (cons c (ignore))))
		      (else
		       (cons string (ignore))))))
	       ((in "<>=!/\\+*-([])")
		;; regular text
		(let ((s (the-string)))
		   (cons s (ignore))))
	       ((: "\"" (* (or (out #a000 #\\ #\") (: #\\ all))) "\"")
		;; strings
		(let ((str (split-string-newline (the-string))))
		   (append (map (lambda (s)
				   (if (eq? s 'eol)
				       "\n"
				       (new markup
					  (markup '&source-string)
					  (body s))))
				str)
			   (ignore))))
	       ((+ (or #\; #\" #\# #\tab))
		(let ((str (the-string)))
		   (cons str (ignore))))
	       (else
		(let ((c (the-failure)))
		   (if (eof-object? c)
		       '()
		       (error "source(C)" "Unexpected character" c)))))))
      (read/rp g (open-input-string s))))

