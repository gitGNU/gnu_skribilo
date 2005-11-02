;*=====================================================================*/
;*    serrano/prgm/project/skribe/src/bigloo/asm.scm                   */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Sep  1 12:08:39 2003                          */
;*    Last change :  Tue Jan 20 06:07:44 2004 (serrano)                */
;*    Copyright   :  2003-04 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    ASM fontification                                                */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module skribe_asm

   (include "new.sch")
   
   (import  skribe_types
	    skribe_lib
	    skribe_resolve
	    skribe_eval
	    skribe_api
	    skribe_param
	    skribe_source)

   (export  asm))

;*---------------------------------------------------------------------*/
;*    asm ...                                                          */
;*---------------------------------------------------------------------*/
(define asm
   (new language
      (name "asm")
      (fontifier asm-fontifier)
      (extractor #f)))

;*---------------------------------------------------------------------*/
;*    asm-fontifier ...                                                */
;*---------------------------------------------------------------------*/
(define (asm-fontifier s)
   (let ((g (regular-grammar ()
	       ((: "/*" (* (or (out #\*) (: (+ #\*) (out #\/ #\*))))
			(+ #\*) "/")
		;; bold comments
		(let ((c (new markup
			    (markup '&source-line-comment)
			    (body (the-string)))))
		   (cons c (ignore))))
	       ((: "//" (* all))
		;; italic comments
		(let ((c (new markup
			    (markup '&source-comment)
			    (body (the-string)))))
		   (cons c (ignore))))
	       ((: "#" (* all))
		;; italic comments
		(let ((c (new markup
			    (markup '&source-comment)
			    (body (the-string)))))
		   (cons c (ignore))))
	       ((+ (or #\Newline #\Space))
		;; separators
		(let ((str (the-string)))
		   (cons str (ignore))))
	       ((: (* (in #\tab #\space))
		   (+ (out #\: #\Space #\Tab #\Newline)) #\:)
		;; labels
		(let ((c (new markup
			    (markup '&source-define)
			    (body (the-string)))))
		   (cons c (ignore))))
	       ((or (in "<>=!/\\+*-([])")
		    #\/
		    (+ (out #\; #\Space #\Tab #\Newline #\( #\) #\[ #\] #\" #\< #\> #\= #\! #\/ #\/ #\+ #\* #\-)))
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
		       (error "source(asm)" "Unexpected character" c)))))))
      (read/rp g (open-input-string s))))

