;*=====================================================================*/
;*    serrano/prgm/project/skribe/src/bigloo/xml.scm                   */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Sep  1 12:08:39 2003                          */
;*    Last change :  Mon May 17 10:14:24 2004 (serrano)                */
;*    Copyright   :  2003-04 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    XML fontification                                                */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module skribe_xml

   (include "new.sch")
   
   (import  skribe_types
	    skribe_lib
	    skribe_resolve
	    skribe_eval
	    skribe_api
	    skribe_param
	    skribe_source)

   (export  xml))

;*---------------------------------------------------------------------*/
;*    xml ...                                                          */
;*---------------------------------------------------------------------*/
(define xml 
   (new language
      (name "xml")
      (fontifier xml-fontifier)
      (extractor #f)))

;*---------------------------------------------------------------------*/
;*    xml-fontifier ...                                                */
;*---------------------------------------------------------------------*/
(define (xml-fontifier s)
   (let ((g (regular-grammar ()
	       ((: #\; (in "<!--") (* (or all #\Newline)) "-->")
		;; italic comments
		(let ((str (split-string-newline (the-string))))
		   (append (map (lambda (s)
				   (if (eq? s 'eol)
				       "\n"
				       (new markup
					  (markup '&source-line-comment)
					  (body s))))
				str)
			   (ignore))))
	       ((+ (or #\Newline #\Space))
		;; separators
		(let ((str (the-string)))
		   (cons str (ignore))))
	       ((or (: #\< (+ (out #\> #\space #\tab #\Newline))) #\>)
		;; markup
		(let ((str (the-string)))
		   (let ((c (new markup
			       (markup '&source-module)
			       (body (the-string)))))
		      (cons c (ignore)))))
	       ((+ (out #\< #\> #\Space #\Tab #\= #\"))
		;; regular text
		(let ((string (the-string)))
		   (cons string (ignore))))
	       ((or (: "\"" (* (or (out #a000 #\\ #\") (: #\\ all))) "\"")
		    (: "\'" (* (or (out #a000 #\\ #\') (: #\\ all))) "\'"))
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
	       ((in "\"=")
		(let ((str (the-string)))
		   (cons str (ignore))))
	       (else
		(let ((c (the-failure)))
		   (if (eof-object? c)
		       '()
		       (error "source(xml)" "Unexpected character" c)))))))
      (with-input-from-string s
	 (lambda ()
	    (read/rp g (current-input-port))))))

