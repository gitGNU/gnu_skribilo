;*=====================================================================*/
;*    serrano/prgm/project/skribe/skr/xml.skr                          */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Sep  2 09:46:09 2003                          */
;*    Last change :  Sat Mar  6 11:22:05 2004 (serrano)                */
;*    Copyright   :  2003-04 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Generic XML Skribe engine                                        */
;*    -------------------------------------------------------------    */
;*    Implementation:                                                  */
;*       common: @path ../src/common/api.src@                          */
;*       bigloo: @path ../src/bigloo/api.bgl@                          */
;*    -------------------------------------------------------------    */
;*    doc:                                                             */
;*       @ref ../../doc/user/xmle.skb:ref@                             */
;*=====================================================================*/

(define-skribe-module (skribilo engine xml))

;*---------------------------------------------------------------------*/
;*    xml-engine ...                                                   */
;*---------------------------------------------------------------------*/
(define xml-engine
   ;; setup the xml engine
   (default-engine-set!
      (make-engine 'xml
		   :version 1.0
		   :format "html"
		   :delegate (find-engine 'base)
		   :filter (make-string-replace '((#\< "&lt;")
						  (#\> "&gt;")
						  (#\& "&amp;")
						  (#\" "&quot;")
						  (#\@ "&#x40;"))))))

;*---------------------------------------------------------------------*/
;*    markup ...                                                       */
;*---------------------------------------------------------------------*/
(let ((xml-margin 0))
   (define (make-margin)
      (make-string xml-margin #\space))
   (define (xml-attribute? val)
      (cond
	 ((or (string? val) (number? val) (boolean? val))
	  #t)
	 ((list? val)
	  (every? xml-attribute? val))
	 (else
	  #f)))
   (define (xml-attribute att val)
      (let ((s (keyword->string att)))
	 (printf " ~a=\"" (substring s 1 (string-length s)))
	 (let loop ((val val))
	    (cond
	       ((or (string? val) (number? val))
		(display val))
	       ((boolean? val)
		(display (if val "true" "false")))
	       ((pair? val)
		(for-each loop val))
	       (else
		#f)))
	 (display #\")))
   (define (xml-option opt val e)
      (let* ((m (make-margin))
	     (ks (keyword->string opt))
	     (s (substring ks 1 (string-length ks))))
	 (printf "~a<~a>\n" m s)
	 (output val e)
	 (printf "~a</~a>\n" m s)))
   (define (xml-options n e)
      ;; display the true options
      (let ((opts (filter (lambda (o)
			     (and (keyword? (car o))
				  (not (xml-attribute? (cadr o)))))
			  (markup-options n))))
	 (if (pair? opts)
	     (let ((m (make-margin)))
		(display m)
		(display "<options>\n")
		(set! xml-margin (+ xml-margin 1))
		(for-each (lambda (o)
			     (xml-option (car o) (cadr o) e))
			  opts)
		(set! xml-margin (- xml-margin 1))
		(display m)
		(display "</options>\n")))))
   (markup-writer #t
      :options 'all
      :before (lambda (n e)
		 (printf "~a<~a" (make-margin) (markup-markup n))
		 ;; display the xml attributes
		 (for-each (lambda (o)
			      (if (and (keyword? (car o))
				       (xml-attribute? (cadr o)))
				  (xml-attribute (car o) (cadr o))))
			   (markup-options n))
		 (set! xml-margin (+ xml-margin 1))
		 (display ">\n"))
      :action (lambda (n e)
		 ;; options
		 (xml-options n e)
		 ;; body
		 (output (markup-body n) e))
      :after (lambda (n e)
		(printf "~a</~a>\n" (make-margin) (markup-markup n))
		(set! xml-margin (- xml-margin 1)))))

;*---------------------------------------------------------------------*/
;*    Restore the base engine                                          */
;*---------------------------------------------------------------------*/
(default-engine-set! (find-engine 'base))
