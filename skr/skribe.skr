;*=====================================================================*/
;*    serrano/prgm/project/skribe/skr/skribe.skr                       */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Jan 11 11:23:12 2002                          */
;*    Last change :  Sun Jul 11 12:22:38 2004 (serrano)                */
;*    Copyright   :  2002-04 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    The standard Skribe style (always loaded).                       */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    p ...                                                            */
;*---------------------------------------------------------------------*/
(define-markup (p #!rest opt #!key ident (class #f) &skribe-eval-location)
   (paragraph :ident ident :class class :loc &skribe-eval-location
      (the-body opt)))

;*---------------------------------------------------------------------*/
;*    fg ...                                                           */
;*---------------------------------------------------------------------*/
(define (fg c . body)
   (color :fg c body))

;*---------------------------------------------------------------------*/
;*    bg ...                                                           */
;*---------------------------------------------------------------------*/
(define (bg c . body)
   (color :bg c body))

;*---------------------------------------------------------------------*/
;*    counter ...                                                      */
;*    -------------------------------------------------------------    */
;*    This produces a kind of "local enumeration" that is:             */
;*       (counting "toto," "tutu," "titi.")                            */
;*    produces:                                                        */
;*       i) toto, ii) tutu, iii) titi.                                 */
;*---------------------------------------------------------------------*/
(define-markup (counter #!rest opts #!key (numbering 'roman))
   (define items (if (eq? (car opts) :numbering) (cddr opts) opts))
   (define vroman '#(- "i" "ii" "iii" "iv" "v" "vi" "vii" "viii" "ix" "x"))
   (define (the-roman-number num)
      (if (< num (vector-length vroman))
	  (list (list "(" (it (vector-ref vroman num)) ") "))
	  (skribe-error 'counter
			"too many items for roman numbering"
			(length items))))
   (define (the-arabic-number num)
      (list (list "(" (it (integer->string num)) ") ")))
   (define (the-alpha-number num)
      (list (list "(" (it (+ (integer->char #\a) num -1)) ") ")))
   (let ((the-number (case numbering
			((roman) the-roman-number)
			((arabic) the-arabic-number)
			((alpha) the-alpha-number)
			(else (skribe-error 'counter
					    "Illegal numbering"
					    numbering)))))
      (let loop ((num 1)
		 (items items)
		 (res '()))
	   (if (null? items)
	       (reverse! res)
	       (loop (+ num 1)
		     (cdr items)
		     (cons (list (the-number num) (car items)) res))))))

;*---------------------------------------------------------------------*/
;*    q                                                                */
;*---------------------------------------------------------------------*/
(define-markup (q #!rest opt)
   (new markup
      (markup 'q)
      (options (the-options opt))
      (body (the-body opt))))

