;*=====================================================================*/
;*    serrano/prgm/project/skribe/tools/skribebibtex/main.scm          */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Oct 12 14:57:58 2001                          */
;*    Last change :  Fri Oct 24 12:00:23 2003 (serrano)                */
;*    Copyright   :  2001-03 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    The entry point of the bibtex->skribe translator                 */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module main
   (import skribebibtex)
   (main main))

;*---------------------------------------------------------------------*/
;*    main ...                                                         */
;*---------------------------------------------------------------------*/
(define (main argv)
   (define (usage args-parse-usage)
      (print "usage: skribebibtex [options] [input]")
      (newline)
      (args-parse-usage #f))
   (let ((stage 'scr)
	 (dest #f)
	 (in #f))
      (args-parse (cdr argv)
	 ((("-h" "--help") (help "This help message"))
	  (usage args-parse-usage)
	  (exit 0))
	 ((("--options") (help "Display the options and exit"))
	  (args-parse-usage #t)
	  (exit 0))
	 (("-o" ?out (help "Set the destination file"))
	  (set! dest out))
	 (else
	  (set! in else)))
      (if (string? dest)
	  (with-output-to-file dest (lambda () (skribebibtex in)))
	  (skribebibtex in))))

