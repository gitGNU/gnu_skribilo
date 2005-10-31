;*=====================================================================*/
;*    serrano/prgm/project/skribe/skr/letter.skr                       */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Oct  3 12:22:13 2003                          */
;*    Last change :  Tue Oct 28 14:33:43 2003 (serrano)                */
;*    Copyright   :  2003 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    French Skribe style                                              */
;*=====================================================================*/

(define-skribe-module (skribilo packages french))

;*---------------------------------------------------------------------*/
;*    LaTeX configuration                                              */
;*---------------------------------------------------------------------*/
(let ((le (find-engine 'latex)))
   (engine-custom-set! le 'usepackage
		       (string-append (engine-custom le 'usepackage)
				      "\\usepackage[french]{babel}
\\usepackage{a4}")))
