;;; extension.scm  --  The Skribe package for documenting extensions
;;;
;;; Copyright 2003, 2004  Manuel Serrano
;;;
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301,
;;; USA.

(define-module (skribilo documentation extension)
  :use-module (skribilo reader)
  :use-module (skribilo utils compat))

(fluid-set! current-reader (make-reader 'skribe))


;*---------------------------------------------------------------------*/
;*    extension                                                        */
;*---------------------------------------------------------------------*/
(define-markup (extension #!rest opt 
			  #!key (ident (symbol->string (gensym 'extension)))
			  (class "extension")
			  title html-title ending author description 
			  (env '()))
   (new document
      (markup 'extension)
      (ident ident)
      (class class)
      (options (the-options opt))
      (body (the-body opt))
      (env (append env
		   (list (list 'example-counter 0) (list 'example-env '())
			 (list 'chapter-counter 0) (list 'chapter-env '())
			 (list 'section-counter 0) (list 'section-env '())
			 (list 'footnote-counter 0) (list 'footnote-env '())
			 (list 'figure-counter 0) (list 'figure-env '()))))))
		  
;*---------------------------------------------------------------------*/
;*    html engine                                                      */
;*---------------------------------------------------------------------*/
(let ((he (find-engine 'html)))
   (engine-custom-set! he 'web-book-main-browsing-extra 
      (lambda (n e)
	 (let ((i (let ((m (find-markup-ident "Index")))
		     (and (pair? m) (car m)))))
	    (if (not i)
		(table :width 100. :border 0 :cellspacing 0 :cellpadding 0
		   (tr (td :align 'left :valign 'top (bold "Skribe: "))
		      (td :align 'right :valign 'top
			 (ref :url *skribe-dir-doc-url* 
			    :text "Directory")))
		   (tr (td)
		      (td :align 'right :valign 'top
			 (ref :url *skribe-user-doc-url* 
			    :text "User Manual"))))
		(table :width 100. :border 0 :cellspacing 0 :cellpadding 0
		   (tr (td :align 'left :valign 'top (bold "index:"))
		      (td :align 'right (ref :handle (handle i))))
		   (tr (td :align 'left :valign 'top (bold "Skribe: "))
		      (td :align 'right :valign 'top
			 (ref :url *skribe-dir-doc-url* 
			    :text "Directory")))
		   (tr (td)
		      (td :align 'right :valign 'top
			 (ref :url *skribe-user-doc-url* 
			    :text "User Manual"))))))))
   (default-engine-set! he))

;*---------------------------------------------------------------------*/
;*    extension-sui ...                                                */
;*---------------------------------------------------------------------*/
(define (extension-sui n e)
   (define (sui)
      (display "(sui \"")
      (skribe-eval (markup-option n :title) html-title-engine)
      (display "\"\n")
      (printf "  :file ~s\n" (sui-referenced-file n e))
      (printf "  :description ~s\n" (markup-option n :description))
      (sui-marks n e)
      (display "  )\n"))
   (if (string? *skribe-dest*)
       (let ((f (format "~a.sui" (prefix *skribe-dest*))))
	  (with-output-to-file f sui))
       (sui)))

;*---------------------------------------------------------------------*/
;*    project ...                                                      */
;*---------------------------------------------------------------------*/
(markup-writer 'extension
   :options '(:title :html-title :ending :author :description)
   :action (lambda (n e)
	      (output n e (markup-writer-get 'document he)))
   :after (lambda (n e)
	     (if (engine-custom e 'emit-sui)
		 (extension-sui n e))))

;*---------------------------------------------------------------------*/
;*    Restore the base engine                                          */
;*---------------------------------------------------------------------*/
(default-engine-set! (find-engine 'base))
