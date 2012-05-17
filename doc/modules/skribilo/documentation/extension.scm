;;; extension.scm  --  The Skribilo package for documenting extensions.
;;; -*- coding: iso-8859-1 -*-
;;;
;;; Copyright 2007, 2009  Ludovic Courtès <ludo@gnu.org>
;;; Copyright 2003, 2004  Manuel Serrano
;;;
;;;
;;; This file is part of Skribilo.
;;;
;;; Skribilo is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; Skribilo is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with Skribilo.  If not, see <http://www.gnu.org/licenses/>.

(define-module (skribilo documentation extension)
  :use-module (skribilo sui)
  :use-module (skribilo config)
  :use-module (skribilo parameters)

  :use-module (skribilo lib)
  :use-module (skribilo ast)
  :use-module (skribilo engine)
  :use-module (skribilo writer)
  :use-module (skribilo output)
  :use-module (skribilo evaluator)

  :use-module (skribilo package base)
  :use-module (skribilo engine html)

  :use-module (skribilo utils files)
  :use-module (skribilo utils compat)
  :use-module (skribilo utils syntax)
  :use-module (skribilo utils keywords)

  :use-module (ice-9 optargs))

(skribilo-module-syntax)


;*---------------------------------------------------------------------*/
;*    extension                                                        */
;*---------------------------------------------------------------------*/
(define-markup (extension :rest opt 
			  :key (ident (symbol->string (gensym 'extension)))
			  (class "extension")
			  title html-title ending author description 
			  (env '()))
   (new document
      (markup 'extension)
      (ident ident)
      (class class)
      (loc &invocation-location)
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
		   (tr (td :align 'left :valign 'top (bold "Skribilo: "))
		      (td :align 'right :valign 'top
			 (ref :url (skribilo-doc-directory)
			    :text "Directory")))
		   (tr (td)
		      (td :align 'right :valign 'top
			 (ref :url (skribilo-doc-directory)
			    :text "User Manual"))))
		(table :width 100. :border 0 :cellspacing 0 :cellpadding 0
		   (tr (td :align 'left :valign 'top (bold "index:"))
		      (td :align 'right (ref :handle (handle i))))
		   (tr (td :align 'left :valign 'top (bold "Skribe: "))
		      (td :align 'right :valign 'top
			 (ref :url (skribilo-doc-directory)
			    :text "Directory")))
		   (tr (td)
		      (td :align 'right :valign 'top
			 (ref :url (skribilo-doc-directory)
			    :text "User Manual"))))))))
   (default-engine-set! he))

;*---------------------------------------------------------------------*/
;*    extension-sui ...                                                */
;*---------------------------------------------------------------------*/
(define (extension-sui n e)
   (define (sui)
      (display "(sui \"")
      (evaluate-document (markup-option n :title) html-title-engine)
      (display "\"\n")
      (printf "  :file ~s\n" (sui-referenced-file n e))
      (printf "  :description ~s\n" (markup-option n :description))
      (sui-marks n e)
      (display "  )\n"))
   (if (string? (*destination-file*))
       (let ((f (string-append (file-prefix (*destination-file*)) ".sui")))
	  (with-output-to-file f sui))
       (sui)))

;*---------------------------------------------------------------------*/
;*    project ...                                                      */
;*---------------------------------------------------------------------*/
(markup-writer 'extension
   :options '(:title :html-title :ending :author :description)
   :action (let ((he (find-engine 'html)))
             (lambda (n e)
               (output n e (markup-writer-get 'document he))))
   :after (lambda (n e)
	     (if (engine-custom e 'emit-sui)
		 (extension-sui n e))))

;*---------------------------------------------------------------------*/
;*    Restore the base engine                                          */
;*---------------------------------------------------------------------*/
(default-engine-set! (find-engine 'base))
