;;; acmproc.scm  --  The Skribe style for ACMPROC articles.
;;; -*- coding: iso-8859-1 -*-
;;;
;;; Copyright 2003, 2004  Manuel Serrano
;;; Copyright 2007, 2013  Ludovic Courtès <ludo@gnu.org>
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

(define-module (skribilo package acmproc)
  :use-module (skribilo ast)
  :use-module (skribilo engine)
  :use-module (skribilo writer)
  :autoload   (skribilo output)         (output)
  :use-module (skribilo package base)
  :autoload   (skribilo utils keywords) (the-options the-body)
  :autoload   (skribilo evaluator)      (evaluate-document)

  :use-module (skribilo lib)
  :use-module (skribilo utils syntax)

  :use-module (ice-9 optargs)
  :use-module (srfi srfi-13)

  :export (abstract references acm-copyright))

(skribilo-module-syntax)



;*---------------------------------------------------------------------*/
;*    LaTeX global customizations                                      */
;*---------------------------------------------------------------------*/
(let ((le (find-engine 'latex)))
   (engine-custom-set! le
		       'documentclass
		       "\\documentclass[letterpaper]{acmproc}")
   ;; &latex-author
   (markup-writer '&latex-author le
      :before (lambda (n e)
		 (let ((body (markup-body n)))
		    (format #t "\\numberofauthors{~a}\n\\author{\n"
			    (if (pair? body) (length body) 1))))
      :action (lambda (n e)
		 (let ((body (markup-body n)))
		    (for-each (lambda (a)
                                 (display "\\alignauthor\n")
				 (output a e))
			      (if (pair? body) body (list body)))))
      :after "}\n")
   ;; author
   (let ((old-author (markup-writer-get 'author le)))
      (markup-writer 'author le
         :options (writer-options old-author)		     
         :action (writer-action old-author)))
   ;; ACM category, terms, and keywords
   (markup-writer '&acm-category le
      :options '(:index :section :subsection)
      :before (lambda (n e)
		 (display "\\category{")
		 (display (markup-option n :index))
		 (display "}")
		 (display "{")
		 (display (markup-option n :section))
		 (display "}")
		 (display "{")
		 (display (markup-option n :subsection))
		 (display "}\n["))
      :after "]\n")
   (markup-writer '&acm-terms le
      :before "\\terms{"
      :after "}")
   (markup-writer '&acm-keywords le
      :before "\\keywords{"
      :after "}")
   (markup-writer '&acm-copyright le
      :action (lambda (n e)
		 (display "\\conferenceinfo{")
		 (output (markup-option n :conference) e)
		 (display ",} {")
		 (output (markup-option n :location) e)
		 (display "}\n")
		 (display "\\CopyrightYear{")
		 (output (markup-option n :year) e)
		 (display "}\n")
		 (display "\\crdata{")
		 (output (markup-option n :crdata) e)
		 (display "}\n"))))

;*---------------------------------------------------------------------*/
;*    HTML global customizations                                       */
;*---------------------------------------------------------------------*/
(let ((he (find-engine 'html)))
   (markup-writer '&html-acmproc-abstract he
      :action (lambda (n e)
		 (let* ((ebg (engine-custom e 'abstract-background))
			(bg (or (and (string? ebg) 
				     (> (string-length ebg) 0))
				ebg
				"#cccccc"))
			(exp (p (center (color :bg bg :width 90. 
					   (markup-body n))))))
		    (evaluate-document exp e))))
   ;; ACM category, terms, and keywords
   (markup-writer '&acm-category :action #f)
   (markup-writer '&acm-terms :action #f)
   (markup-writer '&acm-keywords :action #f)
   (markup-writer '&acm-copyright :action #f))
		 
;*---------------------------------------------------------------------*/
;*    abstract ...                                                     */
;*---------------------------------------------------------------------*/
(define-markup (abstract :rest opt :key (class "abstract") postscript)
   (if (engine-format? "latex")
       (section :number #f :title "ABSTRACT" (p (the-body opt)))
       (let ((a (new markup
		   (markup '&html-acmproc-abstract)
                   (loc  &invocation-location)
		   (body (the-body opt)))))
	  (list (if postscript
		    (section :number #f :toc #f :title "Postscript download"
		       postscript))
		(section :number #f :toc #f :class class :title "Abstract" a)
		(section :number #f :toc #f :title "Table of contents"
		   (toc :subsection #t))))))

;*---------------------------------------------------------------------*/
;*    acm-category ...                                                 */
;*---------------------------------------------------------------------*/
(define-markup (acm-category :rest opt :key index section subsection)
   (new markup
      (markup '&acm-category)
      (loc &invocation-location)
      (options (the-options opt))
      (body (the-body opt))))

;*---------------------------------------------------------------------*/
;*    acm-terms ...                                                    */
;*---------------------------------------------------------------------*/
(define-markup (acm-terms :rest opt)
   (new markup
      (markup '&acm-terms)
      (loc &invocation-location)
      (options (the-options opt))
      (body (the-body opt))))

;*---------------------------------------------------------------------*/
;*    acm-keywords ...                                                 */
;*---------------------------------------------------------------------*/
(define-markup (acm-keywords :rest opt)
   (new markup
      (markup '&acm-keywords)
      (loc &invocation-location)
      (options (the-options opt))
      (body (the-body opt))))

;*---------------------------------------------------------------------*/
;*    acm-copyright ...                                                */
;*---------------------------------------------------------------------*/
(define* (acm-copyright :key conference location year crdata
                        :rest opt)
   (let* ((le (find-engine 'latex))
	  (cop (format #f "\\conferenceinfo{~a,} {~a}
\\CopyrightYear{~a}
\\crdata{~a}\n" conference location year crdata))
	  (old (engine-custom le 'predocument)))
      (if (string? old)
	  (engine-custom-set! le 'predocument (string-append cop old))
	  (engine-custom-set! le 'predocument cop))))
   
;*---------------------------------------------------------------------*/
;*    references ...                                                   */
;*---------------------------------------------------------------------*/
(define (references)
   (list "\n\n"
	 (if (engine-format? "latex")
	     (font :size -1 (flush :side 'left (the-bibliography)))
	     (section :title "References"
                      (font :size -1 (the-bibliography))))))
