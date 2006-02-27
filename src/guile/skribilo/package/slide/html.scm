;;; html.scm  --  HTML implementation of the `slide' package.
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
;;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
;;; USA.

(define-skribe-module (skribilo package slide html)
  :use-module (skribilo package slide))


(define-public (%slide-html-initialize!)
  (let ((he (find-engine 'html)))
    (skribe-message "HTML slides setup...\n")
    ;; &html-page-title
    (markup-writer '&html-document-title he
       ;;:predicate (lambda (n e) %slide-initialized)
       :action html-slide-title)
    ;; slide
    (markup-writer 'slide he
       :options '(:title :number :transition :toc :bg)
       :before (lambda (n e)
		  (printf "<a name=\"~a\">" (markup-ident n))
		  (display "<br>\n"))
       :action (lambda (n e)
		  (let ((nb (markup-option n :number))
			(t (markup-option n :title)))
		     (skribe-eval
		      (center
			 (color :width (slide-body-width e)
			    :bg (or (markup-option n :bg) "#ffffff")
			    (table :width 100.
			       (tr (th :align 'left
				      (list
				       (if nb
					   (format #f "~a / ~a -- " nb
						   (slide-number)))
				       t)))
			       (tr (td (hrule)))
			       (tr (td :width 100. :align 'left
				      (markup-body n))))
			    (linebreak)))
		      e)))
       :after "<br>")
    ;; slide-vspace
    (markup-writer 'slide-vspace he
       :action (lambda (n e) (display "<br>")))))


;*---------------------------------------------------------------------*/
;*    slide-body-width ...                                             */
;*---------------------------------------------------------------------*/
(define (slide-body-width e)
   (let ((w (engine-custom e 'body-width)))
      (if (or (number? w) (string? w)) w 95.)))

;*---------------------------------------------------------------------*/
;*    html-slide-title ...                                             */
;*---------------------------------------------------------------------*/
(define (html-slide-title n e)
   (let* ((title (markup-body n))
	  (authors (markup-option n 'author))
	  (tbg (engine-custom e 'title-background))
	  (tfg (engine-custom e 'title-foreground))
	  (tfont (engine-custom e 'title-font)))
      (printf "<center><table cellspacing='0' cellpadding='0' width=\"~a\" class=\"skribetitle\"><tbody>\n<tr>"
	      (html-width (slide-body-width e)))
      (if (string? tbg)
	  (printf "<td bgcolor=\"~a\">" tbg)
	  (display "<td>"))
      (if (string? tfg)
	  (printf "<font color=\"~a\">" tfg))
      (if title
	  (begin
	     (display "<center>")
	     (if (string? tfont)
		 (begin
		    (printf "<font ~a><strong>" tfont)
		    (output title e)
		    (display "</strong></font>"))
		 (begin
		    (printf "<div class=\"skribetitle\"><strong><big><big><big>")
		    (output title e)
		    (display "</big></big></big></strong</div>")))
	     (display "</center>\n")))
      (if (not authors)
	  (display "\n")
	  (html-title-authors authors e))
      (if (string? tfg)
	  (display "</font>"))
      (display "</td></tr></tbody></table></center>\n")))



;;;
;;; Initialization.
;;;

(%slide-html-initialize!)


;;; arch-tag: 8be0cdf2-b755-4baa-baf6-739cdd00e193
