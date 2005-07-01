;;;;
;;;; html4.skr				-- HTML 4.01 Engine
;;;;
;;;; Copyright © 2004 Erick Gallesio - I3S-CNRS/ESSI <eg@essi.fr>
;;;;
;;;;
;;;; This program is free software; you can redistribute it and/or modify
;;;; it under the terms of the GNU General Public License as published by
;;;; the Free Software Foundation; either version 2 of the License, or
;;;; (at your option) any later version.
;;;;
;;;; This program is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU General Public License
;;;; along with this program; if not, write to the Free Software
;;;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
;;;; USA.
;;;;
;;;;           Author: Erick Gallesio [eg@essi.fr]
;;;;    Creation date: 18-Feb-2004 11:58 (eg)
;;;; Last file update: 26-Feb-2004 21:09 (eg)
;;;;

(define-skribe-module (skribilo engine html4))

(define (find-children node)
  (define (flat l)
    (cond
      ((null? l) l)
      ((pair? l) (append (flat (car l))
			 (flat (cdr l))))
      (else      (list l))))

  (if (markup? node)
      (flat (markup-body node))
      node))

;;; ======================================================================

(let ((le (find-engine 'html)))
  ;;----------------------------------------------------------------------
  ;;	Customizations
  ;;----------------------------------------------------------------------
  (engine-custom-set! le 'html-variant    "html4")
  (engine-custom-set! le 'html4-logo      "http://www.w3.org/Icons/valid-html401")
  (engine-custom-set! le 'html4-validator "http://validator.w3.org/check/referer")

  ;;----------------------------------------------------------------------
  ;;	&html-html ...
  ;;----------------------------------------------------------------------
  (markup-writer '&html-html le
     :before "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\">
<html>\n"
     :after "</html>")

  ;;----------------------------------------------------------------------
  ;;	&html-ending
  ;;----------------------------------------------------------------------
  (let* ((img (engine-custom le 'html4-logo))
	 (url (engine-custom le 'html4-validator))
	 (bottom (list (hrule)
		      (table :width 100.
			 (tr
			    (td :align 'left
			       (font :size -1 [
				 This ,(sc "Html") page has been produced by
				      ,(ref :url (skribe-url) :text "Skribe").
				      ,(linebreak)
				      Last update ,(it (date)).]))
			    (td :align 'right :valign 'top
			       (ref :url url
				  :text (image :url img :width 88 :height 31))))))))
    (markup-writer '&html-ending le
       :before "<div class=\"skribe-ending\">"
       :action (lambda (n e)
		 (let ((body (markup-body n)))
		   (if body
		       (output body #t)
		       (skribe-eval bottom e))))
       :after "</div>\n"))

  ;;----------------------------------------------------------------------
  ;;	color ...
  ;;----------------------------------------------------------------------
  (markup-writer 'color le
     :options '(:bg :fg :width :margin)
     :before (lambda (n e)
	       (let ((m    (markup-option n :margin))
		     (w    (markup-option n :width))
		     (bg   (markup-option n :bg))
		     (fg   (markup-option n :fg)))
		 (when bg
		   (display "<table cellspacing=\"0\"")
		   (html-class n)
		   (printf " cellpadding=\"~a\"" (if m m 0))
		   (if w (printf " width=\"~a\"" (html-width w)))
		   (display "><tbody>\n<tr>")
		   (display "<td bgcolor=\"")
		   (output bg e)
		   (display "\">"))
		 (when fg
		   (display "<span style=\"color:")
		   (output fg e)
		   (display ";\">"))))
     :after (lambda (n e)
	      (when (markup-option n :fg)
		(display "</span>"))
	      (when (markup-option n :bg)
		(display "</td></tr>\n</tbody></table>"))))

  ;;----------------------------------------------------------------------
  ;;	font ...
  ;;----------------------------------------------------------------------
  (markup-writer 'font le
     :options '(:size :face)
     :before (lambda (n e)
	       (let ((face (markup-option n :face))
		     (size (let ((sz (markup-option n :size)))
			     (cond
			       ((or (unspecified? sz) (not sz))
				#f)
			       ((and (number? sz) (or (inexact? sz) (negative? sz)))
				(format "~a%"
					(+ 100
					   (* 20 (inexact->exact (truncate sz))))))
			       ((number? sz)
				sz)
			       (else
				(skribe-error 'font
					      (format "Illegal font size ~s" sz)
					      n))))))
		 (display "<span ")
		 (html-class n)
		 (display "style=\"")
		 (if size (printf "font-size: ~a; " size))
		 (if face (printf "font-family:'~a'; " face))
		 (display "\">")))
     :after "</span>")

  ;;----------------------------------------------------------------------
  ;;	paragraph ...
  ;;----------------------------------------------------------------------
  (copy-markup-writer 'paragraph le
     :validate (lambda (n e)
		 (let ((pred (lambda (x)
			       (and (container? x)
				    (not (memq (markup-markup x) '(font color)))))))
		   (not (any pred (find-children n))))))

  ;;----------------------------------------------------------------------
  ;;	roman ...
  ;;----------------------------------------------------------------------
  (markup-writer 'roman le
     :before "<span style=\"font-family: serif\">"
     :after "</span>")

  ;;----------------------------------------------------------------------
  ;;	table ...
  ;;----------------------------------------------------------------------
  (let ((old-writer (markup-writer-get 'table le)))
    (copy-markup-writer 'table le
	:validate (lambda (n e)
		    (not (null? (markup-body n))))))
)
