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
;;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301,
;;; USA.

(define-module (skribilo package slide html)
  :use-module (skribilo utils syntax)

  :use-module (skribilo ast)
  :use-module (skribilo engine)
  :use-module (skribilo writer)
  :autoload   (skribilo resolve)     (resolve!)
  :autoload   (skribilo output)      (output)
  :autoload   (skribilo evaluator)   (evaluate-document)
  :autoload   (skribilo engine html) (html-width html-title-authors)

  :use-module (skribilo package slide)
  :use-module ((skribilo package base) :select (ref))) 


(fluid-set! current-reader %skribilo-module-reader)



(define-public (%slide-html-initialize!)
  (let ((he (find-engine 'html)))
    (display "HTML slides setup...\n" (current-error-port))

    ;; &html-page-title
    (markup-writer '&html-document-title he
       ;;:predicate (lambda (n e) %slide-initialized)
       :action html-slide-title)

    ;; slide
    (markup-writer 'slide he
       :options '(:title :number :transition :toc :bg)
       :before (lambda (n e)
                  (display "<br>\n")
		  (format #t "<a name=\"~a\">" (markup-ident n)))
       :action (lambda (n e)
		  (let ((nb (markup-option n :number))
			(t (markup-option n :title))
                        (class (markup-class n)))
                     (if class
                         (let ((title-class (string-append class "-title")))
                           ;; When a class is specified, let the user play
                           ;; with CSS.
                           (format #t "\n<div class=\"~a\">" class)
                           (format #t "<div class=\"~a\">" title-class)
                           (format #t "~a / ~a -- " nb (slide-number))
                           (output t e)
                           (display "</div>\n")
                           (output (markup-body n) e)
                           (display "\n</div>\n"))
                         ;; When no class is specified, do HTML tricks.
                         (evaluate-document
                          (center
                           (color :width (slide-body-width e)
                                  :bg (or (markup-option n :bg) "#ffffff")
                                  (table :width 100.
                                         (tr (th :align 'left
                                                 (list
                                                  (if nb
                                                      (format #f "~a / ~a -- "
                                                              nb
                                                              (slide-number)))
                                                  t)))
                                         (tr (td (hrule)))
                                         (tr (td :width 100. :align 'left
                                                 (markup-body n))))
                                  (linebreak)))
                          e))))
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
      (format #t "<center><table cellspacing='0' cellpadding='0' width=\"~a\" class=\"skribilo-title\"><tbody>\n<tr>"
	      (html-width (slide-body-width e)))
      (if (string? tbg)
	  (format #t  "<td bgcolor=\"~a\">" tbg)
	  (display "<td>"))
      (if (string? tfg)
	  (format #t  "<font color=\"~a\">" tfg))
      (if title
	  (begin
	     (display "<center>")
	     (if (string? tfont)
		 (begin
		    (format #t  "<font ~a><strong>" tfont)
		    (output title e)
		    (display "</strong></font>"))
		 (begin
		    (display "<div class=\"skribilo-title\"><strong><big><big><big>")
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
;;; Slide topics/subtopics.
;;;

(markup-writer 'slide-topic (find-engine 'html)
   :options '(:title :outline? :class :ident)
   :action (lambda (n e)
	      (let ((title (markup-option n :title))
		    (body (markup-body n))
                    (class (markup-class n)))
                 ;; top-level class
                 (if class (format #t "\n<div class=\"~a\">" class))

                 ;; the title
                 (if class
                     (format #t "\n<div class=\"~a-title\">" class)
                     (display "\n<h2 class=\"slide-topic:title\">"))
		 (if (markup-ident n)
		     (format #t "<a name=\"~a\"></a>" (markup-ident n)))
		 (output title e)
                 (if class
                     (display "</div>\n")
                     (display "</h2> <br>\n"))

                 ;; pointers to the slides
                 (if class
                     (format #t "\n<div class=\"~a-slide-list\">"
                             class)
                     (display "\n<div class=\"slide-topic:slide-list\">"))
		 (for-each (lambda (s)
                             (let* ((title (markup-option s :title))
                                    (ident (markup-ident s))
                                    (sref (ref :text title :ident ident))
                                    (sref* (resolve! sref e `((parent ,n)))))
                               (output sref* e)
                               (display "&nbsp;--&nbsp;")))
			   (filter (lambda (n)
				      (or (is-markup? n 'slide-subtopic)
					  (is-markup? n 'slide)))
				   (markup-body n)))
		 (display "\n</div> <!-- slide-topic:slide-list -->")

                 (if class
                     (display "\n</div> <!-- slide-topic -->\n")
                     (display "\n<hr><br>\n"))

		 ;; the slides
		 (output (markup-body n) e))))


;;;
;;; Initialization.
;;;

(%slide-html-initialize!)


;;; arch-tag: 8be0cdf2-b755-4baa-baf6-739cdd00e193
