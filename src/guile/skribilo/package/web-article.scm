;;; web-article.scm  --  A style to produce web articles.
;;;
;;; Copyright 2003, 2004  Manuel Serrano
;;; Copyright 2007  Ludovic Courtès <ludo@chbouib.org>
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

(define-module (skribilo package web-article)
  :use-module (skribilo utils syntax)

  :use-module (skribilo ast)
  :use-module (skribilo engine)
  :use-module (skribilo writer)
  :use-module (skribilo package base)

  :autoload   (skribilo output)         (output)
  :autoload   (skribilo evaluator)      (evaluate-document)
  :autoload   (skribilo engine html)    (html-width html-title-authors)
  :autoload   (skribilo utils strings)  (string-canonicalize)

  :use-module (srfi srfi-1))

(fluid-set! current-reader %skribilo-module-reader)


;*---------------------------------------------------------------------*/
;*    &web-article-load-options ...                                    */
;*---------------------------------------------------------------------*/
(define &web-article-load-options (*load-options*))

;*---------------------------------------------------------------------*/
;*    web-article-body-width ...                                       */
;*---------------------------------------------------------------------*/
(define (web-article-body-width e)
   (let ((w (engine-custom e 'body-width)))
      (if (or (number? w) (string? w)) w 98.)))

;*---------------------------------------------------------------------*/
;*    html-document-title-web ...                                      */
;*---------------------------------------------------------------------*/
(define (html-document-title-web n e)
   (let* ((title (markup-body n))
	  (authors (markup-option n 'author))
	  (tbg (engine-custom e 'title-background))
	  (tfg (engine-custom e 'title-foreground))
	  (tfont (engine-custom e 'title-font)))
      (format #t "<center><table cellspacing='0' cellpadding='0' width=\"~a\" class=\"skribetitle\"><tbody>\n<tr>"
	      (html-width (web-article-body-width e)))
      (if (string? tbg)
	  (format #t "<td bgcolor=\"~a\">" tbg)
	  (display "<td>"))
      (if (string? tfg)
	  (format #t "<font color=\"~a\">" tfg))
      (if title
	  (begin
	     (display "<center>")
	     (if (string? tfont)
		 (begin
		    (format #t "<font ~a><b>" tfont)
		    (output title e)
		    (display "</b></font>"))
		 (begin
		    (display "<h1>")
		    (output title e)
		    (display "</h1>")))
	     (display "</center>\n")))
      (if (not authors)
	  (display "\n")
	  (html-title-authors authors e))
      (if (string? tfg)
	  (display "</font>"))
      (display "</td></tr></tbody></table></center>\n")))

;*---------------------------------------------------------------------*/
;*    web-article-css-document-title ...                               */
;*---------------------------------------------------------------------*/
(define (web-article-css-document-title n e)
   (let* ((title (markup-body n))
	  (authors (markup-option n 'author))
	  (id (markup-ident n)))
      ;; the title
      (format #t "<div id=\"~a\" class=\"document-title-title\">\n"
	      (string-canonicalize id))
      (output title e)
      (display "</div>\n")
      ;; the authors
      (format #t "<div id=\"~a\" class=\"document-title-authors\">\n"
	      (string-canonicalize id))
      (for-each (lambda (a) (output a e))
		(cond
		   ((is-markup? authors 'author)
		    (list authors))
		   ((list? authors)
		    authors)
		   (else
		    '())))
      (display "</div>\n")))

;*---------------------------------------------------------------------*/
;*    web-article-css-author ...                                       */
;*---------------------------------------------------------------------*/
(define (web-article-css-author n e)
   (let ((name (markup-option n :name))
	 (title (markup-option n :title))
	 (affiliation (markup-option n :affiliation))
	 (email (markup-option n :email))
	 (url (markup-option n :url))
	 (address (markup-option n :address))
	 (phone (markup-option n :phone)))
      (when name
	 (format #t "<span class=\"document-author-name\" id=\"~a\">"
		 (string-canonicalize (markup-ident n)))
	 (output name e)
	 (display "</span>\n"))
      (when title
	 (format #t "<span class=\"document-author-title\" id=\"~a\">"
		 (string-canonicalize (markup-ident n)))
	 (output title e)
	 (display "</span>\n"))
      (when affiliation
	 (format #t "<span class=\"document-author-affiliation\" id=\"~a\">"
		 (string-canonicalize (markup-ident n)))
	 (output affiliation e)
	 (display "</span>\n"))
      (when (pair? address)
	 (format #t "<span class=\"document-author-address\" id=\"~a\">"
		 (string-canonicalize (markup-ident n)))
	 (for-each (lambda (a)
		      (output a e)
		      (newline))
		   address)
	 (display "</span>\n"))
      (when phone
	 (format #t "<span class=\"document-author-phone\" id=\"~a\">"
		 (string-canonicalize (markup-ident n)))
	 (output phone e)
	 (display "</span>\n"))
      (when email
	 (format #t "<span class=\"document-author-email\" id=\"~a\">"
		 (string-canonicalize (markup-ident n)))
	 (output email e)
	 (display "</span>\n"))
      (when url
	 (format #t "<span class=\"document-author-url\" id=\"~a\">"
		 (string-canonicalize (markup-ident n)))
	 (output url e)
	 (display "</span>\n"))))

;*---------------------------------------------------------------------*/
;*    HTML settings                                                    */
;*---------------------------------------------------------------------*/
(define (web-article-modern-setup he)
   (let ((sec (markup-writer-get 'section he))
	 (ft (markup-writer-get '&html-footnotes he)))
      ;; &html-document-title
      (markup-writer '&html-document-title he
	 :action html-document-title-web)
      ;; section
      (markup-writer 'section he
	 :options 'all
	 :before "<br>"
	 :action (lambda (n e)
		    (let ((e1 (make-engine 'html-web :delegate e))
			  (bg (engine-custom he 'section-background)))
		       (markup-writer 'section e1
			  :options 'all
			  :action (lambda (n e2) (output n e sec)))
		       (evaluate-document
			(center (color :width (web-article-body-width e)
				   :margin 5 :bg bg n))
			e1))))
      ;; &html-footnotes
      (markup-writer '&html-footnotes he
	 :options 'all
	 :before "<br>"
	 :action (lambda (n e)
		    (let ((e1 (make-engine 'html-web :delegate e))
			  (bg (engine-custom he 'section-background))
			  (fg (engine-custom he 'subsection-title-foreground)))
		       (markup-writer '&html-footnotes e1
			  :options 'all
			  :action (lambda (n e2)
				     (invoke (writer-action ft) n e)))
		       (evaluate-document
			(center (color :width (web-article-body-width e)
				   :margin 5 :bg bg :fg fg n))
			e1))))))

;*---------------------------------------------------------------------*/
;*    web-article-css-setup ...                                        */
;*---------------------------------------------------------------------*/
(define (web-article-css-setup he)
   (let ((sec (markup-writer-get 'section he))
	 (ft (markup-writer-get '&html-footnotes he)))
      ;; &html-document-title
      (markup-writer '&html-document-title he
	 :before (lambda (n e)
		    (format #t "<div id=\"~a\" class=\"document-title\">\n"
			    (string-canonicalize (markup-ident n))))
	 :action web-article-css-document-title
	 :after "</div>\n")
      ;; author
      (markup-writer 'author he
	 :options '(:name :title :affiliation :email :url :address :phone :photo :align)
	 :before (lambda (n e)
		    (format #t "<span id=\"~a\" class=\"document-author\">\n"
			    (string-canonicalize (markup-ident n))))
	 :action web-article-css-author
	 :after "</span\n")
      ;; section
      (markup-writer 'section he
	 :options 'all
	 :before (lambda (n e)
		    (format #t "<div class=\"section\" id=\"~a\">"
			    (string-canonicalize (markup-ident n))))
	 :action (lambda (n e) (output n e sec))
	 :after "</div>\n")
      ;; &html-footnotes
      (markup-writer '&html-footnotes he
	 :options 'all
	 :before (lambda (n e)
		    (format #t "<div class=\"footnotes\" id=\"~a\">"
			    (string-canonicalize (markup-ident n))))
	 :action (lambda (n e)
		    (output n e ft))
	 :after "</div>\n")))

;*---------------------------------------------------------------------*/
;*    Setup ...                                                        */
;*---------------------------------------------------------------------*/
(let* ((opt &web-article-load-options)
       (p (memq :style opt))
       (css (memq :css opt))
       (he (find-engine 'html)))
   (cond
      ((and (pair? p) (pair? (cdr p)) (eq? (cadr p) 'css))
       (web-article-css-setup he))
      ((and (pair? css) (pair? (cdr css)) (string? (cadr css)))
       (engine-custom-set! he 'css (cadr css))
       (web-article-css-setup he))
      (else
       (web-article-modern-setup he))))
