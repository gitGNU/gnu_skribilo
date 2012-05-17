;;; letter.scm  --  Skribe style for letters
;;; -*- coding: iso-8859-1 -*-
;;;
;;; Copyright 2003, 2004  Manuel Serrano
;;; Copyright 2007  Ludovic Courtès <ludo@chbouib.org>
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

(define-module (skribilo package letter)
  :use-module (skribilo ast)
  :use-module (skribilo engine)
  :use-module (skribilo writer)
  :use-module (skribilo lib)
  :autoload   (skribilo output)          (output)
  :autoload   (skribilo utils keywords)  (the-body the-options)
  :use-module ((skribilo package base) :renamer (symbol-prefix-proc 'skr:))
  :use-module (srfi srfi-1)

  :use-module (skribilo utils syntax)
  :use-module (ice-9 optargs))

(skribilo-module-syntax)


;*---------------------------------------------------------------------*/
;*    document                                                         */
;*---------------------------------------------------------------------*/
(define-markup (document :rest opt
		  :key (ident #f) (class "letter")
		  where date author
		  &skribe-eval-location)
   (let* ((ubody (the-body opt))
	  (body (list (new markup
			 (markup '&letter-where)
			 (loc &invocation-location)
			 (options `((:where ,where)
				    (:date ,date)
				    (:author ,author))))
		      ubody)))
      (apply skr:document
	     :author #f :title #f
	     (append (concatenate
		      (the-options opt :where :date :author :title))
		     body))))

;*---------------------------------------------------------------------*/
;*    LaTeX configuration                                              */
;*---------------------------------------------------------------------*/
(let ((le (find-engine 'latex)))
   (engine-custom-set! le 'documentclass "\\documentclass[12pt]{letter}\n")
   (engine-custom-set! le 'maketitle #f)
   ;; &letter-where
   (markup-writer '&letter-where le
      :before "\\begin{raggedright}\n"
      :action (lambda (n e)
		 (let* ((w (markup-option n :where))
			(d (markup-option n :date))
			(a (markup-option n :author))
			(hd (if (and w d)
				(list w ", " d)
				(or w d)))
			(ne (copy-engine 'author e)))
		    ;; author
		    (markup-writer 'author ne
		       :options '(:name :title :affiliation :email :url
                                  :address :phone :photo :align :header)
		       :action (lambda (n e)
				  (let ((name (markup-option n :name))
					(title (markup-option n :title))
					(affiliation (markup-option n :affiliation))
					(email (markup-option n :email))
					(url (markup-option n :url))
					(address (markup-option n :address))
					(phone (markup-option n :phone)))
				     (define (row n)
					(output n e)
					(when hd
					   (display "\\hfill ")
					   (output hd e)
					   (set! hd #f))
					(display "\\\\\n"))
				     ;; name
				     (if name (row name))
				     ;; title
				     (if title (row title))
				     ;; affiliation
				     (if affiliation (row affiliation))
				     ;; address
				     (if (pair? address)
					 (for-each row address))
				     ;; telephone
				     (if phone (row phone))
				     ;; email
				     (if email (row email))
				     ;; url
				     (if url (row url)))))
		    ;; emit the author
		    (if a
			(output a ne)
			(output hd e))))
      :after "\\end{raggedright}\n\\vspace{1cm}\n\n"))

;*---------------------------------------------------------------------*/
;*    HTML configuration                                               */
;*---------------------------------------------------------------------*/
(let ((he (find-engine 'html)))
   ;; &letter-where
   (markup-writer '&letter-where he
      :before "<table width=\"100%\">\n"
      :action (lambda (n e)
		 (let* ((w (markup-option n :where))
			(d (markup-option n :date))
			(a (markup-option n :author))
			(hd (if (and w d)
				(list w ", " d)
				(or w d)))
			(ne (copy-engine 'author e)))
		    ;; author
		    (markup-writer 'author ne
		       :options '(:name :title :affiliation :email :url :address :phone :photo :align :header)
		       :action (lambda (n e)
				  (let ((name (markup-option n :name))
					(title (markup-option n :title))
					(affiliation (markup-option n :affiliation))
					(email (markup-option n :email))
					(url (markup-option n :url))
					(address (markup-option n :address))
					(phone (markup-option n :phone)))
				     (define (row n)
					(display "<tr><td align='left'>")
					(output n e)
					(when hd
					   (display "</td><td align='right'>")
					   (output hd e)
					   (set! hd #f))
					(display "</td></tr>\n"))
				     ;; name
				     (if name (row name))
				     ;; title
				     (if title (row title))
				     ;; affiliation
				     (if affiliation (row affiliation))
				     ;; address
				     (if (pair? address)
					 (for-each row address))
				     ;; telephone
				     (if phone (row phone))
				     ;; email
				     (if email (row email))
				     ;; url
				     (if url (row url)))))
		    ;; emit the author
		    (if a
			(output a ne)
			(output hd e))))
      :after "</table>\n<hr>\n\n"))
