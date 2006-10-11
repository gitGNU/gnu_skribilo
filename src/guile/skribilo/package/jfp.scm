;;; jfp.scm  --  The Skribe style for JFP articles.
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

(define-skribe-module (skribilo package jfp))

;*---------------------------------------------------------------------*/
;*    LaTeX global customizations                                      */
;*---------------------------------------------------------------------*/
(let ((le (find-engine 'latex)))
   (engine-custom-set! le 'documentclass "\\documentclass{jfp}")
   (engine-custom-set! le 'hyperref #f)
   ;; &latex-author
   (markup-writer '&latex-author le
      :action (lambda (n e)
		 (define (&latex-subauthor)
		    (let* ((d (ast-document n))
			   (sa (and (is-markup? d 'document)
				    (markup-option d :head-author))))
		       (if sa
			   (begin
			      (display "[")
			      (output sa e)
			      (display "]")))))
		 (define (&latex-author-1 n)
		    (display "\\author")
		    (&latex-subauthor)
		    (display "{\n")
		    (output n e)
		    (display "}\n"))
		 (define (&latex-author-n n)
		    (display "\\author")
		    (&latex-subauthor)
		    (display "{\n")
		    (output (car n) e)
		    (for-each (lambda (a)
				 (display "\\and ")
				 (output a e))
			      (cdr n))
		    (display "}\n"))
		 (let ((body (markup-body n)))
		    (cond
		       ((is-markup? body 'author)
			(&latex-author-1 body))
		       ((and (list? body)
			     (every? (lambda (b) (is-markup? b 'author))
				     body))
			(&latex-author-n body))
		       (else
			(skribe-error 'author
				      "Illegal `jfp' author"
				      body))))))
   ;; title
   (markup-writer '&latex-title le
      :before (lambda (n e)
		 (let* ((d (ast-document n))
			(st (and (is-markup? d 'document)
				 (markup-option d :head-title))))
		    (if st
			(begin
			   (display "\\title[")
			   (output st e)
			   (display "]{"))
			(display "\\title{"))))
      :after "}\n")
   ;; author
   (let ((old-author (markup-writer-get 'author le)))
      (markup-writer 'author le
         :options (writer-options old-author)		     
         :action (lambda (n e)
		    (let ((name (markup-option n :name))
			  (aff (markup-option n :affiliation))
			  (addr (markup-option n :address))
			  (email (markup-option n :email)))
		       (if name
			   (begin
			      (output name e)
			      (display "\\\\\n")))
		       (if aff
			   (begin
			      (output aff e)
			      (display "\\\\\n")))
		       (if addr
			   (begin
			      (if (pair? addr)
				  (for-each (lambda (a)
					       (output a e)
					       (display "\\\\\n"))
					    addr)
				  (begin
				     (output addr e)
				     (display "\\\\\n")))))
		       (if email
			   (begin
			      (display "\\email{")
			      (output email e)
			      (display "}\\\\\n")))))))
   ;; bib-ref
   (markup-writer 'bib-ref le
      :options '(:bib :text :key)
      :before "("
      :action (lambda (n e)
		 (let ((be (handle-ast (markup-body n))))
		    (if (is-markup? be '&bib-entry)
			(let ((a (markup-option be 'author))
			      (y (markup-option be 'year)))
			   (cond
			      ((and (is-markup? a '&bib-entry-author)
				    (is-markup? y '&bib-entry-year))
			       (let ((ba (markup-body a)))
				  (if (not (string? ba))
				      (output ba e)
				      (let* ((s1 (pregexp-replace* " and "
								   ba
								   " \\& "))
					     (s2 (pregexp-replace* ", [^ ]+"
								   s1
								   "")))
					 (output s2 e)
					 (display ", ")
					 (output y e)))))
			      ((is-markup? y '&bib-entry-year)
			       (skribe-error 'bib-ref
					     "Missing `name' entry"
					     (markup-ident be)))
			      (else
			       (let ((ba (markup-body a)))
				  (if (not (string? ba))
				      (output ba e)
				      (let* ((s1 (pregexp-replace* " and "
								   ba
								   " \\& "))
					     (s2 (pregexp-replace* ", [^ ]+"
								   s1
								   "")))
					 (output s2 e)))))))
			(skribe-error 'bib-ref
				      "Illegal bib-ref"
				      (markup-ident be)))))
      :after ")")
      ;; bib-ref/text
   (markup-writer 'bib-ref le
      :options '(:bib :text :key)
      :predicate (lambda (n e)
		    (markup-option n :key))
      :action (lambda (n e)
		 (output (markup-option n :key) e)))
   ;; &the-bibliography
   (markup-writer '&the-bibliography le
      :before (lambda (n e)
		 (display "{%
\\sloppy
\\sfcode`\\.=1000\\relax
\\newdimen\\bibindent
\\bibindent=0em
\\begin{list}{}{%
        \\settowidth\\labelwidth{[]}%
        \\leftmargin\\labelwidth
        \\advance\\leftmargin\\labelsep
        \\advance\\leftmargin\\bibindent
        \\itemindent -\\bibindent
        \\listparindent \\itemindent
    }%\n"))
      :after (lambda (n e)
		(display "\n\\end{list}}\n")))
   ;; bib-entry
   (markup-writer '&bib-entry le
      :options '(:title)
      :action (lambda (n e)
		 (output n e (markup-writer-get '&bib-entry-body e)))
      :after "\n")
   ;; %bib-entry-title
   (markup-writer '&bib-entry-title le
      :action (lambda (n e)
		 (output (markup-body n) e)))
   ;; %bib-entry-body
   (markup-writer '&bib-entry-body le
      :action (lambda (n e)
		 (define (output-fields descr)
		    (display "\\item[")
		    (let loop ((descr descr)
			       (pending #f)
			       (armed #f)
			       (first #t))
		       (cond
			  ((null? descr)
			   'done)
			  ((pair? (car descr))
			   (if (eq? (caar descr) 'or)
			       (let ((o1 (cadr (car descr))))
				  (if (markup-option n o1)
				      (loop (cons o1 (cdr descr)) 
					    pending 
					    #t
					    #f)
				      (let ((o2 (caddr (car descr))))
					 (loop (cons o2 (cdr descr)) 
					       pending
					       armed
					       #f))))
			       (let ((o (markup-option n (cadr (car descr)))))
				  (if o
				      (begin
					 (if (and pending armed)
					     (output pending e))
					 (output (caar descr) e)
					 (output o e)
					 (if (pair? (cddr (car descr)))
					     (output (caddr (car descr)) e))
					 (loop (cdr descr) #f #t #f))
				      (loop (cdr descr) pending armed #f)))))
			  ((symbol? (car descr))
			   (let ((o (markup-option n (car descr))))
			      (if o 
				  (begin
				     (if (and armed pending)
					 (output pending e))
				     (output o e)
				     (if first
					 (display "]"))
				     (loop (cdr descr) #f #t #f))
				  (loop (cdr descr) pending armed #f))))
			  ((null? (cdr descr))
			   (output (car descr) e))
			  ((string? (car descr))
			   (loop (cdr descr) 
				 (if pending pending (car descr))
				 armed
				 #f))
			  (else
			   (skribe-error 'output-bib-fields
					 "Illegal description"
					 (car descr))))))
		 (output-fields
		  (case (markup-option n 'kind)
		     ((techreport)
		      `(author (" (" year ")") " " (or title url) ". " 
			       number ", " institution ", "
			       address ", " month ", "
			       ("pp. " pages) "."))
		     ((article)
		      `(author (" (" year ")") " " (or title url) ". "
			       journal ", " volume ", " ("(" number ")") ", "
			       address ", " month ", " 
			       ("pp. " pages) "."))
		     ((inproceedings)
		      `(author (" (" year ")") " " (or title url) ". " 
			       book(or title url) ", " series ", " ("(" number ")") ", "
			       address ", " month ", " 
			       ("pp. " pages) "."))
		     ((book)
		      '(author (" (" year ")") " " (or title url) ". " 
			       publisher ", " address
			       ", " month ", " ("pp. " pages) "."))
		     ((phdthesis)
		      '(author (" (" year ")") " " (or title url) ". " type ", " 
			       school ", " address
			       ", " month "."))
		     ((misc)
		      '(author (" (" year ")") " " (or title url) ". "
			       publisher ", " address
			       ", " month "."))
		     (else
		      '(author (" (" year ")") " " (or title url) ". "
			       publisher ", " address
			       ", " month ", " ("pp. " pages) "."))))))
   ;; abstract
   (markup-writer 'jfp-abstract le
       :options '(postscript)
       :before "\\begin{abstract}\n"
       :after "\\end{abstract}\n"))

;*---------------------------------------------------------------------*/
;*    HTML global customizations                                       */
;*---------------------------------------------------------------------*/
(let ((he (find-engine 'html)))
   (markup-writer '&html-jfp-abstract he
      :action (lambda (n e)
		 (let* ((bg (engine-custom e 'abstract-background))
                        (exp (p (if bg
				    (center (color :bg bg :width 90. 
					       (it (markup-body n))))
				    (it (markup-body n))))))
                    (skribe-eval exp e)))))
		 
;*---------------------------------------------------------------------*/
;*    abstract ...                                                     */
;*---------------------------------------------------------------------*/
(define-markup (abstract #!rest opt #!key postscript)
   (if (engine-format? "latex")
       (new markup
	  (markup 'jfp-abstract)
	  (body (p (the-body opt))))
       (let ((a (new markup
		   (markup '&html-jfp-abstract)
		   (body (the-body opt)))))
	  (list (if postscript
		    (section :number #f :toc #f :title "Postscript download"
                             postscript))
		(section :number #f :toc #f :title "Abstract" a)
		(section :number #f :toc #f :title "Table of contents"
                         (toc :subsection #t))))))

;*---------------------------------------------------------------------*/
;*    references ...                                                   */
;*---------------------------------------------------------------------*/
(define (references)
   (list "\n\n"
	 (section :title "References" :class "references"
	    :number (not (engine-format? "latex"))
	    (font :size -1 (the-bibliography)))))

