;;; lncs.scm  --  The Skribilo style for LNCS articles.
;;;
;;; Copyright 2003, 2004  Manuel Serrano
;;; Copyright 2007  Ludovic Courtès <ludovic.courtes@laas.fr>
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

(define-module (skribilo package lncs)
  :use-module (skribilo ast)
  :use-module (skribilo engine)
  :use-module (skribilo writer)
  :autoload   (skribilo output)         (output)
  :autoload   (skribilo package base)   (section font flush
                                         toc the-bibliography)
  :autoload   (skribilo utils keywords) (the-options the-body)

  :use-module (skribilo lib)
  :use-module (skribilo utils syntax)

  :use-module (ice-9 optargs)

  :export (abstract references))

(fluid-set! current-reader %skribilo-module-reader)


;*---------------------------------------------------------------------*/
;*    LaTeX global customizations                                      */
;*---------------------------------------------------------------------*/
(let ((le (find-engine 'latex)))
   (engine-custom-set! le 'documentclass "\\documentclass{llncs}")
   (engine-custom-set! le 'class-has-chapters? #f)
   ;; &latex-author
   (markup-writer '&latex-author le
      :action (lambda (n e)
		 (define (&latex-inst-body n)
		    (let ((affiliation (markup-option n :affiliation))
			  (address (markup-option n :address)))
		       (when affiliation (output affiliation e) (display ", "))
		       (when address
			  (for-each (lambda (a) (output a e) (display " "))
				    address)
			  (newline))))
		 (define (&latex-inst-n i)
		    (display "\\institute{\n")
		    (&latex-inst-body (car i))
		    (for-each (lambda (n)
				 (display "\\and\n")
				 (&latex-inst-body n))
			      (cdr i))
		    (display "}\n"))
		 (define (&latex-author-1 n)
		    (display "\\author{\n")
		    (output n e)
		    (display "}\n"))
		 (define (&latex-author-n n)
		    (display "\\author{\n")
		    (output (car n) e)
		    (for-each (lambda (a)
				 (display " and ")
				 (output a e))
			      (cdr n))
		    (display "}\n"))
		 (let ((body (markup-body n)))
		    (cond
		       ((is-markup? body 'author)
			(markup-option-add! n 'inst 1)
			(&latex-author-1 body)
			(&latex-inst-n (list body)))
		       ((and (list? body)
			     (every? (lambda (b) (is-markup? b 'author))
				     body))
			(define (institute=? n1 n2)
			   (let ((aff1 (markup-option n1 :affiliation))
				 (add1 (markup-option n1 :address))
				 (aff2 (markup-option n2 :affiliation))
				 (add2 (markup-option n2 :address)))
			      (and (equal? aff1 aff2) (equal? add1 add2))))
			(define (search-institute n i j)
			   (cond
			      ((null? i)
			       #f)
			      ((institute=? n (car i))
			       j)
			      (else
			       (search-institute n (cdr i) (- j 1)))))
			(if (null? (cdr body))
			    (begin
			       (markup-option-add! (car body) 'inst 1)
			       (&latex-author-1 (car body))
			       (&latex-inst-n body))
			    ;; collect the institutes
			    (let loop ((ns body)
				       (is '())
				       (j 1))
			       (if (null? ns)
				   (begin
				      (&latex-author-n body)
				      (&latex-inst-n (reverse! is)))
				   (let* ((n (car ns))
					  (si (search-institute n is (- j 1))))
				      (if (integer? si)
					  (begin
					     (markup-option-add! n 'inst si)
					     (loop (cdr ns) is j))
					  (begin
					     (markup-option-add! n 'inst j)
					     (loop (cdr ns)
						   (cons n is)
						   (+ 1 j)))))))))
		       (else
			(skribe-error 'author
				      "Illegal `lncs' author"
				      body))))))
   ;; author
   (let ((old-author (markup-writer-get 'author le)))
      (markup-writer 'author le
         :options (writer-options old-author)		     
         :action (lambda (n e)
		    (let ((name (markup-option n :name))
			  (title (markup-option n :title))
			  (inst (markup-option n 'inst)))
		       (if name (output name e))
		       (if title (output title e))
		       (if inst (printf "\\inst{~a}\n" inst)))))))

;*---------------------------------------------------------------------*/
;*    HTML global customizations                                       */
;*---------------------------------------------------------------------*/
(let ((he (find-engine 'html)))
   (markup-writer '&html-lncs-abstract he
      :action (lambda (n e)
		 (let* ((bg (or (engine-custom e 'abstract-background)
				"#cccccc"))
			(exp (p (center (color :bg bg :width 90. 
					   (markup-body n))))))
		    (skribe-eval exp e)))))

;*---------------------------------------------------------------------*/
;*    abstract ...                                                     */
;*---------------------------------------------------------------------*/
(define-markup (abstract :rest opt :key postscript)
   (if (engine-format? "latex")
       (section :number #f :title "ABSTRACT" (p (the-body opt)))
       (let ((a (new markup
		   (markup '&html-lncs-abstract)
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
	 (if (engine-format? "latex")
	     (font :size -1 (flush :side 'left (the-bibliography)))
	     (section :title "References"
                      (font :size -1 (the-bibliography))))))
