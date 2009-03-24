;;; web-book.scm  --  The web book style.
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

(define-module (skribilo package web-book)
  :use-module (skribilo utils syntax)

  :use-module (skribilo ast)
  :use-module (skribilo engine)
  :use-module (skribilo package base))

(skribilo-module-syntax)



;*---------------------------------------------------------------------*/
;*    html customization                                               */
;*---------------------------------------------------------------------*/
(define he (find-engine 'html))
(engine-custom-set! he 'main-browsing-extra #f)
(engine-custom-set! he 'chapter-file #t)

;*---------------------------------------------------------------------*/
;*    main-browsing ...                                                */
;*---------------------------------------------------------------------*/
(define main-browsing 
   (lambda (n e)
      ;; search the document
      (let ((p (ast-document n)))
	 (cond
	    ((document? p)
	     ;; got it
	     (let* ((mt (markup-option p :margin-title))
 		    (r (ref :handle (handle p)
 			    :text (or mt (markup-option p :title))))
 		    (fx (engine-custom e 'web-book-main-browsing-extra)))
		(center
		 (table :width 97. :border 1 :frame 'box
		    :cellpadding 0 :cellspacing 0
		    (tr :bg (engine-custom e 'title-background)
		       (th (let ((text (bold "main page"))
                                 (bg   (engine-custom e 'background)))
                             (if bg (color :fg bg text) text))))
		    (tr :bg (engine-custom e 'background)
		       (td (apply table :width 100. :border 0
				  (tr (td :align 'left 
					 :valign 'top 
					 (bold "top:"))
				     (td :align 'right 
					:valign 'top r))
				  (if (procedure? fx)
				      (list (tr (td :width 100. 
						   :colspan 2 
						   (fx n e))))
				      '()))))))))
	    ((not p)
	     ;; no document!!!
	     #f)))))

;*---------------------------------------------------------------------*/
;*    chapter-browsing ...                                             */
;*---------------------------------------------------------------------*/
(define chapter-browsing
   (lambda (n e)
      (center
       (table :width 97. :border 1 :frame 'box
	  :cellpadding 0 :cellspacing 0
	      (tr :bg (engine-custom e 'title-background)
		  (th (let ((title (bold (markup-option n :title)))
                            (bg    (engine-custom e 'background)))
                        (if bg (color :fg title) title))))
	      (tr :bg (engine-custom e 'background)
		  (td (toc (handle n) :chapter #t :section #t :subsection #t)))))))

;*---------------------------------------------------------------------*/
;*    document-browsing ...                                            */
;*---------------------------------------------------------------------*/
(define document-browsing
   (lambda (n e)
      (let ((chap (find1-down (lambda (n)
				 (is-markup? n 'chapter))
			      n)))
	 (center
	    (table :width 97. :border 1 :frame 'box
	       :cellpadding 0 :cellspacing 0
	       (tr :bg (engine-custom e 'title-background)
		  (th (let ((text (bold (if chap "Chapters" "Sections")))
                            (bg   (engine-custom e 'background)))
                        (if bg (color :fg bg text) text))))
	       (tr :bg (engine-custom e 'background)
		  (td (if chap
			  (toc (handle n) :chapter #t :section #f)
			  (toc (handle n) :section #t :subsection #t)))))))))

;*---------------------------------------------------------------------*/
;*    left margin ...                                                  */
;*---------------------------------------------------------------------*/
(engine-custom-set! he 'left-margin-size 20.)

(engine-custom-set! he 'left-margin
   (lambda (n e) 
      (let ((d (ast-document n))
	    (c (ast-chapter n)))
	 (list (linebreak 1)
	       (main-browsing n e)
	       (if (is-markup? c 'chapter)
		   (list (linebreak 2)
			 (chapter-browsing c e))
		   #f)
	       (if (document? d)
		   (list (linebreak 2)
			 (document-browsing d e))
		   #f)))))

