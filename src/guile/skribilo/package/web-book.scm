;*=====================================================================*/
;*    serrano/prgm/project/skribe/skr/web-book.skr                     */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Sep  1 10:54:32 2003                          */
;*    Last change :  Mon Nov  8 10:43:46 2004 (eg)                     */
;*    Copyright   :  2003-04 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    The Skribe web book style.                                       */
;*=====================================================================*/

(define-skribe-module (skribilo package web-book))

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
		       (th (color :fg (engine-custom e 'background) 
			      (bold "main page"))))
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
		  (th (color :fg (engine-custom e 'background) 
			     (bold (markup-option n :title)))))
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
		  (th (color :fg (engine-custom e 'background)
			 (bold (if chap "Chapters" "Sections")))))
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

