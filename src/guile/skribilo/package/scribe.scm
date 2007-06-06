;;; scribe.scm  --  Scribe Compatibility kit
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

(define-module (skribilo package scribe)
  :use-module (skribilo engine)
  :autoload   (skribilo utils keywords) (the-options the-body)
  :autoload   (skribilo evaluator)      (load-document)
  :use-module (skribilo biblio)
  :use-module ((skribilo package base) :renamer (symbol-prefix-proc 'skr:))

  :use-module (skribilo lib)
  :use-module (skribilo utils syntax)

  :use-module (ice-9 optargs)
  :use-module (srfi srfi-1)
  :use-module (srfi srfi-13)

  :export (style chapter table-of-contents frame copyright sect euro
	   tab space print-bibliography linebreak ref make-index
	   index print-index scribe-format? scribe-url prgm
	   *scribe-tex-predocument* latex-prelude html-prelude

	   *scribe-background* *scribe-foreground* *scribe-tbackground*
	   *scribe-tforeground* *scribe-title-font* *scribe-author-font*
	   *scribe-chapter-numbering* *scribe-footer* *scribe-prgm-color*))

(fluid-set! current-reader %skribilo-module-reader)

;;; Author: Manuel Serrano, Ludovic Courtès
;;;
;;; Commentary:
;;;
;;; Compatibility layer for Scribe, Skribe's predecessor.  See
;;; http://www-sop.inria.fr/mimosa/fp/Scribe/ for details.
;;;
;;; Code:


;*---------------------------------------------------------------------*/
;*    style ...                                                        */
;*---------------------------------------------------------------------*/
(define (style . styles)
   (define (load-style style)
      (let ((name (cond
		     ((string? style)
		      style)
		     ((symbol? style)
		      (string-append (symbol->string style) ".scr")))))
	 (load-document name)))
   (for-each load-style styles))

;*---------------------------------------------------------------------*/
;*    chapter ...                                                      */
;*---------------------------------------------------------------------*/
(define-markup (chapter :rest opt :key title subtitle split number toc file)
   (apply skr:chapter
	  :title (or title subtitle)
	  :number number
	  :toc toc
	  :file file
	  (the-body opt)))

;*---------------------------------------------------------------------*/
;*    table-of-contents ...                                            */
;*---------------------------------------------------------------------*/
(define* (table-of-contents :key chapter section subsection
			    :rest opts)
   (apply skr:toc opts))

;*---------------------------------------------------------------------*/
;*    frame ...                                                        */
;*---------------------------------------------------------------------*/
(define-markup (frame :rest opt :key width margin)
   (apply skr:frame
	  :width (if (real? width) (* 100 width) width)
	  :margin margin
	  (the-body opt)))

;*---------------------------------------------------------------------*/
;*    copyright ...                                                    */
;*---------------------------------------------------------------------*/
(define (copyright)
   (symbol 'copyright))

;*---------------------------------------------------------------------*/
;*    sect ...                                                         */
;*---------------------------------------------------------------------*/
(define (sect)
   (symbol 'section))

;*---------------------------------------------------------------------*/
;*    euro ...                                                         */
;*---------------------------------------------------------------------*/
(define (euro)
   (symbol 'euro))

;*---------------------------------------------------------------------*/
;*    tab ...                                                          */
;*---------------------------------------------------------------------*/
(define (tab)
   (skr:char #\tab))

;*---------------------------------------------------------------------*/
;*    space ...                                                        */
;*---------------------------------------------------------------------*/
(define (space)
   (skr:char #\space))

;*---------------------------------------------------------------------*/
;*    print-bibliography ...                                           */
;*---------------------------------------------------------------------*/
(define-markup (print-bibliography :rest opts
				   :key all (sort bib-sort/authors))
   (skr:the-bibliography all sort))

;*---------------------------------------------------------------------*/
;*    linebreak ...                                                    */
;*---------------------------------------------------------------------*/
(define (linebreak . lnum)
   (cond
      ((null? lnum)
       (skr:linebreak))
      ((string? (car lnum))
       (skr:linebreak (string->number (car lnum))))
      (else
       (skr:linebreak (car lnum)))))

;*---------------------------------------------------------------------*/
;*    ref ...                                                          */
;*---------------------------------------------------------------------*/
(define* (ref :key scribe url id page figure mark
	      chapter section subsection subsubsection subsubsubsection
	      bib bib+ number
	      :rest opts)
   (let ((bd (the-body opts))
	 (args (concatenate (the-options opts :id))))
      (if id (set! args (cons* :mark id args)))
      (if (pair? bd) (set! args (cons* :text bd args)))
      (apply skr:ref args)))

;*---------------------------------------------------------------------*/
;*    indexes ...                                                      */
;*---------------------------------------------------------------------*/
(define *scribe-indexes*
   (list (cons "theindex" (skr:make-index "theindex"))))

(define (make-index index)
   (let ((i (skr:make-index index)))
      (set! *scribe-indexes* (cons (cons index i) *scribe-indexes*))
      i))

(define* (index :key note index shape :rest opts)
   (let ((i (if (not index)
		"theindex"
		(let ((i (assoc index *scribe-indexes*)))
		   (if (pair? i)
		       (cdr i)
		       (make-index index))))))
      (apply skr:index :note note :index i :shape shape (the-body opts))))

(define* (print-index :key split (char-offset 0) (header-limit 100)
		      :rest opts)
   (apply skr:the-index
	  :split split
	  :char-offset char-offset
	  :header-limit header-limit
	  (map (lambda (i)
		(let ((c (assoc i *scribe-indexes*)))
		   (if (pair? c)
		       (cdr c)
		       (skribe-error 'the-index "Unknown index" i))))
	       (the-body opts))))

;*---------------------------------------------------------------------*/
;*    format?                                                          */
;*---------------------------------------------------------------------*/
(define (scribe-format? fmt) #f)

;*---------------------------------------------------------------------*/
;*    scribe-url ...                                                   */
;*---------------------------------------------------------------------*/
(define (scribe-url) "http://www.nongnu.org/skribilo/")

;*---------------------------------------------------------------------*/
;*    Various configurations                                           */
;*---------------------------------------------------------------------*/
(define *scribe-background* #f)
(define *scribe-foreground* #f)
(define *scribe-tbackground* #f)
(define *scribe-tforeground* #f)
(define *scribe-title-font* #f)
(define *scribe-author-font* #f)
(define *scribe-chapter-numbering* #f)
(define *scribe-footer* #f)
(define *scribe-prgm-color* #f)

;*---------------------------------------------------------------------*/
;*    prgm ...                                                         */
;*---------------------------------------------------------------------*/
(define (prgm :key lnum lnumwidth language bg frame (width 1.)
		     colors (monospace #t)
		     :rest opts)
   (let* ((w (cond
		((real? width) (* width 100.))
		((number? width) width)
		(else 100.)))
	  (body (if language
		    (skr:source :language language (the-body opts))
		    (the-body opts)))
	  (body (if monospace
		    (skr:prog :line lnum body)
		    body))
	  (body (if bg
		    (skr:color :width 100. :bg bg body)
		    body)))
      (skr:frame :width w
		 :border (if frame 1 #f)
		 body)))

;*---------------------------------------------------------------------*/
;*    latex configuration                                              */
;*---------------------------------------------------------------------*/
(define *scribe-tex-predocument* #f)

;*---------------------------------------------------------------------*/
;*    latex-prelude ...                                                */
;*---------------------------------------------------------------------*/
(define (latex-prelude e)
   (if (engine-format? "latex" e)
       (begin
	  (if *scribe-tex-predocument*
	      (engine-custom-set! e 'predocument *scribe-tex-predocument*)))))

;*---------------------------------------------------------------------*/
;*    html-prelude ...                                                 */
;*---------------------------------------------------------------------*/
(define (html-prelude e)
   (if (engine-format? "html" e)
       (begin
	  #f)))

;*---------------------------------------------------------------------*/
;*    prelude                                                          */
;*---------------------------------------------------------------------*/
;; FIXME: I (Ludovic) guess `user-prelude' was supposed to be defined by user
;; documents.  The issue is that the document's name space is not reachable
;; from here.
; (let ((p (user-prelude)))
;    (user-prelude-set! (lambda (e) (p e) (latex-prelude e))))


;;; scribe.scm ends here
