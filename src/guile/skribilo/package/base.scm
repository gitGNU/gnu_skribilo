;;; base.scm -- The base markup package of Skribe/Skribilo.
;;;
;;; Copyright 2005, 2006, 2007  Ludovic Courtès  <ludo@gnu.org>
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

(define-module (skribilo package base)
  :use-syntax (skribilo lib)
  :use-syntax (skribilo utils syntax)
  :use-syntax (ice-9 optargs)

  :use-module (skribilo ast)
  :use-module (skribilo resolve)
  :use-module (skribilo utils keywords)
  :autoload   (srfi srfi-1)        (every any filter)
  :autoload   (skribilo evaluator) (include-document)
  :autoload   (skribilo engine)    (engine?)
  :autoload   (skribilo parameters)(*document-path*)

  ;; optional ``sub-packages''
  :autoload   (skribilo biblio)    (*bib-table* resolve-bib
                                    bib-load! bib-add! bib-sort-refs/number)
  :autoload   (skribilo source)    (language? source-read-lines source-fontify)
  :autoload   (skribilo prog)      (make-prog-body resolve-line)
  :autoload   (skribilo index)     (make-index-table default-index)
  :autoload   (skribilo sui)       (load-sui sui-ref->url)

  :replace (symbol))

(fluid-set! current-reader %skribilo-module-reader)

;;; Author: Manuel Serrano
;;; Commentary:
;;;
;;; This module contains all the core markups of Skribe/Skribilo.
;;;
;;; Code:


;;; The contents of the file below are (almost) unchanged compared to Skribe
;;; 1.2d's `api.scm' file found in the `common' directory.



;*---------------------------------------------------------------------*/
;*    include ...                                                      */
;*---------------------------------------------------------------------*/
(define-public (include file)
   (if (not (string? file))
       (skribe-error 'include "Illegal file (string expected)" file)
       (include-document file)))

;*---------------------------------------------------------------------*/
;*    document ...                                                     */
;*---------------------------------------------------------------------*/
(define-markup (document :rest
			 opts
			 :key
			 (ident #f) (class "document")
			 (title #f) (html-title #f) (author #f)
			 (ending #f) (keywords '()) (env '()))
   (new document
      (markup 'document)
      (ident (or ident
		 (ast->string title)
		 (symbol->string (gensym "document"))))
      (class class)
      (loc   &invocation-location)
      (required-options '(:title :author :ending))
      (options (the-options opts :ident :class :env))
      (body (the-body opts))
      (env (append env
		   (list (list 'chapter-counter 0) (list 'chapter-env '())
			 (list 'section-counter 0) (list 'section-env '())
			 (list 'footnote-counter 0) (list 'footnote-env '())
			 (list 'figure-counter 0) (list 'figure-env '()))))))

;*---------------------------------------------------------------------*/
;*    keyword-list->comma-separated ...                                */
;*---------------------------------------------------------------------*/
(define-public (keyword-list->comma-separated kw*)
  ;; Turn the the list of keywords (which may be strings or other markups)
  ;; KW* into a markup where the elements of KW* are comma-separated.  This
  ;; may commonly be used in handling the `:keywords' option of `document'.
  (let loop ((kw* kw*) (result '()))
    (if (null? kw*)
        (reverse! result)
        (loop (cdr kw*)
              (cons* (if (pair? (cdr kw*)) ", " "")
                     (car kw*) result)))))

;*---------------------------------------------------------------------*/
;*    author ...                                                       */
;*---------------------------------------------------------------------*/
(define-markup (author :rest
		       opts
		       :key
		       (ident #f) (class "author")
		       name
		       (title #f)
		       (affiliation #f)
		       (email #f)
		       (url #f)
		       (address #f)
		       (phone #f)
		       (photo #f)
		       (align 'center))
   (if (not (memq align '(center left right)))
       (skribe-error 'author "Illegal align value" align)
       (new container
	  (markup 'author)
	  (ident (or ident (symbol->string (gensym "author"))))
	  (class class)
          (loc   &invocation-location)
	  (required-options '(:name :title :affiliation :email :url :address :phone :photo :align))
	  (options `((:name ,name)
		     (:align ,align)
		     ,@(the-options opts :ident :class)))
	  (body #f))))

;*---------------------------------------------------------------------*/
;*    toc ...                                                          */
;*---------------------------------------------------------------------*/
(define-markup (toc :rest
		    opts
		    :key
		    (ident #f) (class "toc")
		    (chapter #t) (section #t) (subsection #f)
		    (subsubsection #f))
   (let ((body (the-body opts)))
      (new container
	 (markup 'toc)
	 (ident (or ident (symbol->string (gensym "toc"))))
	 (class class)
         (loc   &invocation-location)
	 (required-options '())
	 (options `((:chapter ,chapter)
		    (:section ,section)
		    (:subsection ,subsection)
		    (:subsubsection ,subsubsection)
		    ,@(the-options opts :ident :class)))
	 (body (cond
		  ((null? body)
		   (new unresolved
                      (loc   &invocation-location)
		      (proc (lambda (n e env)
			       (handle
				(resolve-search-parent n env document?))))))
		  ((null? (cdr body))
		   (if (handle? (car body))
		       (car body)
		       (skribe-error 'toc
				     "Illegal argument (handle expected)"
				     (if (markup? (car body))
					 (markup-markup (car body))
					 "???"))))
		  (else
		   (skribe-error 'toc "Illegal argument" body)))))))

;*---------------------------------------------------------------------*/
;*    section-number ...                                               */
;*---------------------------------------------------------------------*/
(define (section-number number markup)
  (cond ((not number)
         ;; number-less
         #f)
        ((or (string? number) (list? number) (ast? number))
         ;; user-specified number
         number)
        (else
         ;; automatic numbering
         (new unresolved
              (proc (lambda (n e env)
                      (resolve-counter n env markup number)))))))

;*---------------------------------------------------------------------*/
;*    chapter ... ...                                                  */
;*    -------------------------------------------------------------    */
;*    doc:                                                             */
;*       @ref ../../doc/user/sectioning.skb:chapter@                   */
;*    writer:                                                          */
;*       html: @ref ../../skr/html.skr:chapter@                        */
;*---------------------------------------------------------------------*/
(define-markup (chapter :rest
			opts
			:key
			(ident #f) (class "chapter")
			title (html-title #f) (file #f) (toc #t) (number #t))
   (new container
      (markup 'chapter)
      (ident (or ident (symbol->string (gensym "chapter"))))
      (class class)
      (loc   &invocation-location)
      (required-options '(:title :file :toc :number))
      (options `((:toc ,toc)
		 (:number ,(section-number number 'chapter))
		 ,@(the-options opts :ident :class)))
      (body (the-body opts))
      (env (list (list 'section-counter 0) (list 'section-env '())
		 (list 'footnote-counter 0) (list 'footnote-env '())
                 (list 'equation-counter 0) (list 'equation-env '())))))

;*---------------------------------------------------------------------*/
;*    section ...                                                      */
;*    -------------------------------------------------------------    */
;*    doc:                                                             */
;*       @ref ../../doc/user/sectioning.skb:section@                   */
;*    writer:                                                          */
;*       html: @ref ../../skr/html.skr:sectionr@                       */
;*---------------------------------------------------------------------*/
(define-markup (section :rest
			opts
			:key
			(ident #f) (class "section")
			title (file #f) (toc #t) (number #t))
   (new container
      (markup 'section)
      (ident (or ident (symbol->string (gensym "section"))))
      (class class)
      (loc   &invocation-location)
      (required-options '(:title :toc :file :toc :number))
      (options `((:number ,(section-number number 'section))
		 (:toc ,toc)
		 ,@(the-options opts :ident :class)))
      (body (the-body opts))
      (env (if file
	       (list (list 'subsection-counter 0) (list 'subsection-env '())
		     (list 'footnote-counter 0) (list 'footnote-env '()))
	       (list (list 'subsection-counter 0) (list 'subsection-env '()))))))

;*---------------------------------------------------------------------*/
;*    subsection ...                                                   */
;*    -------------------------------------------------------------    */
;*    doc:                                                             */
;*       @ref ../../doc/user/sectioning.skb:subsection@                */
;*    writer:                                                          */
;*       html: @ref ../../skr/html.skr:subsectionr@                    */
;*---------------------------------------------------------------------*/
(define-markup (subsection :rest
			   opts
			   :key
			   (ident #f) (class "subsection")
			   title (file #f) (toc #t) (number #t))
   (new container
      (markup 'subsection)
      (ident (or ident (symbol->string (gensym "subsection"))))
      (class class)
      (loc   &invocation-location)
      (required-options '(:title :toc :file :number))
      (options `((:number ,(section-number number 'subsection))
		 (:toc ,toc)
		 ,@(the-options opts :ident :class)))
      (body (the-body opts))
      (env (list (list 'subsubsection-counter 0) (list 'subsubsection-env '())))))

;*---------------------------------------------------------------------*/
;*    subsubsection ...                                                */
;*    -------------------------------------------------------------    */
;*    doc:                                                             */
;*       @ref ../../doc/user/sectioning.skb:subsubsection@             */
;*    writer:                                                          */
;*       html: @ref ../../skr/html.skr:subsubsectionr@                 */
;*---------------------------------------------------------------------*/
(define-markup (subsubsection :rest
			      opts
			      :key
			      (ident #f) (class "subsubsection")
			      title (file #f) (toc #f) (number #t))
   (new container
      (markup 'subsubsection)
      (ident (or ident (symbol->string (gensym "subsubsection"))))
      (class class)
      (loc   &invocation-location)
      (required-options '(:title :toc :number :file))
      (options `((:number ,(section-number number 'subsubsection))
		 (:toc ,toc)
		 ,@(the-options opts :ident :class)))
      (body (the-body opts))))

;*---------------------------------------------------------------------*/
;*    paragraph ...                                                    */
;*---------------------------------------------------------------------*/
(define-simple-markup paragraph)


;*---------------------------------------------------------------------*/
;*    ~ (unbreakable space) ...                                        */
;*---------------------------------------------------------------------*/
(define-markup (~ :rest opts :key (class #f))
  (new markup
     (markup '~)
     (ident (symbol->string (gensym "~")))
     (class class)
     (loc   &invocation-location)
     (required-options '())
     (options (the-options opts :class))
     (body (the-body opts))))

;*---------------------------------------------------------------------*/
;*    breakable-space ...                                              */
;*---------------------------------------------------------------------*/
(define-simple-markup breakable-space)

;*---------------------------------------------------------------------*/
;*    footnote ...                                                     */
;*---------------------------------------------------------------------*/
(define-markup (footnote :rest opts
			 :key (ident #f) (class "footnote") (label #t))
   ;; The `:label' option used to be called `:number'.
   (new container
      (markup 'footnote)
      (ident (symbol->string (gensym "footnote")))
      (class class)
      (loc   &invocation-location)
      (required-options '())
      (options `((:label
		  ,(cond ((string? label) label)
			 ((number? label) label)
			 ((not label)     label)
			 (else
			  (new unresolved
			       (proc (lambda (n e env)
					(resolve-counter n env
							 'footnote #t))))))
		  ,@(the-options opts :ident :class))))
      (body (the-body opts))))

;*---------------------------------------------------------------------*/
;*    linebreak ...                                                    */
;*---------------------------------------------------------------------*/
(define-markup (linebreak :rest opts :key (ident #f) (class #f))
   (let ((ln (new markup
		(ident (or ident (symbol->string (gensym "linebreak"))))
		(class class)
                (loc   &invocation-location)
		(markup 'linebreak)))
	 (num (the-body opts)))
      (cond
	 ((null? num)
	  ln)
	 ((not (null? (cdr num)))
	  (skribe-error 'linebreak "Illegal arguments" num))
	 ((not (and (integer? (car num)) (positive? (car num))))
	  (skribe-error 'linebreak "Illegal argument" (car num)))
	 (else
	  (vector->list (make-vector (car num) ln))))))

;*---------------------------------------------------------------------*/
;*    hrule ...                                                        */
;*---------------------------------------------------------------------*/
(define-markup (hrule :rest
		      opts
		      :key
		      (ident #f) (class #f)
		      (width 100.) (height 1))
   (new markup
      (markup 'hrule)
      (ident (or ident (symbol->string (gensym "hrule"))))
      (class class)
      (loc   &invocation-location)
      (required-options '())
      (options `((:width ,width)
		 (:height ,height)
		 ,@(the-options opts :ident :class)))
      (body #f)))

;*---------------------------------------------------------------------*/
;*    color ...                                                        */
;*---------------------------------------------------------------------*/
(define-markup (color :rest
		      opts
		      :key
		      (ident #f) (class "color")
		      (bg #f) (fg #f) (width #f) (margin #f))
   (new container
      (markup 'color)
      (ident (or ident (symbol->string (gensym "color"))))
      (class class)
      (loc   &invocation-location)
      (required-options '(:bg :fg :width))
      (options `((:bg ,bg)
		 (:fg ,fg)
		 ,@(the-options opts :ident :class :bg :fg)))
      (body (the-body opts))))

;*---------------------------------------------------------------------*/
;*    frame ...                                                        */
;*---------------------------------------------------------------------*/
(define-markup (frame :rest
		      opts
		      :key
		      (ident #f) (class "frame")
		      (width #f) (margin 2) (border 1))
   (new container
      (markup 'frame)
      (ident (or ident (symbol->string (gensym "frame"))))
      (class class)
      (loc   &invocation-location)
      (required-options '(:width :border :margin))
      (options `((:margin ,margin)
		 (:border ,(cond
			      ((integer? border) border)
			      (border 1)
			      (else #f)))
		 ,@(the-options opts :ident :class)))
      (body (the-body opts))))

;*---------------------------------------------------------------------*/
;*    font ...                                                         */
;*---------------------------------------------------------------------*/
(define-markup (font :rest
		     opts
		     :key
		     (ident #f) (class #f)
		     (size #f) (face #f))
   (new container
      (markup 'font)
      (ident (or ident (symbol->string (gensym "font"))))
      (class class)
      (loc   &invocation-location)
      (required-options '(:size))
      (options (the-options opts :ident :class))
      (body (the-body opts))))

;*---------------------------------------------------------------------*/
;*    flush ...                                                        */
;*---------------------------------------------------------------------*/
(define-markup (flush :rest
		      opts
		      :key
		      (ident #f) (class #f)
		      side)
   (case side
      ((center left right)
       (new container
	  (markup 'flush)
	  (ident (or ident (symbol->string (gensym "flush"))))
	  (class class)
          (loc   &invocation-location)
	  (required-options '(:side))
	  (options (the-options opts :ident :class))
	  (body (the-body opts))))
      (else
       (skribe-error 'flush "Illegal side" side))))

;*---------------------------------------------------------------------*/
;*    center ...                                                       */
;*---------------------------------------------------------------------*/
(define-simple-container center)

;*---------------------------------------------------------------------*/
;*    pre ...                                                          */
;*---------------------------------------------------------------------*/
(define-simple-container pre)

;*---------------------------------------------------------------------*/
;*    prog ...                                                         */
;*    -------------------------------------------------------------    */
;*    doc:                                                             */
;*       @ref ../../doc/user/prgm.skb:prog@                            */
;*    writer:                                                          */
;*       html: @ref ../../skr/html.skr:prog@                           */
;*---------------------------------------------------------------------*/
(define-markup (prog :rest
		     opts
		     :key
		     (ident #f) (class "prog")
		     (line 1) (linedigit #f) (mark ";!"))
   (if (not (or (string? mark) (eq? mark #f)))
       (skribe-error 'prog "Illegal mark" mark)
       (new container
	  (markup 'prog)
	  (ident (or ident (symbol->string (gensym "prog"))))
	  (class class)
          (loc   &invocation-location)
	  (required-options '(:line :mark))
	  (options (the-options opts :ident :class :linedigit))
	  (body (make-prog-body (the-body opts) line linedigit mark)))))

;*---------------------------------------------------------------------*/
;*    source ...                                                       */
;*    -------------------------------------------------------------    */
;*    doc:                                                             */
;*       @ref ../../doc/user/prgm.skb:source@                          */
;*    writer:                                                          */
;*       html: @ref ../../skr/html.skr:source@                         */
;*---------------------------------------------------------------------*/
(define-markup (source :rest
		       opts
		       :key
		       language
		       (file #f) (start #f) (stop #f)
		       (definition #f) (tab 8))
   (let ((body (the-body opts)))
      (cond
	 ((and (not (null? body)) (or file start stop definition))
	  (skribe-error 'source
			"file, start/stop, and definition are exclusive with body"
			body))
	 ((and start stop definition)
	  (skribe-error 'source
			"start/stop are exclusive with a definition"
			body))
	 ((and (or start stop definition) (not file))
	  (skribe-error 'source
			"start/stop and definition require a file specification"
			file))
	 ((and definition (not language))
	  (skribe-error 'source
			"definition requires a language specification"
			definition))
	 ((and file (not (string? file)))
	  (skribe-error 'source "Illegal file" file))
	 ((and start (not (or (integer? start) (string? start))))
	  (skribe-error 'source "Illegal start" start))
	 ((and stop (not (or (integer? stop) (string? stop))))
	  (skribe-error 'source "Illegal start" stop))
	 ((and (integer? start) (integer? stop) (> start stop))
	  (skribe-error 'source
			"start line > stop line"
			(format #f "~a/~a" start stop)))
	 ((and language (not (language? language)))
	  (skribe-error 'source "illegal language" language))
	 ((and tab (not (integer? tab)))
	  (skribe-error 'source "illegal tab" tab))
	 (file
	  (let ((s (if (not definition)
		       (source-read-lines file start stop tab)
		       (source-read-definition file definition tab language))))
	     (if language
		 (source-fontify s language)
		 s)))
	 (language
	  (source-fontify body language))
	 (else
	  body))))

;*---------------------------------------------------------------------*/
;*    language ...                                                     */
;*    -------------------------------------------------------------    */
;*    doc:                                                             */
;*       @ref ../../doc/user/prgm.skb:language@                        */
;*---------------------------------------------------------------------*/
(define-markup (language :key name (fontifier #f) (extractor #f))
   (if (not (string? name))
       (skribe-type-error 'language "illegal name" name "string")
       (new language
	  (name name)
	  (fontifier fontifier)
	  (extractor extractor))))

;*---------------------------------------------------------------------*/
;*    figure ...                                                       */
;*    -------------------------------------------------------------    */
;*    doc:                                                             */
;*       @ref ../../doc/user/figure.skb:figure@                        */
;*    writer:                                                          */
;*       html: @ref ../../skr/html.skr:figure@                         */
;*---------------------------------------------------------------------*/
(define-markup (figure :rest
		       opts
		       :key
		       (ident #f) (class "figure")
		       (legend #f) (number #t) (multicolumns #f))
   (new container
      (markup 'figure)
      (ident (or ident
		 (let ((s (ast->string legend)))
		    (if (not (string=? s ""))
			s
			(symbol->string (gensym "figure"))))))
      (class class)
      (loc   &invocation-location)
      (required-options '(:legend :number :multicolumns))
      (options `((:number
		  ,(new unresolved
		      (proc (lambda (n e env)
			       (resolve-counter n env 'figure number)))))
		 ,@(the-options opts :ident :class)))
      (body (the-body opts))))

;*---------------------------------------------------------------------*/
;*    parse-list-of ...                                                */
;*    -------------------------------------------------------------    */
;*    The function table accepts two different prototypes. It          */
;*    may receive its N elements in a list of N elements or in         */
;*    a list of one element which is a list of N elements. This        */
;*    gets rid of APPLY when calling container markup such as ITEMIZE  */
;*    or TABLE.                                                        */
;*---------------------------------------------------------------------*/
(define (parse-list-of for markup lst)
   (cond
      ((null? lst)
       '())
      ((and (pair? lst)
	    (or (pair? (car lst)) (null? (car lst)))
	    (null? (cdr lst)))
       (parse-list-of for markup (car lst)))
      (else
       (let loop ((lst lst)
		  (result '()))
	  (cond
	     ((null? lst)
	      (reverse! result))
	     ((pair? (car lst))
	      (loop (car lst) result))
	     (else
	      (let ((r (car lst)))
		 (if (not (is-markup? r markup))
		     (skribe-warning 2
				     for
				     (format #f "illegal `~a' element, `~a' expected"
					     (if (markup? r)
						 (markup-markup r)
						 (type-name r))
					     markup)))
		 (loop (cdr lst) (cons r result)))))))))

;*---------------------------------------------------------------------*/
;*    itemize ...                                                      */
;*---------------------------------------------------------------------*/
(define-markup (itemize :rest opts :key (ident #f) (class "itemize") symbol)
   (new container
      (markup 'itemize)
      (ident (or ident (symbol->string (gensym "itemize"))))
      (class class)
      (loc   &invocation-location)
      (required-options '(:symbol))
      (options `((:symbol ,symbol) ,@(the-options opts :ident :class)))
      (body (parse-list-of 'itemize 'item (the-body opts)))))

;*---------------------------------------------------------------------*/
;*    enumerate ...                                                    */
;*---------------------------------------------------------------------*/
(define-markup (enumerate :rest opts :key (ident #f) (class "enumerate") symbol)
   (new container
      (markup 'enumerate)
      (ident (or ident (symbol->string (gensym "enumerate"))))
      (class class)
      (loc   &invocation-location)
      (required-options '(:symbol))
      (options `((:symbol ,symbol) ,@(the-options opts :ident :class)))
      (body (parse-list-of 'enumerate 'item (the-body opts)))))

;*---------------------------------------------------------------------*/
;*    description ...                                                  */
;*---------------------------------------------------------------------*/
(define-markup (description :rest opts :key (ident #f) (class "description") symbol)
   (new container
      (markup 'description)
      (ident (or ident (symbol->string (gensym "description"))))
      (class class)
      (loc   &invocation-location)
      (required-options '(:symbol))
      (options `((:symbol ,symbol) ,@(the-options opts :ident :class)))
      (body (parse-list-of 'description 'item (the-body opts)))))

;*---------------------------------------------------------------------*/
;*    item ...                                                         */
;*---------------------------------------------------------------------*/
(define-markup (item :rest opts :key (ident #f) (class #f) key)
   (if (and key (not (or (string? key)
			 (number? key)
			 (markup? key)
			 (pair? key))))
       (skribe-type-error 'item "Illegal key:" key "node")
       (new container
	  (markup 'item)
	  (ident (or ident (symbol->string (gensym "item"))))
	  (class class)
          (loc   &invocation-location)
	  (required-options '(:key))
	  (options `((:key ,key) ,@(the-options opts :ident :class :key)))
	  (body (the-body opts)))))

;*---------------------------------------------------------------------*/
;*    table                                                            */
;*---------------------------------------------------------------------*/
(define-markup (table :rest
		      opts
		      :key
		      (ident #f) (class #f) (&location #f)
		      (border #f) (width #f)
		      (frame 'none) (rules 'none)
		      (cellstyle 'collapse) (cellpadding #f) (cellspacing #f))
   (let ((frame (cond
		   ((string? frame)
		    (string->symbol frame))
		   ((not frame)
		    #f)
		   (else
		    frame)))
	 (rules (cond
		   ((string? rules)
		    (string->symbol rules))
		   ((not rules)
		    #f)
		   (else
		    rules)))
	 (frame-vals '(none above below hsides vsides lhs rhs box border))
	 (rules-vals '(none rows cols all header))
	 (cells-vals '(collapse separate)))
      (cond
	 ((and frame (not (memq frame frame-vals)))
	  (skribe-error 'table
			(format #f "frame should be one of \"~a\"" frame-vals)
			frame))
	 ((and rules (not (memq rules rules-vals)))
	  (skribe-error 'table
			(format #f "rules should be one of \"~a\"" rules-vals)
			rules))
	 ((not (or (memq cellstyle cells-vals)
		   (string? cellstyle)
		   (number? cellstyle)))
	  (skribe-error 'table
			(format #f "cellstyle should be one of \"~a\", or a number, or a string" cells-vals)
			cellstyle))
	 (else
	  (new container
	     (markup 'table)
	     (ident (or ident (symbol->string (gensym "table"))))
	     (class class)
             (loc   (or &location &invocation-location))
	     (required-options '(:width :frame :rules))
	     (options `((:frame ,frame)
			(:rules ,rules)
			(:cellstyle ,cellstyle)
			,@(the-options opts :ident :class)))
	     (body (parse-list-of 'table 'tr (the-body opts))))))))

;*---------------------------------------------------------------------*/
;*    tr ...                                                           */
;*---------------------------------------------------------------------*/
(define-markup (tr :rest opts :key (ident #f) (class #f) (bg #f))
   (new container
      (markup 'tr)
      (ident (or ident (symbol->string (gensym "tr"))))
      (class class)
      (loc   &invocation-location)
      (required-options '())
      (options `(,@(if bg `((:bg ,bg)) '())
		 ,@(the-options opts :ident :class :bg)))
      (body (parse-list-of 'tr 'tc (the-body opts)))))

;*---------------------------------------------------------------------*/
;*    tc...                                                            */
;*---------------------------------------------------------------------*/
(define-markup (tc m
		   :rest
		   opts
		   :key
		   (ident #f) (class #f)
		   (width #f) (align 'center) (valign #f)
		   (colspan 1) (rowspan 1) (bg #f))
   (let ((align (if (string? align)
		    (string->symbol align)
		    align))
	 (valign (if (string? valign)
		     (string->symbol valign)
		     valign)))
      (cond
	 ((not (integer? colspan))
	  (skribe-type-error 'tc "Illegal colspan, " colspan "integer"))
	 ((not (symbol? align))
	  (skribe-type-error 'tc "Illegal align, " align "align"))
	 ((not (memq align '(#f center left right)))
	  (skribe-error
	   'tc
	   "align should be one of 'left', `center', or `right'"
	   align))
	 ((not (memq valign '(#f top middle center bottom)))
	  (skribe-error
	   'tc
	   "valign should be one of 'top', `middle', `center', or `bottom'"
	   valign))
	 (else
	  (new container
	     (markup 'tc)
	     (ident (or ident (symbol->string (gensym "tc"))))
	     (class class)
             (loc   &invocation-location)
	     (required-options '(:width :align :valign :colspan))
	     (options `((markup ,m)
			(:align ,align)
			(:valign ,valign)
			(:colspan ,colspan)
			,@(if bg
			      `((:bg ,bg))
			      '())
			,@(the-options opts :ident :class :bg :align :valign)))
	     (body (the-body opts)))))))

;*---------------------------------------------------------------------*/
;*    th ...                                                           */
;*---------------------------------------------------------------------*/
(define-markup (th :rest
		   opts
		   :key
		   (ident #f) (class #f)
		   (width #f) (align 'center) (valign #f)
		   (colspan 1) (rowspan 1) (bg #f))
   (apply tc 'th opts))

;*---------------------------------------------------------------------*/
;*    td ...                                                           */
;*---------------------------------------------------------------------*/
(define-markup (td :rest
		   opts
		   :key
		   (ident #f) (class #f)
		   (width #f) (align 'center) (valign #f)
		   (colspan 1) (rowspan 1) (bg #f))
   (apply tc 'td opts))

;*---------------------------------------------------------------------*/
;*    image ...                                                        */
;*    -------------------------------------------------------------    */
;*    doc:                                                             */
;*       @ref ../../doc/user/image.skb:image@                          */
;*    writer:                                                          */
;*       html: @ref ../../skr/html.skr:image@                          */
;*       latex: @ref ../../skr/latex.skr:image@                        */
;*---------------------------------------------------------------------*/
(define-markup (image :rest
		      opts
		      :key
		      (ident #f) (class #f)
		      file (url #f) (width #f) (height #f) (zoom #f))
   (cond
      ((not (or (string? file) (string? url)))
       (skribe-error 'image "No file or url provided" file))
      ((and (string? file) (string? url))
       (skribe-error 'image "Both file and url provided" (list file url)))
      (else
       (new markup
	  (markup 'image)
	  (ident (or ident (symbol->string (gensym "image"))))
	  (class class)
          (loc   &invocation-location)
	  (required-options '(:file :url :width :height))
	  (options (the-options opts :ident :class))
	  (body (the-body opts))))))

;*---------------------------------------------------------------------*/
;*    blockquote                                                       */
;*---------------------------------------------------------------------*/
(define-simple-markup blockquote)

;*---------------------------------------------------------------------*/
;*    Ornaments ...                                                    */
;*---------------------------------------------------------------------*/
(define-simple-markup roman)
(define-simple-markup bold)
(define-simple-markup underline)
(define-simple-markup strike)
(define-simple-markup emph)
(define-simple-markup kbd)
(define-simple-markup it)
(define-simple-markup tt)
(define-simple-markup code)
(define-simple-markup var)
(define-simple-markup samp)
(define-simple-markup sf)
(define-simple-markup sc)
(define-simple-markup sub)
(define-simple-markup sup)

;*---------------------------------------------------------------------*/
;*    char ...                                                         */
;*---------------------------------------------------------------------*/
(define-public (char char)
   (cond
      ((char? char)
       (string char))
      ((integer? char)
       (string (integer->char char)))
      ((and (string? char) (= (string-length char) 1))
       char)
      (else
       (skribe-error 'char "Illegal char" char))))

;*---------------------------------------------------------------------*/
;*    symbol ...                                                       */
;*---------------------------------------------------------------------*/
(define-markup (symbol symbol)
  (let ((v (cond
	    ((symbol? symbol)
	     (symbol->string symbol))
	    ((string? symbol)
	     symbol)
	    (else
	     (skribe-error 'symbol
			   "Illegal argument (symbol expected)"
			   symbol)))))
    (new markup
	 (markup 'symbol)
         (loc    &invocation-location)
	 (body v))))

;*---------------------------------------------------------------------*/
;*    ! ...                                                            */
;*---------------------------------------------------------------------*/
(define-markup (! format :rest node)
   (if (not (string? format))
       (skribe-type-error '! "Illegal format:" format "string")
       (new command
          (loc &invocation-location)
	  (fmt format)
	  (body node))))

;*---------------------------------------------------------------------*/
;*    processor ...                                                    */
;*---------------------------------------------------------------------*/
(define-markup (processor :rest opts
			  :key (combinator #f) (engine #f) (procedure #f))
   (cond
      ((and combinator (not (procedure? combinator)))
       (skribe-error 'processor "Combinator not a procedure" combinator))
      ((and engine (not (engine? engine)))
       (skribe-error 'processor "Illegal engine" engine))
      ((and procedure
	    (or (not (procedure? procedure))
		(not (let ((a (procedure-property procedure 'arity)))
                       (and (pair? a)
                            (let ((compulsory (car a))
                                  (optional   (cadr a))
                                  (rest?      (caddr a)))
                              (or rest?
                                  (>= (+ compulsory optional) 2))))))))
       (skribe-error 'processor "Illegal procedure" procedure))
      (else
       (new processor
          (loc &invocation-location)
	  (combinator combinator)
	  (engine engine)
	  (procedure (or procedure (lambda (n e) n)))
	  (body (the-body opts))))))

;*---------------------------------------------------------------------*/
;*    Processors ...                                                   */
;*---------------------------------------------------------------------*/
(define-processor-markup html-processor)
(define-processor-markup tex-processor)

;*---------------------------------------------------------------------*/
;*    handle ...                                                       */
;*---------------------------------------------------------------------*/
(define-markup (handle :rest opts
		       :key (ident #f) (class "handle") value section)
   (let ((body (the-body opts)))
      (cond
	 (section
	  (error 'handle "Illegal handle `section' option" section)
	  (new unresolved
             (loc  &invocation-location)
	     (proc (lambda (n e env)
		      (let ((s (resolve-ident section 'section n env)))
			 (new handle
                            (loc   &invocation-location)
			    (ast s)))))))
	 ((and (pair? body)
	       (null? (cdr body))
	       (markup? (car body)))
	  (new handle
             (loc &invocation-location)
	     (ast (car body))))
	 (else
	  (skribe-error 'handle "Illegal handle" opts)))))

;*---------------------------------------------------------------------*/
;*    mailto ...                                                       */
;*    -------------------------------------------------------------    */
;*    doc:                                                             */
;*       @ref ../../doc/user/links.skb:mailto@                         */
;*    writer:                                                          */
;*       html: @ref ../../skr/html.skr:mailto@                         */
;*---------------------------------------------------------------------*/
(define-markup (mailto :rest opts :key (ident #f) (class "mailto") text)
   (new markup
      (markup 'mailto)
      (ident (or ident (symbol->string (gensym "ident"))))
      (class class)
      (loc   &invocation-location)
      (required-options '(:text))
      (options (the-options opts :ident :class))
      (body (the-body opts))))

;*---------------------------------------------------------------------*/
;*    mark ...                                                         */
;*    -------------------------------------------------------------    */
;*    doc:                                                             */
;*       @ref ../../doc/user/links.skb:mark@                           */
;*    writer:                                                          */
;*       html: @ref ../../skr/html.skr:mark@                           */
;*---------------------------------------------------------------------*/
(define-markup (mark :rest opts :key (ident #f) (class "mark") (text #f))
   (let ((bd (the-body opts)))
      (cond
	 ((and (pair? bd) (not (null? (cdr bd))))
	  (skribe-error 'mark "Too many argument provided" bd))
	 ((null? bd)
	  (skribe-error 'mark "Missing argument" '()))
	 ((not (string? (car bd)))
	  (skribe-type-error 'mark "Illegal ident:" (car bd) "string"))
	 (ident
	  (skribe-error 'mark "Illegal `ident:' option" ident))
	 (else
	  (let* ((bs (ast->string bd))
		 (n (new markup
		       (markup 'mark)
		       (ident bs)
		       (class class)
                       (loc   &invocation-location)
		       (options (the-options opts :ident :class :text))
		       (body text))))
	     n)))))

;*---------------------------------------------------------------------*/
;*    ref ...                                                          */
;*    -------------------------------------------------------------    */
;*    doc:                                                             */
;*       @ref ../../doc/user/links.skb:ref@                            */
;*    writer:                                                          */
;*       html: @ref ../../skr/html.skr:ref@                            */
;*       latex: @ref ../../skr/latex.skr:ref@                          */
;*---------------------------------------------------------------------*/
(define-markup (ref :rest
		    opts
		    :key
		    (class #f)
		    (ident #f)
		    (text #f)
		    (chapter #f)
		    (section #f)
		    (subsection #f)
		    (subsubsection #f)
		    (bib #f)
		    (bib-table (*bib-table*))
		    (url #f)
		    (figure #f)
		    (mark #f)
		    (handle #f)
		    (line #f)
		    (skribe #f)
		    (page #f)
                    (sort-bib-refs bib-sort-refs/number))
   (define (unref ast text kind)
      (let ((msg (format #f "can't find `~a': " kind)))
	 (if (ast? ast)
	     (begin
		(skribe-warning/ast 1 ast 'ref msg text)
		(new markup
		   (markup 'unref)
		   (ident (symbol->string (gensym "unref")))
		   (class class)
                   (loc   &invocation-location)
		   (required-options '(:text))
		   (options `((kind ,kind) ,@(the-options opts :ident :class)))
		   (body (list text ": " (ast->file-location ast)))))
	     (begin
		(skribe-warning 1 'ref msg text)
		(new markup
		   (markup 'unref)
		   (ident (symbol->string (gensym "unref")))
		   (class class)
                   (loc   &invocation-location)
		   (required-options '(:text))
		   (options `((kind ,kind) ,@(the-options opts :ident :class)))
		   (body text))))))
   (define (skribe-ref skribe)
      (let ((path (search-path (*document-path*) skribe)))
	 (if (not path)
	     (unref #f skribe 'sui-file)
	     (let* ((sui (load-sui path))
		    (os (the-options opts :skribe :class :text))
		    (u (sui-ref->url (dirname path) sui ident os)))
		(if (not u)
		    (unref #f os 'sui-ref)
		    (ref :url u :text text :ident ident :class class))))))
   (define (handle-ref text)
      (new markup
	 (markup 'ref)
	 (ident (symbol->string (gensym "handle-ref")))
	 (class class)
         (loc   &invocation-location)
	 (required-options '(:text))
	 (options `((kind handle) ,@(the-options opts :ident :class)))
	 (body text)))
   (define (do-title-ref title kind)
      (if (not (string? title))
	  (skribe-type-error 'ref "illegal reference" title "string")
	  (new unresolved
             (loc  &invocation-location)
	     (proc (lambda (n e env)
		      (let* ((doc (ast-document n))
                             (s (find1-down
                                 (lambda (n)
                                   (and (is-markup? n kind)
                                        (equal? (markup-option n :title)
                                                title)))
                                 doc)))
			 (if s
			     (new markup
				(markup 'ref)
				(ident (symbol->string (gensym "title-ref")))
				(class class)
                                (loc   &invocation-location)
				(required-options '(:text))
				(options `((kind ,kind)
					   (mark ,title)
					   ,@(the-options opts :ident :class)))
				(body (new handle
					 (ast s))))
			     (unref n title (or kind 'title)))))))))
   (define (do-ident-ref text kind)
      (if (not (string? text))
	  (skribe-type-error 'ref "Illegal reference" text "string")
	  (new unresolved
             (loc  &invocation-location)
	     (proc (lambda (n e env)
		      (let ((s (resolve-ident text kind n env)))
			 (if s
			     (new markup
				(markup 'ref)
				(ident (symbol->string (gensym "ident-ref")))
				(class class)
                                (loc   &invocation-location)
				(required-options '(:text))
				(options `((kind ,kind)
					   (mark ,text)
					   ,@(the-options opts :ident :class)))
				(body (new handle
					 (ast s))))
			     (unref n text (or kind 'ident)))))))))
   (define (mark-ref mark)
     (do-ident-ref mark 'mark))
   (define (make-bib-ref v)
      (let ((s (resolve-bib bib-table v)))
	 (if s
	     (let* ((n (new markup
			  (markup 'bib-ref)
			  (ident (symbol->string (gensym "bib-ref")))
			  (class class)
                          (loc   &invocation-location)
			  (required-options '(:text))
			  (options (the-options opts :ident :class))
			  (body (new handle
				   (ast s)))))
		    (h (new handle (ast n)))
		    (o (markup-option s 'used)))
		(markup-option-add! s 'used (if (pair? o) (cons h o) (list h)))
		n)
	     (unref #f v 'bib)))) ; FIXME: This prevents source location info
				  ; from being provided in the warning msg
   (define (bib-ref text)
      (if (pair? text)
	  (new markup
	     (markup 'bib-ref+)
	     (ident (symbol->string (gensym "bib-ref+")))
	     (class class)
             (loc   &invocation-location)
	     (options `((:sort-bib-refs ,sort-bib-refs)
                        ,@(the-options opts :ident :class)))
	     (body (map make-bib-ref text)))
	  (make-bib-ref text)))
   (define (url-ref)
      (new markup
	 (markup 'url-ref)
	 (ident (symbol->string (gensym "url-ref")))
	 (class class)
         (loc   &invocation-location)
	 (required-options '(:url :text))
	 (options (the-options opts :ident :class))))
   (define (line-ref line)
      (new unresolved
         (loc   &invocation-location)
	 (proc  (lambda (n e env)
		   (let ((l (resolve-line (ast-document n) line)))
		      (if l
			  (new markup
			     (markup 'line-ref)
			     (ident (symbol->string (gensym "line-ref")))
			     (class class)
                             (loc   &invocation-location)
			     (options `((:text ,(markup-ident l))
					,@(the-options opts :ident :class)))
			     (body (new handle
				      (ast l))))
			  (unref n line 'line)))))))
   (let ((b (the-body opts)))
      (if (not (null? b))
	  (skribe-warning 1 'ref "Arguments ignored " b))
      (cond
	 (skribe (skribe-ref skribe))
	 (handle (handle-ref handle))
	 (ident (do-ident-ref ident #f))
	 (chapter (do-title-ref chapter 'chapter))
	 (section (do-title-ref section 'section))
	 (subsection (do-title-ref subsection 'subsection))
	 (subsubsection (do-title-ref subsubsection 'subsubsection))
	 (figure (do-ident-ref figure 'figure))
	 (mark (mark-ref mark))
	 (bib (bib-ref bib))
	 (url (url-ref))
	 (line (line-ref line))
	 (else (skribe-error 'ref "illegal reference" opts)))))


;*---------------------------------------------------------------------*/
;*    numref ...                                                       */
;*---------------------------------------------------------------------*/
(define-markup (numref :rest opts
                       :key (ident #f) (text "") (page #f)
                             (separator ".") (class #f))
  ;; Produce a numbered reference to `ident'.
  (new unresolved
       (loc  &invocation-location)
       (proc (lambda (n e env)
	       (let* ((doc (ast-document n))
		      (target (document-lookup-node doc ident))
		      (number (and target
                                   (markup-option target :number))))
                 (cond
                  ((not target)
                   (skribe-warning/ast 1 n 'numref
                                       "can't find `ident': "
                                       ident)
                   (new markup
                        (markup 'unref)
                        (ident (symbol->string (gensym "unref")))
                        (class class)
                        (loc   &invocation-location)
                        (required-options '(:text))
                        (options `((kind numref)
                                   ,@(the-options opts :ident :class)))
                        (body (list ident ": " (ast->file-location n)))))
                  ((unresolved? number)
                   ;; Loop until `number' is resolved.
                   n)
                  (else
                   (let ((xref
                          (ref :text
                               (list (if text text "") " "
                                     (if (number? number)
                                         (markup-number-string target
                                                               separator)
                                         ""))
                               :page page
                               :handle (handle target))))
                     (resolve! xref e env)))))))))

;*---------------------------------------------------------------------*/
;*    resolve ...                                                      */
;*---------------------------------------------------------------------*/
(define-markup (resolve fun)
   (new unresolved
      (loc  &invocation-location)
      (proc fun)))

;*---------------------------------------------------------------------*/
;*    bibliography ...                                                 */
;*    -------------------------------------------------------------    */
;*    doc:                                                             */
;*       @ref ../../doc/user/bib.skb:bibliography@                     */
;*---------------------------------------------------------------------*/
(define-markup (bibliography :rest files
			     :key
			     (command #f) (bib-table (*bib-table*)))
   (for-each (lambda (f)
		(cond
		   ((string? f)
		    (bib-load! bib-table f command))
		   ((pair? f)
		    (bib-add! bib-table f))
		   (else
		    (skribe-error "bibliography" "Illegal entry" f))))
	     (the-body files)))

;*---------------------------------------------------------------------*/
;*    the-bibliography ...                                             */
;*    -------------------------------------------------------------    */
;*    doc:                                                             */
;*       @ref ../../doc/user/bib.skb:the-bibliography@                 */
;*    writer:                                                          */
;*       base: @ref ../../skr/base.skr:the-bibliography@               */
;*---------------------------------------------------------------------*/
(define-markup (the-bibliography :rest opts
				 :key
				 pred
				 (bib-table (*bib-table*))
				 (sort bib-sort/authors)
				 (count 'partial)
                                 (labels 'number))
   (if (not (memq count '(partial full)))
       (skribe-error 'the-bibliography
		     "count must be either `partial' or `full'"
		     count)
       (let ((label-proc (case labels
                           ((number)    assign-entries-numbers!)
                           ((name+year) assign-entries-name+years!)
                           (else
                            (skribe-error
                             'the-bibliography
                             "invalid label type" labels)))))
         (new unresolved
            (loc  &invocation-location)
            (proc (lambda (n e env)
                     (resolve-the-bib bib-table
                                      (new handle (ast n))
                                      sort
                                      pred
                                      count
                                      (the-options opts)
                                      label-proc)))))))

;*---------------------------------------------------------------------*/
;*    noabbrev ...                                                     */
;*---------------------------------------------------------------------*/
(define-simple-markup noabbrev)

;*---------------------------------------------------------------------*/
;*    make-index ...                                                   */
;*    -------------------------------------------------------------    */
;*    doc:                                                             */
;*       @ref ../../doc/user/index.skb:make-index@                     */
;*---------------------------------------------------------------------*/
(define-public (make-index ident)
   (make-index-table ident))

;*---------------------------------------------------------------------*/
;*    index ...                                                        */
;*    -------------------------------------------------------------    */
;*    doc:                                                             */
;*       @ref ../../doc/user/index.skb:index@                          */
;*---------------------------------------------------------------------*/
(define-markup (index :rest
		      opts
		      :key
		      (ident #f) (class "index")
		      (note #f) (index #f) (shape #f)
		      (url #f))
   (let* ((entry-name (the-body opts))
	  (ename (cond
		    ((string? entry-name)
		     entry-name)
		    ((and (pair? entry-name) (every string? entry-name))
		     (string-concatenate entry-name))
		    (else
		     (skribe-error
		      'index
		      "entry-name must be either a string or a list of strings"
		      entry-name))))
	  (table (cond
		    ((not index) (default-index))
		    ((index? index) index)
		    (else (skribe-type-error 'index
					     "Illegal index table, "
					     index
					     "index"))))
	  (m (mark (symbol->string (gensym "mark"))))
	  (h (new handle (ast m)))
	  (new (new markup
		  (markup '&index-entry)
		  (ident (or ident (symbol->string (gensym "index"))))
		  (class class)
                  (loc   &invocation-location)
		  (options `((name ,ename) ,@(the-options opts :ident :class)))
		  (body (if url
			    (ref :url url :text (or shape ename))
			    (ref :handle h :text (or shape ename)))))))
      ;; New is bound to a dummy option of the mark in order
      ;; to make new options verified.
      (markup-option-add! m 'to-verify new)

      (let ((handle (hash-get-handle table ename)))
        (if (not handle)
            (hash-set! table ename (list new))
            (set-cdr! handle (cons new (cdr handle)))))

      m))

;*---------------------------------------------------------------------*/
;*    the-index ...                                                    */
;*    -------------------------------------------------------------    */
;*    doc:                                                             */
;*       @ref ../../doc/user/index.skb:the-index@                      */
;*    writer:                                                          */
;*       base: @ref ../../skr/base.skr:the-index@                      */
;*       html: @ref ../../skr/html.skr:the-index-header@               */
;*---------------------------------------------------------------------*/
(define-markup (the-index :rest
			  opts
			  :key
			  (ident #f)
			  (class "the-index")
			  (split #f)
			  (char-offset 0)
			  (header-limit 50)
			  (column 1))
   (let ((bd (the-body opts)))
      (cond
	 ((not (and (integer? char-offset) (>= char-offset 0)))
	  (skribe-error 'the-index "Illegal char offset" char-offset))
	 ((not (integer? column))
	  (skribe-error 'the-index "Illegal column number" column))
	 ((not (every index? bd))
	  (skribe-error 'the-index
			"Illegal indexes"
			(filter (lambda (o) (not (index? o))) bd)))
	 (else
	  (new unresolved
             (loc  &invocation-location)
	     (proc (lambda (n e env)
		      (resolve-the-index (ast-loc n)
					 ident class
					 bd
					 split
					 char-offset
					 header-limit
					 column))))))))


;;; This part comes from the file `skribe.skr' in the original Skribe
;;; distribution.

;*---------------------------------------------------------------------*/
;*    p ...                                                            */
;*---------------------------------------------------------------------*/
(define-markup (p :rest opt :key ident (class #f))
   (paragraph :ident ident :class class :loc &invocation-location
      (the-body opt)))

;*---------------------------------------------------------------------*/
;*    fg ...                                                           */
;*---------------------------------------------------------------------*/
(define-public (fg c . body)
   (color :fg c body))

;*---------------------------------------------------------------------*/
;*    bg ...                                                           */
;*---------------------------------------------------------------------*/
(define-public (bg c . body)
   (color :bg c body))

;*---------------------------------------------------------------------*/
;*    counter ...                                                      */
;*    -------------------------------------------------------------    */
;*    This produces a kind of "local enumeration" that is:             */
;*       (counting "toto," "tutu," "titi.")                            */
;*    produces:                                                        */
;*       i) toto, ii) tutu, iii) titi.                                 */
;*---------------------------------------------------------------------*/
(define-markup (counter :rest opts :key (numbering 'roman))
   (define items (if (eq? (car opts) :numbering) (cddr opts) opts))
   (define vroman #(- "i" "ii" "iii" "iv" "v" "vi" "vii" "viii" "ix" "x"))
   (define (the-roman-number num)
      (if (< num (vector-length vroman))
	  (list (list "(" (it (vector-ref vroman num)) ") "))
	  (skribe-error 'counter
			"too many items for roman numbering"
			(length items))))
   (define (the-arabic-number num)
      (list (list "(" (it (number->string num)) ") ")))
   (define (the-alpha-number num)
      (list (list "(" (it (+ (char->integer #\a) num -1)) ") ")))
   (let ((the-number (case numbering
			((roman) the-roman-number)
			((arabic) the-arabic-number)
			((alpha) the-alpha-number)
			(else (skribe-error 'counter
					    "Illegal numbering"
					    numbering)))))
      (let loop ((num 1)
		 (items items)
		 (res '()))
	   (if (null? items)
	       (reverse! res)
	       (loop (+ num 1)
		     (cdr items)
		     (cons (list (the-number num) (car items)) res))))))

;*---------------------------------------------------------------------*/
;*    q                                                                */
;*---------------------------------------------------------------------*/
(define-markup (q :rest opt)
   (new markup
      (markup 'q)
      (loc &invocation-location)
      (options (the-options opt))
      (body (the-body opt))))

