;;; slide.scm  --  Overhead transparencies.
;;;
;;; Copyright 2003, 2004  Manuel Serrano
;;; Copyright 2006  Ludovic Courtès <ludovic.courtes@laas.fr>
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


(define-skribe-module (skribilo package slide)
  :autoload (skribilo engine html) (html-width html-title-authors))


;*---------------------------------------------------------------------*/
;*    slide-options                                                    */
;*---------------------------------------------------------------------*/
(define-public &slide-load-options (skribe-load-options))


;*---------------------------------------------------------------------*/
;*    %slide-the-slides ...                                            */
;*---------------------------------------------------------------------*/
(define %slide-the-slides '())
(define %slide-the-counter 0)

;*---------------------------------------------------------------------*/
;*    %slide-initialize! ...                                           */
;*---------------------------------------------------------------------*/
(format (current-error-port) "Slides initializing...~%")

;; Register specific implementations for lazy loading.
(when-engine-is-loaded 'latex
  (lambda ()
    (resolve-module '(skribilo package slide latex))))
(when-engine-is-loaded 'html
  (lambda ()
    (resolve-module '(skribilo package slide html))))
(when-engine-is-loaded 'lout
  (lambda ()
    (resolve-module '(skribilo package slide lout))))


;*---------------------------------------------------------------------*/
;*    slide ...                                                        */
;*---------------------------------------------------------------------*/
(define-markup (slide #!rest opt
		      #!key
		      (ident #f) (class #f)
		      (toc #t)
		      title (number #t)
		      (vspace #f) (vfill #f)
		      (transition #f)
		      (bg #f) (image #f))
   (let ((s (new container
	       (markup 'slide)
	       (ident (if (not ident)
			  (symbol->string (gensym 'slide))
			  ident))
	       (class class)
	       (required-options '(:title :number :toc))
	       (options `((:number
			   ,(cond
			       ((number? number)
				(set! %slide-the-counter number)
				number)
			       (number
				(set! %slide-the-counter
				      (+ 1 %slide-the-counter))
				%slide-the-counter)
			       (else
				#f)))
			  (:toc ,toc)
			  ,@(the-options opt :ident :class :vspace :toc)))
	       (body (if vspace
			 (list (slide-vspace vspace) (the-body opt))
			 (the-body opt))))))
      (set! %slide-the-slides (cons s %slide-the-slides))
      s))

;*---------------------------------------------------------------------*/
;*    ref ...                                                          */
;*---------------------------------------------------------------------*/
(define %slide-old-ref ref)

;; Extend the definition of `ref'.
;; FIXME: This technique breaks `ref' for some reason.
; (set! ref
;       (lambda args
; 	;; Filter out ARGS and look for a `:slide' keyword argument.
; 	(let loop ((slide #f)
; 		   (opt '())
; 		   (args args))
; 	  (if (null? args)
; 	      (set! opt (reverse! opt))
; 	      (let ((s? (eq? (car args) :slide)))
; 		(loop (if s? (cadr args) #f)
; 		      (if s? opt (cons (car args) opt))
; 		      (if s? (cddr args) (cdr args)))))

; 	  (format (current-error-port)
; 		  "slide.scm:ref: slide=~a opt=~a~%" slide opt)

; 	  (if (not slide)
; 	      (apply %slide-old-ref opt)
; 	      (new unresolved
; 		   (proc (lambda (n e env)
; 			   (cond
; 			    ((eq? slide 'next)
; 			     (let ((c (assq n %slide-the-slides)))
; 			       (if (pair? c)
; 				   (handle (cadr c))
; 				   #f)))
; 			    ((eq? slide 'prev)
; 			     (let ((c (assq n (reverse %slide-the-slides))))
; 			       (if (pair? c)
; 				   (handle (cadr c))
; 				   #f)))
; 			    ((number? slide)
; 			     (let loop ((s %slide-the-slides))
; 			       (cond
; 				((null? s)
; 				 #f)
; 				((= slide (markup-option
; 					   (car s) :number))
; 				 (handle (car s)))
; 				(else
; 				 (loop (cdr s))))))
; 			    (else
; 			     #f)))))))))


;*---------------------------------------------------------------------*/
;*    slide-pause ...                                                  */
;*---------------------------------------------------------------------*/
(define-markup (slide-pause)
   (new markup
      (markup 'slide-pause)))

;*---------------------------------------------------------------------*/
;*    slide-vspace ...                                                 */
;*---------------------------------------------------------------------*/
(define-markup (slide-vspace #!rest opt #!key (unit 'cm))
   (new markup
      (markup 'slide-vspace)
      (options `((:unit ,unit) ,@(the-options opt :unit)))
      (body (the-body opt))))

;*---------------------------------------------------------------------*/
;*    slide-embed ...                                                  */
;*---------------------------------------------------------------------*/
(define-markup (slide-embed #!rest opt
			    #!key
			    command
			    (geometry-opt "-geometry")
			    (geometry #f) (rgeometry #f)
			    (transient #f) (transient-opt #f)
			    (alt #f)
			    &skribe-eval-location)
   (if (not (string? command))
       (skribe-error 'slide-embed
		     "No command provided"
		     command)
       (new markup
	  (markup 'slide-embed)
	  (loc &skribe-eval-location)
	  (required-options '(:alt))
	  (options `((:geometry-opt ,geometry-opt)
		     (:alt ,alt)
		     ,@(the-options opt :geometry-opt :alt)))
	  (body (the-body opt)))))

;*---------------------------------------------------------------------*/
;*    slide-record ...                                                 */
;*---------------------------------------------------------------------*/
(define-markup (slide-record #!rest opt #!key ident class tag (play #t))
   (if (not tag)
       (skribe-error 'slide-record "Tag missing" tag)
       (new markup
	  (markup 'slide-record)
	  (ident ident)
	  (class class)
	  (options `((:play ,play) ,@(the-options opt)))
	  (body (the-body opt)))))

;*---------------------------------------------------------------------*/
;*    slide-play ...                                                   */
;*---------------------------------------------------------------------*/
(define-markup (slide-play #!rest opt #!key ident class tag color)
   (if (not tag)
       (skribe-error 'slide-play "Tag missing" tag)
       (new markup
	  (markup 'slide-play)
	  (ident ident)
	  (class class)
	  (options `((:color ,(if color (skribe-use-color! color) #f))
		     ,@(the-options opt :color)))
	  (body (the-body opt)))))

;*---------------------------------------------------------------------*/
;*    slide-play* ...                                                  */
;*---------------------------------------------------------------------*/
(define-markup (slide-play* #!rest opt
			    #!key ident class color (scolor "#000000"))
   (let ((body (the-body opt)))
      (for-each (lambda (lbl)
		   (match-case lbl
		      ((?id ?col)
		       (skribe-use-color! col))))
		body)
      (new markup
	 (markup 'slide-play*)
	 (ident ident)
	 (class class)
	 (options `((:color ,(if color (skribe-use-color! color) #f))
		    (:scolor ,(if color (skribe-use-color! scolor) #f))
		    ,@(the-options opt :color :scolor)))
	 (body body))))

;*---------------------------------------------------------------------*/
;*    base                                                             */
;*---------------------------------------------------------------------*/
(let ((be (find-engine 'base)))
   (skribe-message "Base slides setup...\n")
   ;; slide-pause
   (markup-writer 'slide-pause be
      :action #f)
   ;; slide-vspace
   (markup-writer 'slide-vspace be
      :options '()
      :action #f)
   ;; slide-embed
   (markup-writer 'slide-embed be
      :options '(:alt :geometry-opt)
      :action (lambda (n e)
		 (output (markup-option n :alt) e)))
   ;; slide-record
   (markup-writer 'slide-record be
      :options '(:tag :play)
      :action (lambda (n e)
		 (output (markup-body n) e)))
   ;; slide-play
   (markup-writer 'slide-play be
      :options '(:tag :color)
      :action (lambda (n e)
		 (output (markup-option n :alt) e)))
   ;; slide-play*
   (markup-writer 'slide-play* be
      :options '(:tag :color :scolor)
      :action (lambda (n e)
		 (output (markup-option n :alt) e))))


;*---------------------------------------------------------------------*/
;*    slide-number ...                                                 */
;*---------------------------------------------------------------------*/
(define-public (slide-number)
   (length (filter (lambda (n)
		      (and (is-markup? n 'slide)
			   (markup-option n :number)))
		   %slide-the-slides)))
