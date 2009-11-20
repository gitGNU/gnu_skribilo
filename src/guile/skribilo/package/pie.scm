;;; pie.scm  --  An pie-chart formatting package.
;;;
;;; Copyright 2005, 2006, 2007, 2009  Ludovic Courtès <ludo@gnu.org>
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

(define-module (skribilo package pie)
  :autoload   (skribilo ast)    (markup? markup-ident ast-parent)
  :autoload   (skribilo output) (output)
  :use-module (skribilo writer)
  :use-module (skribilo engine)
  :use-module (skribilo lib)            ;; `skribe-error' et al.
  :use-module (skribilo utils syntax)
  :use-module (skribilo utils keywords) ;; `the-options', etc.
  :use-module (skribilo utils strings)  ;; `make-string-replace'
  :autoload   (skribilo color)        (color->rgb)
  :autoload   (skribilo package base) (bold)
  :autoload   (srfi srfi-13)          (string-concatenate)
  :autoload   (ice-9 popen)           (open-output-pipe)
  :use-module (ice-9 optargs)
  :export     (%ploticus-program %ploticus-debug?
               pie-sliceweight-value pie-remove-markup))

(skribilo-module-syntax)



;;;
;;; Markup.
;;;

(define-markup (pie :rest opts
		    :key (ident #f) (class "pie") (title "Pie Chart")
		    (initial-angle 0) (total #f) (radius 3)
		    (fingers? #t) (labels 'outside))
   (new container
	(markup 'pie)
	(ident (or ident (symbol->string (gensym "pie"))))
        (loc &invocation-location)
	(options (the-options opts))
	(body (the-body opts))))

(define-markup (slice :rest opts
		      :key (ident #f) (class "pie-slice")
                           (weight 1) (color "white") (detach? #f))
   (new container
	(markup 'slice)
	(ident (or ident (symbol->string (gensym "slice"))))
        (loc &invocation-location)
	(weight weight)
	(color color)
	(detach? detach?)
	(options (the-options opts))
	(body (the-body opts))))

(define-markup (sliceweight :rest opts
			    :key (ident #f) (class "pie-sliceweight")
                                 (percentage? #f))
   (new markup
	(markup 'sliceweight)
	(ident (or ident (symbol->string (gensym "sliceweight"))))
        (loc &invocation-location)
	(percentage? percentage?)
	(options (the-options opts))
	(body '())))



;;;
;;; Helper functions.
;;;

(define (make-rounder pow10)
  ;; Return a procedure that round to 10 to the -POW10.
  (let ((times (expt 10.0 pow10)))
    (lambda (x)
      (/ (round (* x times)) times))))

(define (pie-sliceweight-value sw-node pct?)
   "Return the value that should be displayed by `sw-node', a
   `sliceweight' markup node.  If `pct?' is true, then this value
   should be a percentage."
   (let* ((the-slice (ast-parent sw-node))
	  (weight (and the-slice (markup-option the-slice :weight))))
      (if (not the-slice)
	  (skribe-error 'lout
			"`sliceweight' node not within a `slice' body"
			sw-node)
	  (if pct?
	      (let* ((the-pie (ast-parent the-slice))
		     (total (and the-pie
				 (markup-option the-pie
						'&total-weight))))
		 (if (not the-pie)
		     (skribe-error 'lout
				   "`slice' not within a `pie' body"
				   the-slice)
		     (* 100.0 (/ weight total)))) ;; flonum (FIXME: precision)

	      weight))))

(define (pie-remove-markup node)
  "Remove markup from `node', ie. turn something like `(it \"hello\")' into
the string \"hello\".  Implement `sliceweight' markups too."
  (define percentage-round (make-rounder 2))

  (if (markup? node)
      (if (and node (is-markup? node 'sliceweight))
	  (let* ((pct? (markup-option node :percentage?))
		 (value (pie-sliceweight-value node pct?)))
	     (number->string (percentage-round value)))
	  (pie-remove-markup (markup-body node)))
      (if (list? node)
	  (string-concatenate (map pie-remove-markup node))
	  node)))

(define strip-newlines (make-string-replace '((#\newline " "))))

(define (select-output-format engine)
  ;; Choose an ouptut format suitable for ENGINE.
  (define %supported-formats '("png" "ps" "eps" "svg" "svgz"))
  (define %default-format    "png")

  (let ((fmt (engine-custom engine 'image-format)))
    (cond ((string? fmt) fmt)
	  ((and (list?   fmt) (not (null? fmt)))
	   (let ((f (car fmt)))
	     (if (member f %supported-formats)
		 f
		 %default-format)))
	  (else %default-format))))


;;;
;;; Default implementation (`base' engine).
;;;

;; Ploticus-based implementation of pie charts, suitable for most engines.
;; See http://ploticus.sf.net for info about Ploticus.

(define %ploticus-program "ploticus")
(define %ploticus-debug? #f)

(define (color-spec->ploticus color-spec)
  (define round (make-rounder 2))

  (call-with-values (lambda () (color->rgb color-spec))
    (lambda (r g b)
      (format #f "rgb(~a,~a,~a)"
	      (round (/ r 255.0))
	      (round (/ g 255.0))
	      (round (/ b 255.0))))))

(define (ploticus-script pie)
  (let* ((weights (map (lambda (slice)
			 (markup-option slice :weight))
		       (markup-body pie)))
	 (colors (map (lambda (slice)
			(let ((c (markup-option slice :color)))
			  (string-append (color-spec->ploticus c)
					 " ")))
		      (markup-body pie)))
	 (total-weight
          (let ((w (or (if (number? (markup-option pie
                                                   :total))
                           (markup-option pie :total)
                           #f)
                       (apply + weights))))

            ;; Attach useful information to the pie and its slices
            (markup-option-add! pie '&total-weight w)
            w))

	 ;; One slice label per line -- so we need to remove
	 ;; newlines from labels.
	 (labels (map (lambda (b)
			(strip-newlines (pie-remove-markup b)))
		      (markup-body pie)))

; 		     (flat-title (map pie-remove-markup
; 				      (markup-option pie :title)))
	 (detached (map (lambda (slice)
			  (let ((d (markup-option slice
						  :detach?)))
			    (cond ((number? d) d)
				  (d           0.5) ;; default
				  (#t          0))))
			(markup-body pie)))

	 (initial-angle (or (markup-option pie :initial-angle)
			    0))
	 (radius (or (markup-option pie :radius)
                     ;; 2.5cm is the default used in `pie.pl'.
                     2.5)))

    (string-concatenate
	   (append (list "#proc getdata\n" "data: ")
		   (map (lambda (weight)
			  (string-append (number->string weight)
					 "\n"))
			weights)
		   `("\n"
;					      "#proc page\n"
;					      "title " ,@flat-title
;					      "\n"
		     "#proc pie\n"
		     "total: "
		     ,(number->string total-weight)
		     "\n"
		     "datafield: " "1" "\n")
		   `("firstslice: " ,(number->string initial-angle) "\n")
		   `("radius: " ,(number->string radius) "\n")

                   ;; This value is the default used in `pie.pl'.  It happens
                   ;; to work well (at least in PNG output) for 1 < RADIUS < 4.
                   ;; XXX: For RADIUS beyond that, the pie or labels are
                   ;; often truncated.
		   `("center: 6.25 6.25\n")

		   `("labelmode: "
		     ,(case (markup-option
			     pie :labels)
			((outside) "line+label")
			((inside)  "labelonly")
			((legend)  "legend")
			(else      "legend"))
		     "\n"
		     "labels: " ,@(map (lambda (label)
					 (string-append label "\n"))
				       labels)
		     "\n")
		   `("explode: "
		     ,@(map (lambda (number)
			      (string-append (number->string number)
					     " "))
			    detached)
		     "\n")
		   `("colors: " ,@colors "\n")))))

(markup-writer 'pie (find-engine 'base)
  :options '(:title :initial-angle :total :radius :labels)
  :action (lambda (node engine)
	    (let* ((fmt (select-output-format engine))
		   (pie-file (string-append (markup-ident node) "."
					    fmt))
		   (port (open-output-pipe
			  (string-append %ploticus-program
					 " -o " pie-file
					 " -cm -" fmt " -stdin")))
		   (script (ploticus-script node)))


		(if %ploticus-debug?
		    (format (current-error-port) "** Ploticus script: ~a"
			    script))

		(display script port)

		(let ((exit-val (status:exit-val (close-pipe port))))
		  (if (not (eqv? 0 exit-val))
		      (skribe-error 'pie/ploticus
				    "ploticus exited with error code"
				    exit-val)))

		(if (not (file-exists? pie-file))
		    (skribe-error 'ploticus
				  "Ploticus did not create the image file"
				  script))

		(if (markup-option node :title)
		    (output (list (bold (markup-option node :title))
				  (linebreak))
			    engine))

		(output (image :file pie-file
			       :class (markup-option node :class)
			       (or (markup-option node :title)
				   "A Pie Chart"))
			engine))))

(markup-writer 'slice (find-engine 'base)
  :options '(:weight :color :detach?)
  :action (lambda (node engine)
	    ;; Nothing to do here
	    (error "slice: this writer should never be invoked")))

(markup-writer 'sliceweight (find-engine 'base)
   ;; This writer should work for every engine, provided the `pie' markup has
   ;; a proper `&total-weight' option.
   :options '(:percentage?)
   :action (lambda (node engine)
	      (let ((pct? (markup-option node :percentage?)))
		 (output (number->string
			  (pie-sliceweight-value node pct?))
			 engine))))


;;;
;;; Initialization.
;;;

(when-engine-is-loaded 'lout
  (lambda ()
    (resolve-module '(skribilo package pie lout))))


;;; arch-tag: 8095d8f6-b810-4619-9fdb-23fb94a77ee3
