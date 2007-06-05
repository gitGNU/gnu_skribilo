;;; lout.scm  --  Lout implementation of the `pie' package.
;;;
;;; Copyright 2005, 2006, 2007  Ludovic Courtès <ludovic.courtes@laas.fr>
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

(define-module (skribilo package pie lout)
  :use-module (skribilo package pie)
  :use-module (skribilo ast)
  :autoload   (skribilo output) (output)
  :use-module (skribilo writer)
  :use-module (skribilo engine)
  :use-module (skribilo lib)
  :use-module (skribilo utils syntax)
  :autoload   (skribilo engine lout) (lout-color-specification))

(fluid-set! current-reader %skribilo-module-reader)



;;;
;;; Helper functions.
;;;

(let ((lout (find-engine 'lout)))
   (if lout
       (engine-custom-set! lout 'includes
	  (string-append (engine-custom lout 'includes)
			 "\n@SysInclude { pie } # Pie Charts\n"))))



;;;
;;; Writers.
;;;

(markup-writer 'pie (find-engine 'lout)
   :before (lambda (node engine)
	      (let* ((weights (map (lambda (slice)
				     (markup-option slice :weight))
				   (markup-body node)))
		     (total-weight (or (if (number? (markup-option node
								   :total))
					   (markup-option node :total)
					   #f)
				       (apply + weights))))

		 (if (= 0 total-weight)
		     (skribe-error 'lout
				   "Slices weight sum should not be zero"
				   total-weight))

		 ;; Attach useful information to the pie and its slices
		 (markup-option-add! node '&total-weight total-weight)

		 (display "\n@Pie\n")
		 (display "  abovecaption { ")
		 (if (markup-option node :title)
		     (output (markup-option node :title) engine))
		 (display " }\n")
		 (format #t "  totalweight { ~a }\n" total-weight)
		 (format #t "  initialangle { ~a }\n"
			  (or (markup-option node :initial-angle) 0))
		 (format #t "  finger { ~a }\n"
			 (case (markup-option node :labels)
			   ((outside) (if (markup-option node :fingers?)
					  "yes" "no"))
			   (else "no")))

		 ;; We assume `:radius' to be centimeters
		 (if (markup-option node :radius)
		     (format #t "  radius { ~ac }\n"
			     (markup-option node :radius)))

		 (format #t "  labelradius { ~a }\n"
			 (case (markup-option node :labels)
			   ((outside #f) "external")  ; FIXME: options are
						  ; not availble within
						  ; :before? (hence the #f)

			   ((inside)  "internal")
			   (else
			    (skribe-error 'lout
					  "`:labels' should be one of 'inside or 'outside."
					  (markup-option node :labels)))))
		 (display "{\n")))
   :after "\n} # @Pie\n")

(markup-writer 'slice (find-engine 'lout)
   :options '(:weight :detach? :color)
   :action (lambda (node engine)
	     (display "  @Slice\n")
	     (format #t "    detach { ~a }\n"
		     (if (markup-option node :detach?)
			 "yes"
			 "no"))
	     (format #t "     paint { ~a }\n"
		     (lout-color-specification (markup-option node
							      :color)))
	     (format #t "     weight { ~a }\n"
		     (markup-option node :weight))

	     (display "    label { ")
	     (output (markup-body node) engine)
	     (display " }\n")))

(markup-writer 'sliceweight (find-engine 'base)
   ;; This writer should work for every engine, provided the `pie' markup has
   ;; a proper `&total-weight' option.
   :action (lambda (node engine)
	      (let ((pct? (markup-option node :percentage?)))
		 (output (number->string
			  (pie-sliceweight-value node pct?))
			 engine))))

;;; arch-tag: b5221e30-f80e-4b72-a281-83ce19ddb755
