;;; lout.scm  --  Lout implementation of the `slide' package.
;;;
;;; Copyright 2005, 2006, 2008  Ludovic Courtès <ludo@gnu.org>
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

(define-module (skribilo package slide lout)
  :use-module (skribilo utils syntax)

  :autoload   (skribilo utils strings) (make-string-replace)
  :use-module (skribilo engine)
  :use-module (skribilo writer)
  :autoload   (skribilo output)        (output)
  :use-module (skribilo ast)
  :autoload   (skribilo lib)           (skribe-error)

  :use-module (srfi srfi-13) ;; `string-join'

  ;; XXX: If changing the following `autoload' to `use-module' doesn't work,
  ;; then you need to fix your Guile.  See this thread about
  ;; `make-autoload-interface':
  ;;
  ;;   http://article.gmane.org/gmane.lisp.guile.devel/5748
  ;;   http://lists.gnu.org/archive/html/guile-devel/2006-03/msg00004.html .

  :autoload (skribilo engine lout) (lout-tagify lout-output-pdf-meta-info
				    lout-verbatim-encoding))


(skribilo-module-syntax)


;;; TODO:
;;;
;;; Make some more PS/PDF trickery.

(format (current-error-port) "Lout slides setup...~%")

(let ((le (find-engine 'lout)))

  ;; FIXME: Automatically switching to `slides' is problematic, e.g., for the
  ;; user manual which embeds slides.
;  ;; Automatically switch to the `slides' document type.
;  (engine-custom-set! le 'document-type 'slides))

  (markup-writer 'slide le
     :options '(:title :number :toc :ident) ;; '(:bg :vspace :image)

     :validate (lambda (n e)
		  (eq? (engine-custom e 'document-type) 'slides))

     :before (lambda (n e)
		(display "\n@Overhead\n")
		(display "  @Title { ")
		(output (markup-option n :title) e)
		(display " }\n")
		(if (markup-ident n)
		    (begin
		       (display "  @Tag { ")
		       (display (lout-tagify (markup-ident n)))
		       (display " }\n")))
		(if (markup-option n :number)
		    (begin
		       (display "  @BypassNumber { ")
		       (output (markup-option n :number) e)
		       (display " }\n")))
		(display "@Begin\n")

		;; `doc' documents produce their PDF outline right after
		;; `@Text @Begin'; other types of documents must produce it
		;; as part of their first chapter.
		(lout-output-pdf-meta-info (ast-document n) e))

     :after "@End @Overhead\n")

  (markup-writer 'slide-vspace le
     :options '(:unit)
     :validate (lambda (n e)
		  (and (pair? (markup-body n))
		       (number? (car (markup-body n)))))
     :action (lambda (n e)
		(format #t "\n//~a~a # slide-vspace\n"
			(car (markup-body n))
			(case (markup-option n :unit)
			   ((cm)              "c")
			   ((point points pt) "p")
			   ((inch inches)     "i")
			   (else
			    (skribe-error 'lout
					  "Unknown vspace unit"
					  (markup-option n :unit)))))))

  (markup-writer 'slide-embed le
     :options '(:command :arguments :alt :geometry :geometry-opt)
     :action (lambda (n e)
	       (let ((command       (markup-option n :command))
                     (args          (markup-option n :arguments))
                     (alt           (markup-option n :alt))
                     (geometry      (markup-option n :geometry))
                     (geometry-opt  (markup-option n :geometry-opt)))
                 (format #t "~%\"~a\" @SkribiloEmbed { "
                         (string-append command " "
                                        (if (and geometry-opt geometry)
                                            (string-append geometry-opt " "
                                                           geometry " ")
                                            "")
                                        (string-join args)))
                 (output alt e)
                 (display " }\n"))))

  (markup-writer 'slide-pause le
     ;; FIXME:  Use a `pdfmark' custom action and a PDF transition action.
     ;; << /Type /Action
     ;; << /S /Trans
     ;; entry in the trans dict
     ;; << /Type /Trans  /S /Dissolve >>
     :action (lambda (n e)
	       (let ((filter (make-string-replace lout-verbatim-encoding))
		     (pdfmark "
[ {ThisPage} << /Trans << /S /Wipe /Dm /V /D 3 /M /O >> >> /PUT pdfmark"))
		 (display (lout-embedded-postscript-code
			   (filter pdfmark))))))

  ;; For movies, see
  ;; http://www.tug.org/tex-archive/macros/latex/contrib/movie15/movie15.sty .
  )


;;; Local Variables:
;;; coding: latin-1
;;; End:
