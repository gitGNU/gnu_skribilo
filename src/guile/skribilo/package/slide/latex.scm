;;; latex.scm  --  LaTeX implementation of the `slide' package.
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

(define-module (skribilo package slide latex)
  :use-module (skribilo package slide)
  :use-module (skribilo utils syntax)

  :use-module (skribilo engine)
  :use-module (skribilo writer)
  :autoload   (skribilo output)        (output)
  :use-module (skribilo ast)
  :use-module (skribilo lib)
  :autoload   (skribilo evaluator)     (evaluate-document)
  :autoload   (skribilo engine latex)  (skribe-get-latex-color)

  :autoload   (ice-9 regex)            (string-match)
  :use-module (ice-9 match)
  :use-module (srfi srfi-11)
  :use-module (srfi srfi-13)
  :use-module (srfi srfi-39)

  :export (%slide-latex-mode %slide-latex-initialize! *slide-advi-scale*))


(fluid-set! current-reader %skribilo-module-reader)


(define %slide-latex-mode 'seminar)

(define (%slide-latex-initialize!)
  (skribe-message "LaTeX slides setup...\n")
  (case %slide-latex-mode
    ((seminar)
     (%slide-seminar-setup!))
    ((advi)
     (%slide-advi-setup!))
    ((prosper)
     (%slide-prosper-setup!))
    (else
     (skribe-error 'slide "Illegal latex mode" %slide-latex-mode))))

(define string->integer string->number)

(define *slide-advi-scale*
  (make-parameter 1.0))


;*---------------------------------------------------------------------*/
;*    &slide-seminar-predocument ...                                   */
;*---------------------------------------------------------------------*/
(define &slide-seminar-predocument
   "\\special{landscape}
   \\slideframe{none}
   \\centerslidesfalse
   \\raggedslides[0pt]
   \\renewcommand{\\slideleftmargin}{0.2in}
   \\renewcommand{\\slidetopmargin}{0.3in}
   \\newdimen\\slidewidth \\slidewidth 9in")

;*---------------------------------------------------------------------*/
;*    &slide-seminar-maketitle ...                                     */
;*---------------------------------------------------------------------*/
(define &slide-seminar-maketitle
   "\\def\\labelitemi{$\\bullet$}
   \\def\\labelitemii{$\\circ$}
   \\def\\labelitemiii{$\\diamond$}
   \\def\\labelitemiv{$\\cdot$}
   \\pagestyle{empty}
   \\slideframe{none}
   \\centerslidestrue
   \\begin{slide}
   \\date{}
   \\maketitle
   \\end{slide}
   \\slideframe{none}
   \\centerslidesfalse")

;*---------------------------------------------------------------------*/
;*    &slide-prosper-predocument ...                                   */
;*---------------------------------------------------------------------*/
(define &slide-prosper-predocument
   "\\slideCaption{}\n")

;*---------------------------------------------------------------------*/
;*    latex                                                            */
;*---------------------------------------------------------------------*/
(define &latex-slide #f)
(define &latex-pause #f)
(define &latex-embed #f)
(define &latex-record #f)
(define &latex-play #f)
(define &latex-play* #f)

;;; FIXME: We shouldn't load `latex.scm' from here.  Instead, we should
;;; register a hook on its load.
(let ((le (find-engine 'latex)))
   ;; slide-vspace
   (markup-writer 'slide-vspace le
      :options '(:unit)
      :action (lambda (n e)
		 (display "\n\\vspace{")
		 (output (markup-body n) e)
		 (format #t " ~a}\n\n" (markup-option n :unit))))
   ;; slide-slide
   (markup-writer 'slide le
      :options '(:title :number :transition :vfill :toc :vspace :image)
      :action (lambda (n e)
		 (if (procedure? &latex-slide)
		     (&latex-slide n e))))
   ;; slide-pause
   (markup-writer 'slide-pause le
      :options '()
      :action (lambda (n e)
		 (if (procedure? &latex-pause)
		     (&latex-pause n e))))
   ;; slide-embed
   (markup-writer 'slide-embed le
      :options '(:alt :command :geometry-opt :geometry
		      :rgeometry :transient :transient-opt)
      :action (lambda (n e)
		 (if (procedure? &latex-embed)
		     (&latex-embed n e))))
   ;; slide-record
   (markup-writer 'slide-record le
      :options '(:tag :play)
      :action (lambda (n e)
		 (if (procedure? &latex-record)
		     (&latex-record n e))))
   ;; slide-play
   (markup-writer 'slide-play le
      :options '(:tag :color)
      :action (lambda (n e)
		 (if (procedure? &latex-play)
		     (&latex-play n e))))
   ;; slide-play*
   (markup-writer 'slide-play* le
      :options '(:tag :color :scolor)
      :action (lambda (n e)
		 (if (procedure? &latex-play*)
		     (&latex-play* n e)))))

;*---------------------------------------------------------------------*/
;*    %slide-seminar-setup! ...                                        */
;*---------------------------------------------------------------------*/
(define (%slide-seminar-setup!)
   (skribe-message "Seminar slides setup...\n")
   (let ((le (find-engine 'latex)))
      ;; latex configuration
      (define (seminar-slide n e)
	 (let ((nb (markup-option n :number))
	       (t (markup-option n :title)))
	    (display "\\begin{slide}\n")
	    (if nb (format #t "~a/~a -- " nb (slide-number)))
	    (output t e)
	    (display "\\hrule\n"))
	 (output (markup-body n) e)
	 (if (markup-option n :vill) (display "\\vfill\n"))
	 (display "\\end{slide}\n"))
      (engine-custom-set! le 'documentclass
	 "\\documentclass[landscape]{seminar}\n")
      (let ((o (engine-custom le 'predocument)))
	 (engine-custom-set! le 'predocument
	    (if (string? o)
		(string-append &slide-seminar-predocument o)
		&slide-seminar-predocument)))
      (engine-custom-set! le 'maketitle
	 &slide-seminar-maketitle)
      (engine-custom-set! le 'hyperref-usepackage
	 "\\usepackage[setpagesize=false]{hyperref}\n")
      ;; slide-slide
      (set! &latex-slide seminar-slide)))

;*---------------------------------------------------------------------*/
;*    %slide-advi-setup! ...                                           */
;*---------------------------------------------------------------------*/
(define (%slide-advi-setup!)
   (skribe-message "Generating `Advi Seminar' slides...\n")
   (let ((le (find-engine 'latex)))
      (define (advi-geometry geo)
	 (let ((r (string-match "([0-9]+)x([0-9]+)" geo)))
	    (if (pair? r)
		(let* ((w (cadr r))
		       (h (caddr r)))
		   (values "" (string-append w "x" h "+!x+!y")))
		(let ((r (string-match "([0-9]+)x([0-9]+)[+](-?[0-9]+)[+](-?[0-9]+)" geo)))
		   (if (pair? r)
		       (let ((w (number->string (/ (string->integer (cadr r))
						   (*slide-advi-scale*))))
			     (h (number->string (/ (string->integer (caddr r))
						   (*slide-advi-scale*)))))
			  (values (string-append "width=" w "cm,height=" h "cm")
				  "!g"))
		       (values "" geo))))))
      (define (advi-transition trans)
	 (cond
	    ((string? trans)
	     (format #t "\\advitransition{~s}" trans))
	    ((and (symbol? trans)
		  (memq trans '(wipe block slide)))
	     (format #t "\\advitransition{~s}" trans))
	    (else
	     #f)))
      ;; latex configuration
      (define (advi-slide n e)
	 (let ((i (markup-option n :image))
	       (n (markup-option n :number))
	       (t (markup-option n :title))
	       (lt (markup-option n :transition))
	       (gt (engine-custom e 'transition)))
	    (if (and i (engine-custom e 'advi))
		(format #t "\\advibg[global]{image=~a}\n"
			(if (and (pair? i)
				 (null? (cdr i))
				 (string? (car i)))
			    (car i)
			    i)))
	    (display "\\begin{slide}\n")
	    (advi-transition (or lt gt))
	    (if n (format #t "~a/~a -- " n (slide-number)))
	    (output t e)
	    (display "\\hrule\n"))
	 (output (markup-body n) e)
	 (if (markup-option n :vill) (display "\\vfill\n"))
	 (display "\\end{slide}\n\n\n"))
      ;; advi record
      (define (advi-record n e)
	 (display "\\advirecord")
	 (when (markup-option n :play) (display "[play]"))
	 (format #t "{~a}{" (markup-option n :tag))
	 (output (markup-body n) e)
	 (display "}"))
      ;; advi play
      (define (advi-play n e)
	 (display "\\adviplay")
	 (let ((c (markup-option n :color)))
	    (when c
	       (display "[")
	       (display (skribe-get-latex-color c))
	       (display "]")))
	 (format #t "{~a}" (markup-option n :tag)))
      ;; advi play*
      (define (advi-play* n e)
	 (let ((c (skribe-get-latex-color (markup-option n :color)))
	       (d (skribe-get-latex-color (markup-option n :scolor))))
	    (let loop ((lbls (markup-body n))
		       (last #f))
	       (when last
		  (display "\\adviplay[")
		  (display d)
		  (format #t "]{~a}" last))
	       (when (pair? lbls)
		  (let ((lbl (car lbls)))
		     (match lbl
			((id col)
			 (display "\\adviplay[")
			 (display (skribe-get-latex-color col))
			 (display (string-append "]{" id "}"))
			 (evaluate-document (slide-pause) e)
			 (loop (cdr lbls) id))
			(else
			 (display "\\adviplay[")
			 (display c)
			 (format #t "]{~a}" lbl)
			 (evaluate-document (slide-pause) e)
			 (loop (cdr lbls) lbl))))))))
      (engine-custom-set! le 'documentclass
	 "\\documentclass{seminar}\n")
      (let ((o (engine-custom le 'predocument)))
	 (engine-custom-set! le 'predocument
	    (if (string? o)
		(string-append &slide-seminar-predocument o)
		&slide-seminar-predocument)))
      (engine-custom-set! le 'maketitle
	 &slide-seminar-maketitle)
      (engine-custom-set! le 'usepackage
	 (string-append "\\usepackage{advi}\n"
			(engine-custom le 'usepackage)))
      ;; slide
      (set! &latex-slide advi-slide)
      (set! &latex-pause
	    (lambda (n e) (display "\\adviwait\n")))
      (set! &latex-embed
	    (lambda (n e)
	       (let ((geometry-opt (markup-option n :geometry-opt))
		     (geometry (markup-option n :geometry))
		     (rgeometry (markup-option n :rgeometry))
		     (transient (markup-option n :transient))
		     (transient-opt (markup-option n :transient-opt))
		     (cmd (markup-option n :command)))
		  (let* ((a (string-append "ephemeral="
					   (symbol->string (gensym))))
			 (c (cond
			       (geometry
				(string-append cmd " "
					       geometry-opt " "
					       geometry))
			       (rgeometry
				(let-values (((aopt dopt)
                                              (advi-geometry rgeometry)))
				   (set! a (string-append a "," aopt))
				   (string-append cmd " "
						  geometry-opt " "
						  dopt)))
			       (else
				cmd)))
			 (c (if (and transient transient-opt)
				(string-append c " " transient-opt " !p")
				c)))
		     (format #t "\\adviembed[~a]{~a}\n" a c)))))
      (set! &latex-record advi-record)
      (set! &latex-play advi-play)
      (set! &latex-play* advi-play*)))

;*---------------------------------------------------------------------*/
;*    %slide-prosper-setup! ...                                        */
;*---------------------------------------------------------------------*/
(define (%slide-prosper-setup!)
   (skribe-message "Generating `Prosper' slides...\n")
   (let ((le (find-engine 'latex))
	 (overlay-count 0))
      ;; transitions
      (define (prosper-transition trans)
	 (cond
	    ((string? trans)
	     (format #t "[~s]" trans))
	    ((eq? trans 'slide)
	     (display "[Blinds]"))
	    ((and (symbol? trans)
		  (memq trans '(split blinds box wipe dissolve glitter)))
	     (format #t "[~s]"
		     (string-upcase (symbol->string trans))))
	    (else
	     #f)))
      ;; latex configuration
      (define (prosper-slide n e)
	 (let* ((t (markup-option n :title))
		(lt (markup-option n :transition))
		(gt (engine-custom e 'transition))
		(pa (search-down (lambda (x) (is-markup? x 'slide-pause)) n))
		(lpa (length pa)))
	    (set! overlay-count 1)
	    (if (>= lpa 1) (format #t "\\overlays{~a}{%\n" (+ 1 lpa)))
	    (display "\\begin{slide}")
	    (prosper-transition (or lt gt))
	    (display "{")
	    (output t e)
	    (display "}\n")
	    (output (markup-body n) e)
	    (display "\\end{slide}\n")
	    (if (>= lpa 1) (display "}\n"))
	    (newline)
	    (newline)))
      (engine-custom-set! le 'documentclass "\\documentclass[pdf,skribe,slideColor,nototal]{prosper}\n")
      (let* ((cap (engine-custom le 'slide-caption))
	     (o (engine-custom le 'predocument))
	     (n (if (string? cap)
		    (format #f "~a\\slideCaption{~a}\n"
			    &slide-prosper-predocument
			    cap)
		    &slide-prosper-predocument)))
	 (engine-custom-set! le 'predocument
	    (if (string? o) (string-append n o) n)))
      (engine-custom-set! le 'hyperref-usepackage "\\usepackage{hyperref}\n")
      ;; writers
      (set! &latex-slide prosper-slide)
      (set! &latex-pause
	    (lambda (n e)
	       (set! overlay-count (+ 1 overlay-count))
	       (format #t "\\FromSlide{~s}%\n" overlay-count)))))

;*---------------------------------------------------------------------*/
;*    Setup ...                                                        */
;*---------------------------------------------------------------------*/
(let* ((opt &slide-load-options)
       (p (memq :prosper opt)))
   (if (and (pair? p) (pair? (cdr p)) (cadr p))
       ;; prosper
       (set! %slide-latex-mode 'prosper)
       (let ((a (memq :advi opt)))
	  (if (and (pair? a) (pair? (cdr a)) (cadr a))
	      ;; advi
	      (set! %slide-latex-mode 'advi)))))



;;;
;;; Initialization.
;;;

(%slide-latex-initialize!)

;;; arch-tag: b99e2c65-55f7-462c-8482-f47c7e223538
