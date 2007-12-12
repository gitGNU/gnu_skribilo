#!/bin/sh
# aside from this initial boilerplate, this is actually -*- scheme -*- code
main='(module-ref (resolve-module '\''(skribilo)) '\'main')'
exec ${GUILE-guile} --debug -l $0 -c "(apply $main (cdr (command-line)))" "$@"
!#

;;; skribilo.scm  --  The Skribilo document processor.
;;;
;;; Copyright 2005, 2006, 2007  Ludovic Courtès <ludo@gnu.org>
;;; Copyright 2003, 2004  Erick Gallesio - I3S-CNRS/ESSI <eg@unice.fr>
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

;;; Commentary:
;;;
;;; Usage: skribilo [ARGS]
;;;
;;; Process a skribilo document using options from the command-line.
;;;
;;; Code:



(define-module (skribilo)
  :autoload    (skribilo module) (make-user-module *skribilo-user-module*)
  :autoload    (skribilo engine) (*current-engine*)
  :autoload    (skribilo reader) (*document-reader*)

  :use-module  (skribilo utils syntax)
  :use-module  (skribilo evaluator)
  :use-module  (skribilo debug)
  :use-module  (skribilo parameters)
  :use-module  (skribilo config)
  :use-module  (skribilo lib)

  :autoload    (srfi srfi-1)     (alist-cons)
  :use-module  (srfi srfi-37)
  :use-module  (srfi srfi-39)
  :use-module  (ice-9 optargs))


;; Install the Skribilo module syntax reader.
(fluid-set! current-reader %skribilo-module-reader)

(if (not (keyword? :kw))
    (error "guile-reader sucks"))




;;;
;;; Legacy option processing (FIXME: To be removed!).
;;;

(define* (process-option-specs longname
			       :key (alternate #f) (arg #f) (help #f)
			       :rest thunk)
  "Process STkLos-like option specifications and return getopt-long option
specifications."
  `(,(string->symbol longname)
    ,@(if alternate
	  `((single-char ,(string-ref alternate 0)))
	  '())
    (value ,(if arg #t #f))))

(define (raw-options->getopt-long options)
  "Converts @var{options} to a getopt-long-compatible representation."
  (map (lambda (option-specs)
	 (apply process-option-specs (car option-specs)))
       options))

(define-macro (define-options binding . options)
  `(define ,binding (quote ,(raw-options->getopt-long options))))

(define-options skribilo-options
  (("reader" :alternate "R" :arg reader
    (nothing)))
  (("compat" :arg compat
    :help "use the COMPAT compatibility mode, e.g., `skribe'"))
  (("target" :alternate "t" :arg target
    :help "sets the output format to <target>")
   (set! engine (string->symbol target)))
  (("load-path" :alternate "I" :arg path :help "adds <path> to Skribe path")
   (set! paths (cons path paths)))
  (("bib-path" :alternate "B" :arg path :help "adds <path> to bibliography path")
   (skribe-bib-path-set! (cons path (skribe-bib-path))))
  (("source-path" :alternate "S" :arg path :help "adds <path> to source path")
   (skribe-source-path-set! (cons path (skribe-source-path))))
  (("image-path" :alternate "P" :arg path :help "adds <path> to image path")
   (skribe-image-path-set! (cons path (skribe-image-path))))
  (("split-chapters" :alternate "C" :arg chapter
    :help "emit chapter's sections in separate files")
   (set! *skribe-chapter-split* (cons chapter *skribe-chapter-split*)))
  (("preload" :arg file :help "preload <file>")
   (set! *skribe-preload* (cons file *skribe-preload*)))
  (("use-variant" :alternate "u" :arg variant
    :help "use <variant> output format")
   (set! *skribe-variants* (cons variant *skribe-variants*)))
  (("base" :alternate "b" :arg base
    :help "base prefix to remove from hyperlinks")
   (set! *skribe-ref-base* base))
  (("rc-dir" :arg dir :alternate "d" :help "set the RC directory to <dir>")
   (set! *skribe-rc-directory* dir))

  ;;"File options:"
  (("no-init-file" :help "Dont load rc Skribe file")
   (set! *load-rc* #f))
  (("output" :alternate "o" :arg file :help "set the output to <file>")
   (set! *skribe-dest* file)
   (let* ((s (file-suffix file))
	  (c (assoc s *skribe-auto-mode-alist*)))
     (if (and (pair? c) (symbol? (cdr c)))
	 (set! *skribe-engine* (cdr c)))))

  ;;"Misc:"
  (("help" :alternate "h" :help "provides help for the command")
   (arg-usage (current-error-port))
   (exit 0))
  (("options" :help "display the skribe options and exit")
   (arg-usage (current-output-port) #t)
   (exit 0))
  (("version" :alternate "V" :help "displays the version of Skribe")
   (version)
   (exit 0))
  (("query" :alternate "q"
    :help "displays informations about Skribe conf.")
   (query)
   (exit 0))
  (("verbose" :alternate "v" :arg level
    :help "sets the verbosity to <level>. Use -v0 for crystal silence")
   (let ((val (string->number level)))
     (if (integer? val)
	 (set! *skribe-verbose* val))))
  (("warning" :alternate "w" :arg level
    :help "sets the verbosity to <level>. Use -w0 for crystal silence")
   (let ((val (string->number level)))
     (if (integer? val)
	 (set! *skribe-warning* val))))
  (("debug" :alternate "g" :arg level :help "sets the debug <level>")
   (let ((val (string->number level)))
     (if (integer? val)
	 (set-skribe-debug! val)
	 (begin
	   ;; Use the symbol for debug
	   (set-skribe-debug!	    1)
	   (add-skribe-debug-symbol (string->symbol level))))))
  (("no-color" :help "disable coloring for output")
   (no-debug-color))
  (("custom" :alternate "c" :arg key=val :help "Preset custom value")
   (let ((args (string-split key=val "=")))
     (if (and (list args) (= (length args) 2))
	 (let ((key (car args))
	       (val (cadr args)))
	   (set! *skribe-precustom* (cons (cons (string->symbol key) val)
					  *skribe-precustom*)))
	 (error 'parse-arguments "Bad custom ~S" key=val))))
  (("eval" :alternate "e" :arg expr :help "evaluate expression <expr>")
   (with-input-from-string expr
      (lambda () (eval (read))))))



;;;
;;; Help.
;;;

(define (skribilo-show-help)
  (format #t "Usage: skribilo [OPTIONS] [INPUT]

Processes a Skribilo/Skribe source file and produces its output.

  --reader=READER  Use READER to parse the input file (by default,
                   the `skribe' reader is used)
  --target=ENGINE  Use ENGINE as the underlying engine
  --compat=COMPAT  Use COMPAT as the compatibility layer, e.g., \"skribe\"

  --help           Give this help list
  --version        Print program version

Report bugs to <~a>.~%"
          (skribilo-bug-report-address)))

(define (skribilo-show-version)
  (format #t "skribilo ~a~%" (skribilo-version)))

(define (leave fmt . args)
  (apply format (current-error-port) (string-append fmt "~%") args)
  (exit 1))



;;;
;;; Document processing.
;;;

(define *load-rc* #f)  ;; FIXME:  This should go somewhere else.

(define (load-rc)
  (if *load-rc*
    (let ((file (make-path (*rc-directory*) (*rc-file*))))
      (if (and file (file-exists? file))
	(load file)))))

(define *skribilo-output-port* (make-parameter (current-output-port)))

(define (doskribe compat)
  (let ((output-port (current-output-port))
	(user-module (current-module)))
    (dynamic-wind
	(lambda ()
	  ;; FIXME: Using this technique, anything written to `stderr' will
	  ;; also end up in the output file (e.g. Guile warnings).
	  (set-current-output-port (*skribilo-output-port*))
          (let ((user (make-user-module (string->symbol compat))))
            (set-current-module user)
            (*skribilo-user-module* user)))
	(lambda ()
	  ;;(format #t "engine is ~a~%" (*current-engine*))
	  (evaluate-document-from-port (current-input-port)
				       (*current-engine*)))
	(lambda ()
	  (set-current-output-port output-port)
	  (set-current-module user-module)
          (*skribilo-user-module* #f)))))



;;;
;;; Argument parsing.
;;;

(define (make-path-processor key)
  (lambda (opt name arg result)
    (let ((path (assoc key result)))
      (alist-cons key (if (pair? path)
                          (cons arg (cdr path))
                          (list arg))
                  (alist-delete key result eq?)))))

(define (make-level-processor key default)
  (lambda (opt name arg result)
    (alist-cons key (if (string? arg)
                        (or (string->number arg) default)
                        default)
                result)))

(define %options
  ;; Specifications of the command-line options.
  (list (option '(#\h "help") #f #f
                (lambda args
                  (skribilo-show-help)
                  (exit 0)))
        (option '(#\V "version") #f #f
                (lambda args
                  (skribilo-show-version)
                  (exit 0)))

        (option '(#\R "reader") #t #f
                (lambda (opt name arg result)
                  (alist-cons :reader arg result)))
        (option '(#\t "target") #t #f
                (lambda (opt name arg result)
                  (alist-cons :target arg result)))
        (option '(#\o "output") #t #f
                (lambda (opt name arg result)
                  (if (assoc :output result)
                      (leave "~a: only one output at a time" arg)
                      (alist-cons :output arg result))))

        (option '("compat") #t #f
                (lambda (opt name arg result)
                  (alist-cons :compat arg result)))
        (option '(#\I "doc-path") #t #f
                (make-path-processor :doc-path))
        (option '(#\B "bib-path") #t #f
                (make-path-processor :bib-path))
        (option '(#\S "source-path") #t #f
                (make-path-processor :source-path))
        (option '(#\P "image-path") #t #f
                (make-path-processor :image-path))

        (option '(#\v "verbose") #f #t
                (make-level-processor :verbose 0))
        (option '(#\w "warning") #f #t
                (make-level-processor :warning 1))
        (option '(#\g "debug") #f #t
                (lambda (opt name arg result)
                  (let ((num (string->number arg)))
                    (if (integer? num)
                        (alist-cons key (if (string? arg)
                                            (or (string->number arg) default)
                                            default)
                                    result)
                        (let ((watched (assoc :watched-symbols result)))
                          (alist-cons :watched-symbols
                                      (cons (string->symbol arg)
                                            (cdr watched))
                                      result))))))))

(define %default-options
  ;; Default value of various command-line options.
  '((:debug     . 0)
    (:warning   . 1)
    (:verbose   . 0)
    (:reader    . "skribe")
    (:target    . "html")
    (:compat    . "skribilo")
    (:doc-path    ".")
    (:bib-path    ".")
    (:source-path ".")
    (:image-path  ".")
    (:watched-symbols)))

(define (parse-args args)
  "Parse argument list @var{args} and return an alist with all the relevant
options."
  (args-fold args %options
             (lambda (opt name arg result)
               (leave "~A: unrecognized option" opt))
             (lambda (file result)
               (if (assoc :input result)
                   (leave "~a: only one input file at a time" file)
                   (alist-cons :input file result)))
             %default-options))


;;;
;;; The program.
;;;

(define-public (skribilo . args)
  (let* ((options           (parse-args args))

	 (reader-name       (string->symbol (assoc-ref options :reader)))
	 (engine            (string->symbol (assoc-ref options :target)))
         (input-file        (assoc-ref options :input))
	 (output-file       (assoc-ref options :output))

         (verbosity-level   (assoc-ref options :verbose))
	 (debugging-level   (assoc-ref options :debug))
	 (warning-level     (assoc-ref options :warning))
         (watched-symbols   (assoc-ref options :watched-symbols))

	 (load-path         (assoc-ref options :doc-path))
	 (bib-path          (assoc-ref options :bib-path))
	 (source-path       (assoc-ref options :source-path))
	 (image-path        (assoc-ref options :image-path))
         (compat            (assoc-ref options :compat))
	 (preload           '()) ;; FIXME: Implement
	 (variants          '()) ;; FIXME: Implement
         )

    (if (> (*debug*) 4)
	(set! %load-hook
	      (lambda (file)
		(format #t "~~ loading `~a'...~%" file))))

    (parameterize ((*document-reader* (make-reader reader-name))
		   (*current-engine*  engine)
		   (*document-path*   load-path)
		   (*bib-path*        bib-path)
		   (*source-path*     source-path)
		   (*image-path*      image-path)
		   (*debug*           debugging-level)
                   (*watched-symbols* watched-symbols)
		   (*warning*         warning-level)
		   (*verbose*         verbosity-level))

      ;; Load the user rc file (FIXME)
      ;;(load-rc)

      (for-each (lambda (f)
		  (skribe-load f :engine (*current-engine*)))
		preload)

      ;; Load the specified variants. (FIXME)
;;       (for-each (lambda (x)
;; 		  (skribe-load (format #f "~a.skr" x)
;; 			       :engine (*current-engine*)))
;; 		(reverse! variants))

      (if (and output-file (file-exists? output-file))
          (delete-file output-file))

      (parameterize ((*destination-file* output-file)
                     (*source-file*      input-file)
                     (*skribilo-output-port*
                      (if (string? output-file)
                          (open-output-file output-file)
                          (current-output-port))))

        (setvbuf (*skribilo-output-port*) _IOFBF 16384)

        (if input-file
            (with-input-from-file input-file
              (lambda ()
                (doskribe compat)))
            (doskribe compat))

        ;; Make sure the output port is flushed before we leave.
        (force-output (*skribilo-output-port*))))))


(define main skribilo)

;;; skribilo ends here.
