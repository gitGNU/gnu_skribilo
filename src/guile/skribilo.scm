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

  :autoload    (srfi srfi-1)     (alist-cons)
  :use-module  (srfi srfi-37)
  :use-module  (srfi srfi-39)

  :export (skribilo))


;; Install the Skribilo module syntax reader.
(fluid-set! current-reader %skribilo-module-reader)

(if (not (keyword? :kw))
    (error "guile-reader sucks"))



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

(define *skribilo-output-port* (make-parameter (current-output-port)))

(define (doskribe module)
  (let ((output-port (current-output-port))
	(user-module (current-module)))
    (dynamic-wind
	(lambda ()
	  ;; FIXME: Using this technique, anything written to `stderr' will
	  ;; also end up in the output file (e.g. Guile warnings).
	  (set-current-output-port (*skribilo-output-port*))
          (set-current-module module)
          (*skribilo-user-module* module))
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

        (option '(#\b "base") #t #f
                (lambda (opt name arg result)
                  (if (assoc :ref-base result)
                      (leave "~a: only one reference base at a time" arg)
                      (alist-cons :ref-base arg result))))
        (option '(#\e "eval") #t #f
                (lambda (opt name arg result)
                  (let ((expr-list (assoc :expressions result))
                        (expr      (with-input-from-string arg read)))
                    (alist-cons :expressions
                                (if (pair? expr-list)
                                    (cons expr (cdr expr-list))
                                    (list expr))
                                result))))
        (option '(#\p "preload") #t #f
                (lambda (opt name arg result)
                  (let ((preloads (assoc :preloads result)))
                    (alist-cons :preloads
                                (if (pair? preloads)
                                    (cons arg preloads)
                                    (list arg))
                                result))))

        (option '(#\v "verbose") #f #t
                (make-level-processor :verbose 0))
        (option '(#\w "warning") #f #t
                (make-level-processor :warning 1))
        (option '(#\g "debug") #f #t
                (lambda (opt name arg result)
                  (let ((num (string->number arg)))
                    (if (integer? num)
                        (alist-cons :debug num result)
                        (let ((watched (assoc :watched-symbols result)))
                          (alist-cons :watched-symbols
                                      (cons (string->symbol arg)
                                            (cdr watched))
                                      result))))))
        (option '("no-color") #f #f
                (lambda (opt name arg result)
                  (alist-cons :no-color? #t result)))))

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

(define (skribilo . args)
  (let* ((options           (parse-args args))

	 (reader-name       (string->symbol (assoc-ref options :reader)))
	 (engine            (string->symbol (assoc-ref options :target)))
         (input-file        (assoc-ref options :input))
	 (output-file       (assoc-ref options :output))

         (verbosity-level   (assoc-ref options :verbose))
	 (debugging-level   (assoc-ref options :debug))
	 (warning-level     (assoc-ref options :warning))
         (watched-symbols   (assoc-ref options :watched-symbols))
         (color?            (not (assoc-ref options :no-color?)))

         (ref-base          (assoc-ref options :ref-base))
         (expressions       (assoc-ref options :expressions))

	 (load-path         (assoc-ref options :doc-path))
	 (bib-path          (assoc-ref options :bib-path))
	 (source-path       (assoc-ref options :source-path))
	 (image-path        (assoc-ref options :image-path))
         (compat            (assoc-ref options :compat))
	 (preloads          (assoc-ref options :preloads))
	 ;;(variants          '()) ;; FIXME: Implement
         )

    (define user-module
      ;; The environment in which the document is evaluated.
      (make-user-module (string->symbol compat)))


    (if (> (*debug*) 4)
	(set! %load-hook
	      (lambda (file)
		(format #t "~~ loading `~a'...~%" file))))


    (parameterize ((*document-reader*   (make-reader reader-name))
		   (*current-engine*    engine)
                   (*ref-base*          ref-base)
		   (*document-path*     load-path)
		   (*bib-path*          bib-path)
		   (*source-path*       source-path)
		   (*image-path*        image-path)
		   (*debug*             debugging-level)
                   (*debug-use-colors?* color?)
                   (*watched-symbols*   watched-symbols)
		   (*warning*           warning-level)
		   (*verbose*           verbosity-level))

      ;; Load the user rc file (FIXME)
      ;;(load-rc)

      ;; Evaluate expressions passed as `--eval'.
      (for-each (lambda (expr)
                  (eval expr user-module))
                (or expressions '()))

      ;; Load files passed as `--preload' using the default reader.
      (for-each (lambda (f)
                  (save-module-excursion
                   (lambda ()
                     (load f))))
		(or preloads '()))

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
                (doskribe user-module)))
            (doskribe user-module))

        ;; Make sure the output port is flushed before we leave.
        (force-output (*skribilo-output-port*))))))

;;; skribilo ends here.
