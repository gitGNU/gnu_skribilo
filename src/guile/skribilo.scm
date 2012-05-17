;;; skribilo.scm  --  The Skribilo document processor.
;;;
;;; Copyright 2005, 2006, 2007, 2008, 2009, 2011, 2012  Ludovic Courtès <ludo@gnu.org>
;;; Copyright 2003, 2004  Erick Gallesio - I3S-CNRS/ESSI <eg@unice.fr>
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

  :use-module  (srfi srfi-1)
  :use-module  (srfi srfi-13)
  :use-module  (srfi srfi-37)
  :use-module  (srfi srfi-39)

  :export (skribilo))


;; Install the Skribilo module syntax reader.
(skribilo-module-syntax)

(if (not (keyword? :kw))
    (error "guile-reader sucks"))



;;;
;;; Help.
;;;

(define (skribilo-show-help)
  (display (_ "Usage: skribilo [OPTIONS] [INPUT]"))
  (display (_ "
Process Skribilo document from file INPUT (or the standard input) using the
specified reader syntax, and produce its output using the specified engine.
"))
  (display (_ "
  -R, --reader=READER    Use READER to parse the input file, e.g., `skribe'
                         (default) or `outline'."))
  (display (_ "
  -t, --target=ENGINE    Use ENGINE as the output engine, e.g., `html'."))
  (display (_ "
  -c, --custom=C=VAL     Use VAL as the value of ENGINE's custom C."))
  (display (_ "
  -o, --output=FILE      Write output to FILE."))
  (display (_ "
      --compat=COMPAT    Use COMPAT as the compatibility layer, e.g., `skribe'."))
  (newline)
  (display (_ "
  -I, --doc-path=DIR     Prepend DIR to the document include path."))
  (display (_ "
  -B, --bib-path=DIR     Prepend DIR to the bibliography include path."))
  (display (_ "
  -S, --source-path=DIR  Prepend DIR to the source include path."))
  (display (_ "
  -P, --image-path=DIR   Prepend DIR to the image include path."))
  (display (_ "
  -U, --sui-path=DIR     Prepend DIR to the Skribe URL Index (SUI) search path."))
  (newline)
  (display (_ "
  -b, --base=BASE        Strip BASE from all hyperlinks (`html' engine)."))
  (display (_ "
  -e, --eval=EXPR        Prepend EXPR to the list of expressions to be
                         evaluted before the input file is processed."))
  (display (_ "
  -p, --preload=FILE     Preload FILE before processing the input file."))
  (newline)
  (display (_ "
  -v, --verbose[=LEVEL]  Be verbose, unless LEVEL is 0."))
  (display (_ "
  -w, --warning[=LEVEL]  Issue warnings, unless LEVEL is 0."))
  (display (_ "
  -g, --debug[=ARG]      Issue debugging output, unless ARG is 0.  If ARG is
                         not a number, it is interpreted as a symbol to be
                         watched."))
  (display (_ "
      --no-color         Disable colored debugging output."))
  (newline)
  (display (_ "
  -h, --help             Give this help list"))
  (display (_ "
  -V, --version          Print program version"))
  (newline)
  (format #t (_ "
Report bugs to <~a>.~%")
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
        (option '(#\c "custom") #t #f
                (lambda (opt name arg result)
                  (let ((=-pos (string-index arg #\=)))
                    (if (not =-pos)
                        (leave (_ "~a: missing value for custom") arg)
                        (let ((custom (string-take arg =-pos))
                              (value  (string-drop arg (+ =-pos 1))))
                          (catch 'read-error
                            (lambda ()
                              (let ((custom (string->symbol custom))
                                    (value
                                     (with-input-from-string value read))
                                    (result
                                     (alist-delete :customs result eq?))
                                    (customs
                                     (assoc-ref result :customs)))
                                (alist-cons
                                 :customs
                                 (alist-cons custom value customs)
                                 result)))
                            (lambda (key . args)
                              (leave (_ "~a: invalid custom value")
                                     value))))))))
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
        (option '(#\U "sui-path") #t #f
                (make-path-processor :sui-path))

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
                  (let ((num (if arg
                                 (string->number arg)
                                 1)))
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
    (:sui-path    ".")
    (:customs)
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
         (customs           (assoc-ref options :customs))
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
         (sui-path          (assoc-ref options :sui-path))
         (compat            (assoc-ref options :compat))
	 (preloads          (assoc-ref options :preloads))
	 ;;(variants          '()) ;; FIXME: Implement
         )

    (define user-module
      ;; The environment in which the document is evaluated.
      (make-user-module (string->symbol compat)))

    ;; Install the user-specified locale.
    (setlocale LC_ALL "")

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
                   (*sui-path*          sui-path)
		   (*debug*             debugging-level)
                   (*debug-use-colors?* color?)
                   (*watched-symbols*   watched-symbols)
		   (*warning*           warning-level)
		   (*verbose*           verbosity-level))

      ;; Load the user rc file (FIXME)
      ;;(load-rc)

      (or (null? customs)
          (let ((engine (lookup-engine engine)))
            (for-each (lambda (custom+value)
                        (let ((custom (car custom+value))
                              (value  (cdr custom+value)))
                          (engine-custom-set! engine custom value)))
                      customs)))

      ;; Evaluate expressions passed as `--eval'.
      (for-each (lambda (expr)
                  (eval expr user-module))
                (or expressions '()))

      ;; Load files passed as `--preload' using the default reader.
      (for-each (lambda (f)
                  (save-module-excursion
                   (lambda ()
                     (set-current-module user-module)
                     (load f))))
		(or preloads '()))

      ;; Load the specified variants. (FIXME)
;;       (for-each (lambda (x)
;; 		  (skribe-load (format #f "~a.skr" x)
;; 			       :engine (*current-engine*)))
;; 		(reverse! variants))

      (if (and output-file (file-exists? output-file))
          (delete-file output-file))

      ;; Choose UTF-8 as the default encoding so that string ports will
      ;; accept all of Unicode.
      (default-to-utf-8
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
                  (set-correct-file-encoding!)
                  (doskribe user-module)))
              (doskribe user-module))

          ;; Make sure the output port is flushed before we leave.
          (force-output (*skribilo-output-port*)))))))


;;; Local Variables:
;;; coding: latin-1
;;; End:

;;; skribilo ends here.
