;;; module.scm  --  Integration of Skribe code as Guile modules.
;;;
;;; Copyright 2005  Ludovic Courtès <ludovic.courtes@laas.fr>
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
;;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
;;; USA.

(define-module (skribilo module)
  :use-module (skribilo reader)
  :use-module (skribilo evaluator)
  :use-module (skribilo debug)
  :use-module (srfi srfi-1)
  :use-module (ice-9 optargs))

;;; Author:  Ludovic Courtès
;;;
;;; Commentary:
;;;
;;; This (fake) module defines a macro called `define-skribe-module' which
;;; allows to package Skribe code (which uses Skribe built-ins and most
;;; importantly a Skribe syntax) as a Guile module.  This module
;;; automatically exports the macro as a core binding so that future
;;; `use-modules' referring to Skribe modules will work as expected.
;;;
;;; Code:

(define *skribilo-user-imports*
  ;; List of modules that should be imported by any good Skribilo module.
  '((srfi srfi-1)         ;; lists
    (srfi srfi-13)        ;; strings
    ;(srfi srfi-19)        ;; date and time
    (oop goops)           ;; `make'
    (ice-9 optargs)       ;; `define*'
    (ice-9 and-let-star)  ;; `and-let*'
    (ice-9 receive)       ;; `receive'

    (skribilo module)
    (skribilo types)      ;; `<document>', `document?', etc.
    (skribilo config)
    (skribilo vars)
    (skribilo runtime)    ;; `the-options', `the-body'
    (skribilo biblio)
    (skribilo lib)        ;; `define-markup', `unwind-protect', etc.
    (skribilo resolve)
    (skribilo engine)
    (skribilo writer)
    (skribilo output)
    (skribilo evaluator)
    (skribilo color)
    (skribilo debug)))

(define *skribe-core-modules*
  '("utils" "api" "bib" "index" "param" "sui"))

(define-macro (define-skribe-module name . options)
  `(begin
     (define-module ,name
       #:reader (make-reader 'skribe)
       #:use-module (skribilo reader)
       ,@options)

     ;; Pull all the bindings that Skribe code may expect, plus those needed
     ;; to actually create and read the module.
     ,(cons 'use-modules
	    (append *skribilo-user-imports*
		    (filter-map (lambda (mod)
				  (let ((m `(skribilo skribe
						      ,(string->symbol
							mod))))
				    (and (not (equal? m name)) m)))
				*skribe-core-modules*)))))


;; Make it available to the top-level module.
(module-define! the-root-module
                'define-skribe-module define-skribe-module)




(define *skribilo-user-module* #f)

;;;
;;; MAKE-RUN-TIME-MODULE
;;;
(define-public (make-run-time-module)
  "Return a new module that imports all the necessary bindings required for
execution of Skribilo/Skribe code."
  (let ((the-module (make-module)))
        (for-each (lambda (iface)
                    (module-use! the-module (resolve-module iface)))
                  (append *skribilo-user-imports*
			  (map (lambda (mod)
				 `(skribilo skribe
					    ,(string->symbol mod)))
			       *skribe-core-modules*)))
        (set-module-name! the-module '(skribilo-user))
        the-module))

;;;
;;; RUN-TIME-MODULE
;;;
(define-public (run-time-module)
  "Return the default instance of a Skribilo/Skribe run-time module."
  (if (not *skribilo-user-module*)
      (set! *skribilo-user-module* (make-run-time-module)))
  *skribilo-user-module*)


;; FIXME:  This will eventually be replaced by the per-module reader thing in
;;         Guile.
(define-public (load-file-with-read file read module)
  (with-debug 5 'load-file-with-read
     (debug-item "loading " file)

     (with-input-from-file (search-path %load-path file)
       (lambda ()
;      (format #t "load-file-with-read: ~a~%" read)
	 (let loop ((sexp (read))
		    (result #f))
	   (if (eof-object? sexp)
	       result
	       (begin
;              (format #t "preparing to evaluate `~a'~%" sexp)
		 (loop (read)
		       (primitive-eval sexp)))))))))

(define-public (load-skribilo-file file reader-name)
  (load-file-with-read file (make-reader reader-name) (current-module)))

(define*-public (load-skribe-modules #:optional (debug? #f))
  "Load the core Skribe modules, both in the @code{(skribilo skribe)}
hierarchy and in @code{(run-time-module)}."
  (for-each (lambda (mod)
              (format #t "~~ loading skribe module `~a'...~%" mod)
              (load-skribilo-file (string-append "skribilo/skribe/"
                                                 mod ".scm")
                                  'skribe)
              (module-use! (run-time-module)
                           (resolve-module `(skribilo skribe
                                             ,(string->symbol mod)))))
            *skribe-core-modules*))


;;; module.scm ends here
