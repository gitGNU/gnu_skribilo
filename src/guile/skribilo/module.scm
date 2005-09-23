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
  :use-module (skribilo eval)
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

(define-macro (define-skribe-module name)
  `(begin
     (define-module ,name)

     ;; Pull all the bindings that Skribe code may expect, plus those needed
     ;; to actually create and read the module.
     (use-modules (skribilo module)
                  (skribilo reader)
                  (skribilo eval)   ;; `run-time-module'

                  (srfi srfi-1)
                  (ice-9 optargs)

                  (skribilo lib) ;; `define-markup', `unwind-protect', etc.
                  (skribilo runtime)
                  (skribilo vars)
                  (skribilo config))

     (use-syntax (skribilo lib))

     ;; The `define' below results in a module-local definition.  So the
     ;; definition of `read' in the `(guile-user)' module is left untouched.
     ;(define read ,(make-reader 'skribe))

     ;; Everything is exported.
     (define-macro (define . things)
       (let* ((first (car things))
              (binding (cond ((symbol? first) first)
                             ((list? first)   (car first))
                             ((pair? first)   (car first))
                             (else
                              (error "define/skribe: bad formals" first)))))
         `(begin
            (define-public ,@things)
            ;; Automatically push it to the run-time user module.
;             (module-define! ,(run-time-module)
;                             (quote ,binding) ,binding)
            )))))


;; Make it available to the top-level module.
(module-define! the-root-module
                'define-skribe-module define-skribe-module)


(define-public (load-file-with-read file read module)
  (with-input-from-file file
    (lambda ()
;      (format #t "load-file-with-read: ~a~%" read)
      (let loop ((sexp (read))
                 (result #f))
        (if (eof-object? sexp)
            result
            (begin
;              (format #t "preparing to evaluate `~a'~%" sexp)
              (loop (read)
                    (eval sexp module))))))))

(define-public (load-skribilo-file file reader-name)
  (load-file-with-read file (make-reader reader-name) (current-module)))

(define-public *skribe-core-modules*
  '("utils" "api" "bib" "index" "param" "sui"))

(define*-public (load-skribe-modules #:optional (debug? #f))
  "Load the core Skribe modules, both in the @code{(skribilo skribe)}
hierarchy and in @code{(run-time-module)}."
  (for-each (lambda (mod)
              (if debug?
                  (format #t "loading skribe module `~a'...~%" mod))
              (load-skribilo-file (string-append "skribe/" mod ".scm")
                                  'skribe))
            *skribe-core-modules*)
  (for-each (lambda (mod)
              (module-use! (run-time-module)
                           (resolve-interface (list skribilo skribe
                                                    (string->symbol
                                                     mod)))))
            *skribe-core-modules*))

;;; module.scm ends here
