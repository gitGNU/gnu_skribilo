;;; module.scm  --  Execution environment for Skribilo documents.
;;; -*- coding: iso-8859-1 -*-
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

(define-module (skribilo module)
  :use-module (srfi srfi-1)
  :use-module (srfi srfi-39)
  :use-module (skribilo utils syntax)
  :export (make-user-module user-module-flavor
           *skribilo-user-module*))

(skribilo-module-syntax)

;;; Author:  Ludovic Courtès
;;;
;;; Commentary:
;;;
;;; This module provides facilities to create run-time modules in which
;;; Skribilo documents (or legacy Skribe documents) are to be executed.
;;;
;;; Code:

(define %skribilo-user-imports
  ;; List of modules needed by Skribilo modules.  The list is much shorter
  ;; than in the Skribe case, as we want to only provide the minimal set of
  ;; bindings that documents need.
  '((skribilo package base)))

(define %skribe-user-imports
  ;; List of modules imported by legacy Skribe documents.
  '((srfi srfi-1)         ;; lists
    (srfi srfi-13)        ;; strings
    (ice-9 optargs)       ;; `define*'

    (skribilo package base) ;; the core markups
    (skribilo utils syntax) ;; `unless', `when', etc.
    (skribilo utils compat) ;; `skribe-load-path', etc.
    (skribilo utils keywords) ;; `the-body', `the-options'
    (skribilo utils strings)  ;; `make-string-replace', etc.
    (skribilo module)
    (skribilo ast)        ;; `<document>', `document?', etc.
    (skribilo config)
    (skribilo biblio)
    (skribilo lib)        ;; `define-markup', `unwind-protect', etc.
    (skribilo resolve)
    (skribilo engine)
    (skribilo writer)
    (skribilo output)
    (skribilo evaluator)
    (skribilo debug)
    (skribilo location)
    ))

(define %skribe-user-autoloads
  ;; List of auxiliary modules be lazily autoloaded by legacy Skribe documents.
  '(((skribilo engine lout)   . (!lout lout-illustration))
    ((skribilo engine latex)  . (!latex LaTeX TeX))
    ((skribilo engine html)   . (html-markup-class html-class
				 html-width))
    ((skribilo utils images)  . (convert-image))
    ((skribilo index)         . (index? make-index-table default-index
                                 resolve-the-index))
    ((skribilo source)        . (source-read-lines source-fontify
				 language? language-extractor
				 language-fontifier source-fontify))
    ((skribilo source lisp)   . (skribe scheme stklos bigloo lisp))
    ((skribilo source xml)    . (xml))
    ((skribilo source c)      . (c java))
    ((skribilo prog)          . (make-prog-body resolve-line))
    ((skribilo color) .
     (skribe-color->rgb skribe-get-used-colors skribe-use-color!))
    ((skribilo sui)           . (load-sui))

    ((ice-9 and-let-star)     . (and-let*))
    ((ice-9 receive)          . (receive))))


;;;
;;; Run-time document modules.
;;;

(define maybe-set-module-name!
  ;; In Guile 2.x, `psyntax' expect module names to be actually bound using
  ;; `nested-define!' (see the definition of `module-name' in `boot-9.scm').
  ;; Thus, if we want to change a custom module name, we should also do the
  ;; `nested-define!' magic.  Failing to do that results in errors like:
  ;;
  ;;   <unnamed port>: In expression (use-modules (foo)):
  ;;   <unnamed port>: Wrong number of arguments to #<program 8868a0 (y)>
  ;;
  ;; (Observed on Guile 1.9.5.)
  ;;
  ;; Consequently, on Guile 2.x, we just let Guile choose a module name and
  ;; do the right thing.
  (cond-expand ((not guile-2) set-module-name!)
               (else          (lambda (m n) #t))))

(define (make-skribe-user-module)
  "Return a new module that imports all the necessary bindings required for
execution of legacy Skribe code/documents."
  (let* ((the-module (make-module))
         (autoloads (map (lambda (name+bindings)
                           (make-autoload-interface the-module
                                                    (car name+bindings)
                                                    (cdr name+bindings)))
                         %skribe-user-autoloads)))
    (maybe-set-module-name! the-module '(skribe-user))
    (module-use-interfaces! the-module
                            (cons the-scm-module
                                  (append (map resolve-interface
                                               %skribe-user-imports)
                                          autoloads)))
    the-module))

(define (make-skribilo-user-module)
  "Return a new module that imports all the necessary bindings required for
Skribilo documents."
  (let* ((the-module (make-module)))
    (maybe-set-module-name! the-module '(skribilo-user))
    (module-use-interfaces! the-module
                            (cons the-scm-module
                                  (map resolve-interface
                                       %skribilo-user-imports)))
    the-module))


;;;
;;; Public interface.
;;;

(define (make-user-module flavor . args)
  "Return a new user module of type @var{flavor}, for use as an execution
environment for Skribilo document.  @var{flavor} should be a symbol, e.g.,
@code{skribilo} for the default execution environment or @code{skribe} for
a Skribe-compatible environment."
  (case flavor
    ((skribe skribe-1.2)
     (make-skribe-user-module))
    ((skribilo)
     (make-skribilo-user-module))
    (else
     (error "unrecognized user module type" flavor))))

(define (user-module-flavor . args)
  "Return a symbol denoting the kind of user module that is passed, e.g.,
@code{'skribilo}."
  (let ((module (if (null? args)
                    (*skribilo-user-module*)
                    (car args))))
    (case (module-name module)
      ((skribe-user)    'skribe)
      ((skribilo-user)  'skribilo)
      (else             #f))))

;; The current module in which the document is evaluated.
(define *skribilo-user-module*
  (make-parameter (make-user-module 'skribilo)))


;;; module.scm ends here
