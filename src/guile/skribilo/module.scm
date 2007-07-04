;;; module.scm  --  Integration of Skribe code as Guile modules.
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

(define-module (skribilo module)
  :use-module (srfi srfi-1)
  :use-module (srfi srfi-39)
  :use-module (skribilo utils syntax)
  :export (make-run-time-module *skribilo-user-module*))

(fluid-set! current-reader %skribilo-module-reader)

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

(define %skribilo-user-imports
  ;; List of modules that should be imported by any good Skribilo module.
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

(define %skribilo-user-autoloads
  ;; List of auxiliary modules that may be lazily autoloaded.
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



;; The very macro to turn a legacy Skribe file (which uses Skribe's syntax)
;; into a Guile module.

(define-macro (define-skribe-module name . options)
  `(begin
     (define-module ,name
       :use-module ((skribilo reader) :select (%default-reader))
       :use-module (srfi srfi-1)
       ,@(append-map (lambda (mod)
		       (list :autoload (car mod) (cdr mod)))
		     %skribilo-user-autoloads)
       ,@options)

     ;; Pull all the bindings that Skribe code may expect, plus those needed
     ;; to actually create and read the module.
     ;; TODO: These should be auto-loaded.
     ,(cons 'use-modules %skribilo-user-imports)

     ;; Change the current reader to a Skribe-compatible reader.  If this
     ;; primitive is not provided by Guile (i.e., version <= 1.7.2), then it
     ;; should be provided by `guile-reader' (version >= 0.3) as a core
     ;; binding and installed by `(skribilo utils syntax)'.
     (fluid-set! current-reader %default-reader)))


;; Make it available to the top-level module.
(module-define! the-root-module
                'define-skribe-module define-skribe-module)




;;;
;;; MAKE-RUN-TIME-MODULE
;;;
(define (make-run-time-module)
  "Return a new module that imports all the necessary bindings required for
execution of Skribilo/Skribe code."
  (let* ((the-module (make-module))
         (autoloads (map (lambda (name+bindings)
                           (make-autoload-interface the-module
                                                    (car name+bindings)
                                                    (cdr name+bindings)))
                         %skribilo-user-autoloads)))
    (set-module-name! the-module '(skribilo-user))
    (module-use-interfaces! the-module
                            (cons the-root-module
                                  (append (map resolve-interface
                                               %skribilo-user-imports)
                                          autoloads)))
    the-module))

;; The current module in which the document is evaluated.
(define *skribilo-user-module* (make-parameter (make-run-time-module)))


;;; module.scm ends here
