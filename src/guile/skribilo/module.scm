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
  :autoload   (skribilo reader) (make-reader)
  :use-module (skribilo debug)
  :use-module (system reader confinement) ;; `set-current-reader'
  :use-module (srfi srfi-1)
  :use-module (ice-9 optargs)
  :use-module (skribilo utils syntax))

(set-current-reader %skribilo-module-reader)

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

    (skribilo utils syntax) ;; `unless', `when', etc.
    (skribilo utils compat) ;; `skribe-load-path', etc.
    (skribilo module)
    (skribilo ast)        ;; `<document>', `document?', etc.
    (skribilo config)
    (skribilo runtime)    ;; `the-options', `the-body', `make-string-replace'
    (skribilo biblio)
    (skribilo lib)        ;; `define-markup', `unwind-protect', etc.
    (skribilo resolve)
    (skribilo engine)
    (skribilo writer)
    (skribilo output)
    (skribilo evaluator)
    (skribilo debug)
    ))

(define %skribilo-user-autoloads
  ;; List of auxiliary modules that may be lazily autoloaded.
  '(((skribilo engine lout)   . (lout-illustration
				 ;; FIXME: The following should eventually be
				 ;;        removed from here.
				 lout-structure-number-string))
    ((skribilo source)        . (source-read-lines source-fontify
				 language? language-extractor
				 language-fontifier source-fontify))
    ((skribilo coloring lisp) . (skribe scheme lisp))
    ((skribilo coloring xml)  . (xml))
    ((skribilo prog)          . (make-prog-body resolve-line))
    ((skribilo color) .
     (skribe-color->rgb skribe-get-used-colors skribe-use-color!))

    ((ice-9 and-let-star)     . (and-let*))
    ((ice-9 receive)          . (receive))))

(define %skribe-core-modules
  '("utils" "api" "bib" "index" "param" "sui"))



;; The very macro to turn a legacy Skribe file (which uses Skribe's syntax)
;; into a Guile module.

(define-macro (define-skribe-module name . options)
  `(begin
     (define-module ,name
       :use-module ((skribilo reader) :select (%default-reader))
       :use-module (system reader confinement)
       :use-module (srfi srfi-1)
       ,@(append-map (lambda (mod)
		       (list :autoload (car mod) (cdr mod)))
		     %skribilo-user-autoloads)
       ,@options)

     ;; Pull all the bindings that Skribe code may expect, plus those needed
     ;; to actually create and read the module.
     ;; TODO: These should be auto-loaded.
     ,(cons 'use-modules
	    (append %skribilo-user-imports
		    (filter-map (lambda (mod)
				  (let ((m `(skribilo skribe
						      ,(string->symbol
							mod))))
				    (and (not (equal? m name)) m)))
				%skribe-core-modules)))

     ;; Change the current reader to a Skribe-compatible reader.  If this
     ;; primitive is not provided by Guile, it should be provided by the
     ;; `confinement' module (version 0.2 and later).
     (set-current-reader %default-reader)))


;; Make it available to the top-level module.
(module-define! the-root-module
                'define-skribe-module define-skribe-module)




(define %skribilo-user-module #f)

;;;
;;; MAKE-RUN-TIME-MODULE
;;;
(define-public (make-run-time-module)
  "Return a new module that imports all the necessary bindings required for
execution of Skribilo/Skribe code."
  (let ((the-module (make-module)))
        (for-each (lambda (iface)
                    (module-use! the-module (resolve-module iface)))
                  (append %skribilo-user-imports
			  (map (lambda (mod)
				 `(skribilo skribe
					    ,(string->symbol mod)))
			       %skribe-core-modules)))
        (set-module-name! the-module '(skribilo-user))
        the-module))

;;;
;;; RUN-TIME-MODULE
;;;
(define-public (run-time-module)
  "Return the default instance of a Skribilo/Skribe run-time module."
  (if (not %skribilo-user-module)
      (set! %skribilo-user-module (make-run-time-module)))
  %skribilo-user-module)


;;; module.scm ends here
