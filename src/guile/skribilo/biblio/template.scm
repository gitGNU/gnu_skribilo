;;; template.scm  --  Template system for bibliography entries.
;;;
;;; Copyright 2003, 2004  Manuel Serrano
;;; Copyright 2006, 2007  Ludovic Courtès <ludovic.courtes@laas.fr>
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

(define-module (skribilo biblio template)
  :use-module (skribilo ast)
  :autoload   (skribilo lib)    (skribe-error)
  :autoload   (skribilo output) (output)

  :use-module (ice-9 optargs)

  :use-module (skribilo utils syntax)

  :export (output-bib-entry-template
           make-bib-entry-template/default
           make-bib-entry-template/skribe))

(fluid-set! current-reader %skribilo-module-reader)

;;; Author: Manuel Serrano, Ludovic Courtès
;;;
;;; Commentary:
;;;
;;; This module provides a helper procedure to output bibliography entries
;;; according to a given template, as well as ready-to-use templates.  A
;;; template only contains part of the style information for a bibliography
;;; entry.  Specific style information can be added by modifying the markup
;;; writers for `&bib-entry-author', `&bib-entry-title', etc. (see `(skribilo
;;; package base)' for details).
;;;
;;; Code:


;;;
;;; Outputting a bibliography entry template for a specific entry.
;;;

(define* (output-bib-entry-template bib engine template
                                    :optional (get-field markup-option))
  ;; Output the fields of BIB (a bibliography entry) for ENGINE according to
  ;; TEMPLATE.  Example of templates are found below (e.g.,
  ;; `make-bib-entry-template/default').
  (let loop ((template template)
             (pending #f)
             (armed #f))
    (cond
     ((null? template)
      'done)
     ((pair? (car template))
      (if (eq? (caar template) 'or)
          (let ((o1 (cadr (car template))))
            (if (get-field bib o1)
                (loop (cons o1 (cdr template))
                      pending
                      #t)
                (let ((o2 (caddr (car template))))
                  (loop (cons o2 (cdr template))
                        pending
                        armed))))
          (let ((o (get-field bib (cadr (car template)))))
            (if o
                (begin
                  (if (and pending armed)
                      (output pending engine))
                  (output (caar template) engine)
                  (output o engine)
                  (if (pair? (cddr (car template)))
                      (output (caddr (car template)) engine))
                  (loop (cdr template) #f #t))
                (loop (cdr template) pending armed)))))
     ((symbol? (car template))
      (let ((o (get-field bib (car template))))
        (if o
            (begin
              (if (and armed pending)
                  (output pending engine))
              (output o engine)
              (loop (cdr template) #f #t))
            (loop (cdr template) pending armed))))
     ((null? (cdr template))
      (output (car template) engine))
     ((string? (car template))
      (loop (cdr template)
            (if pending pending (car template))
            armed))
     (else
      (skribe-error 'output-bib-fields
                    "Illegal templateiption"
                    (car template))))))


;;;
;;; Example bibliography entry templates.
;;;

(define (make-bib-entry-template/default kind)
  ;; The default bibliography entry template.
  (case kind
    ((techreport)
     `(author ". " (or title url documenturl) ". "
              ,(_ "Technical Report") " " number ", " institution ", "
              address ", " month " " year ", "
              ("pp. " pages) "."))
    ((article)
     `(author ". " (or title url documenturl) ". "
              "In " journal ", " volume
              ("(" number ")") ", "
              address ", " month " " year ", "
              ("pp. " pages) "."))
    ((inproceedings)
     `(author ". " (or title url documenturl) ". "
              "In " booktitle ", "
              (series ", ")
              ("(" number ")")
              ("pp. " pages ", ")
              ("" publisher ", ")
              ;; FIXME:  Addr., month.
              year "."))
    ((book) ;; FIXME:  Title should be in italics
     '(author ". " (or title url documenturl) ". "
              publisher ", " address
              ", " month " " year ", "
              ("pp. " pages) "."))
    ((phdthesis)
     '(author ". " (or title url documenturl)
              ". " type ", "
              school ", " address
              ", " month " " year"."))
    ((misc)
     '(author ". " (or title url documenturl) ". "
              publisher ", " address
              ", " month " " year
              (", " url) "."))
    (else
     '(author ". " (or title url documenturl) ". "
              publisher ", " address
              ", " month " " year ", "
              ("pp. " pages) "."))))

(define (make-bib-entry-template/skribe kind)
  ;; The awful template found by default in Skribe.
  (case kind
    ((techreport)
     `(author " -- " (or title url documenturl) " -- "
              ,(_ "Technical Report") " " number ", " institution ", "
              address ", " month ", " year ", "
              ("pp. " pages) "."))
    ((article)
     `(author " -- " (or title url documenturl) " -- "
              journal ", " volume "" ("(" number ")") ", "
              address ", " month ", " year ", "
              ("pp. " pages) "."))
    ((inproceedings)
     `(author " -- " (or title url documenturl) " -- "
              booktitle ", " series ", " ("(" number ")") ", "
              address ", " month ", " year ", "
              ("pp. " pages) "."))
    ((book)
     '(author " -- " (or title url documenturl) " -- "
              publisher ", " address
              ", " month ", " year ", " ("pp. " pages) "."))
    ((phdthesis)
     '(author " -- " (or title url documenturl) " -- " type ", "
              school ", " address
              ", " month ", " year"."))
    ((misc)
     '(author " -- " (or title url documenturl) " -- "
              publisher ", " address
              ", " month ", " year"."))
    (else
     '(author " -- " (or title url documenturl) " -- "
              publisher ", " address
              ", " month ", " year ", " ("pp. " pages) "."))))


;;; arch-tag: 5931579f-b606-442d-9a45-6047c94da5a2

;;; template.scm ends here
