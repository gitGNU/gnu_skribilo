;;; template.scm  --  Template system for bibliography entries.
;;;
;;; Copyright 2003, 2004  Manuel Serrano
;;; Copyright 2006, 2007, 2015 Ludovic Courtès <ludo@gnu.org>
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

(define-module (skribilo biblio template)
  :use-module (srfi srfi-1)
  :use-module (srfi srfi-34)
  :use-module (srfi srfi-35)

  :use-module (skribilo ast)
  :autoload   (skribilo output) (output)
  :use-module (skribilo biblio)

  :use-module (ice-9 optargs)

  :use-module (skribilo utils syntax)

  :export (evaluate-bib-entry-template
           output-bib-entry-template
           make-bib-entry-template/default
           make-bib-entry-template/skribe))

(skribilo-module-syntax)

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

(define (evaluate-bib-entry-template bib template . rest)
  ;; An interpreter for the bibliography template language.  Overview of the
  ;; language:
  ;;
  ;;  form := (cond-list|special-sexp|string|field-spec)
  ;;
  ;;  field-spec   := ("author"|"title"|...)
  ;;  cond-list    := (form+)
  ;;  special-sexp := (("if" form form form?)|("or" form*))
  ;;
  ;; A `cond-list' gets issued only if all its elements are true.

  (define get-field
    (if (null? rest)
        markup-option
        (car rest)))

  (define (eval-cond-list sexp eval-sexp)
    (let loop ((sexp   sexp)
               (result '()))
      (if (null? sexp)
          (reverse! result)
          (let ((head (eval-sexp (car sexp))))
            (if (not head)
                #f
                (loop (cdr sexp)
                      (cons head result)))))))

  (define (eval-special-sexp sexp eval-sexp)
    (let ((special (car sexp))
          (formals (cdr sexp)))
      (case special
        ((or)
         (any eval-sexp formals))
        ((if)
         (if (or (> (length formals) 3)
                 (< (length formals) 2))
             (raise (condition
                     (&biblio-template-error (expression sexp)
                                             (template template)))))
         (let* ((if-cond (car formals))
                (if-then (cadr formals))
                (if-else (if (null? (cddr formals))
                             #f
                             (caddr formals)))
                (result (eval-sexp if-cond)))
           (if result
               (eval-sexp if-then)
               (eval-sexp if-else))))
        (else
         (eval-cond-list sexp eval-sexp)))))

  (let loop ((template template))
    (cond ((symbol? template)
           (get-field bib template))
          ((null? template)
           #f)
          ((pair? template)
           (cond ((symbol? (car template))
                  (eval-special-sexp template loop))
                 (else
                  (eval-cond-list template loop))))
          ((string? template)
           template)
          (else
           (raise (condition
                   (&biblio-template-error (expression template)
                                           (template template))))))))


(define* (output-bib-entry-template bib engine template
                                    :optional (get-field markup-option))
  ;; Output the fields of BIB (a bibliography entry) for ENGINE according to
  ;; TEMPLATE.  Example of templates are found below (e.g.,
  ;; `make-bib-entry-template/default').
  (output (map (lambda (form)
                 (evaluate-bib-entry-template bib form get-field))
               template)
          engine))


;;;
;;; Example bibliography entry templates.
;;;

(define (make-bib-entry-template/default kind)
  ;; The default bibliography entry template.

  (case kind
    ((techreport)
     `(author ". " (or title url documenturl) ". "
              ;; TRANSLATORS: The next few msgids are fragments of
              ;; bibliography items.
              ,(_ "Technical Report") " " number
              (", " institution)
              (", " address)
              (", " month) " " year
              (", pp. " pages) "."))
    ((article)
     '(author ". " (or title url documenturl) ". "
              "In " journal ", " volume
              ("(" number ") ")", "
              (address ", ") month " " year ", "
              ("pp. " pages) "."))
    ((inproceedings)
     '(author ". " (or title url documenturl) ". "
              "In " booktitle ", "
              (series ", ")
              ("(" number ")")
              ("pp. " pages ", ")
              (publisher ", ")
              (month " ") year "."))
    ((book) ;; FIXME:  Title should be in italics
     '((or author editor)
              ". " (or title url documenturl) ". "
              publisher
              (", " address)
              (", " month)
              ", " year
              (", pp. " pages) "."))
    ((inbook)
     `(author ". " (or title url documenturl) ". "
              "In " booktitle ", " publisher
              (", " editor " (" ,(_ "editor") ")")
              (", " ,(_ "Chapter ") chapter)
              (", pp. " pages) ", "
              (month " ") year "."))
    ((phdthesis)
     `(author ". " (or title url documenturl)
              ", " ,(_ "PhD Thesis")
              (", " (or school institution))
              (", " address)
              (", " month)
              (if month " " ", ") year "."))
    ((misc)
     '(author ". " (or title url documenturl) ". "
              (institution ", ")
              (publisher ", ")
              (address ", ")
              (month " ") year ". "
              (url ".")))
    (else
     '(author ". " (or title url documenturl) ". "
              (publisher ", ")
              (address ", ")
              (month " ") year ", "
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
