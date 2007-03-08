;;; diff.scm  --  A document difference highlighting package.
;;;
;;; Copyright 2007  Ludovic Courtès <ludovic.courtes@laas.fr>
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

(define-module (skribilo package diff)
  :use-module (differ)
  :use-module (srfi srfi-1)
  :use-module (srfi srfi-39)
  :use-module (ice-9 optargs)

  :use-module (skribilo ast)
  :use-module (skribilo lib)
  :autoload   (skribilo reader)        (*document-reader*)
  :autoload   (skribilo engine)        (*current-engine*)
  :autoload   (skribilo module)        (make-run-time-module)
  :autoload   (skribilo resolve)       (resolve!)
  :autoload   (skribilo evaluator)     (evaluate-ast-from-port)
  :autoload   (skribilo biblio)        (*bib-table* make-bib-table)
  :use-module (skribilo package base)
  :use-module (skribilo utils syntax)

  :export (make-diff-document-from-files))

(fluid-set! current-reader %skribilo-module-reader)

;;; Author: Ludovic Courtès
;;;
;;; Commentary:
;;;
;;; This package provides facilities to automatically produce documents where
;;; changes from a previous version of the document are highlighted.
;;;
;;; Warning: This is very experimental at this stage!
;;;
;;; Code:



;;;
;;; Markup.
;;;

(define-markup (deletion :rest args)
  (color :fg "red" "[deletion]"))

(define-markup (insertion :rest args)
  (color :fg "green" args))

(define-markup (unchanged :rest args)
  args)


;;;
;;; Helpers for string diffs.
;;;

(define (coalesce-edits edits)
  ;; Coalesce EDITS (an array of edits as returned by `diff:edits') into a
  ;; list of contiguous changes, each change being denoted by `(CHANGE-KIND
  ;; START END)' where CHANGE-KIND is one of `deletion', `insertion' or
  ;; `replacement'.
  (define (do-coalesce edit-kind edit result)
    (cond ((null? result)
           `((,edit-kind ,edit ,edit)))
          ((eq? (caar result) edit-kind)
           (let ((start (cadr  (car result)))
                 (end   (caddr (car result))))
             (if (= edit (+ end 1))
                 (cons `(,edit-kind ,start ,edit)
                       (cdr result))
                 (cons `(,edit-kind ,edit ,edit)
                       result))))
          (else
           (let ((start (cadr  (car result)))
                 (end   (caddr (car result))))
             (if (and (= start end edit)
                      (not (eq? (caar result) 'replacement)))
                 (do-coalesce 'replacement edit (cdr result))
                 (cons `(,edit-kind ,edit ,edit)
                       result))))))

  (reverse! (fold (lambda (edit result)
                    (if (negative? edit)
                        (let ((edit (- -1 edit)))
                          (do-coalesce 'deletion edit result))
                        (let ((edit (- edit 1)))
                          (do-coalesce 'insertion edit result))))
                  '()
                  (array->list edits))))

(define (add-unchanged edits str-len)
  ;; Add information about unchanged regions to EDITS, a list returned by
  ;; `coalesce-edits'.  STR-LEN should be the length of the _target_ string,
  ;; i.e., the second argument of `diff:edits'.
  (define %nop '(unchanged 0 0))
  (define (strip-nop result)
    (if (equal? (car result) %nop)
        (cdr result)
        result))

  (let loop ((edits   edits)
             (result  (list (list 'unchanged 0 0)))
             (str-pos 0))
    (if (null? edits)
        (strip-nop
         (reverse! (if (< str-pos (- str-len 1))
                       (cons (list 'unchanged str-pos (- str-len 1))
                             result)
                       result)))
        (let* ((change (car edits))
               (kind  (car change))
               (start (cadr change))
               (end   (caddr change))

               (prev-unchanged? (eq? (caar result) 'unchanged))
               (prev-start      (cadr (car result)))
               (prev-end        (caddr (car result))))

          (loop (cdr edits)
                (if (memq kind '(insertion replacement))
                    (if (> start (+ 1 prev-end))
                        (if prev-unchanged?
                            (cons* change
                                   `(unchanged ,prev-start
                                               ,(- start 1))
                                   (cdr result))
                            (cons* change
                                   `(unchanged ,(+ 1 prev-end)
                                               ,(- start 1))
                                   result))
                        (cons change result))
                    (cons change result))
                (+ end 1))))))

(define (string-diff-sequences str1 str2)
  ;; Return a "diff sequence" between STR1 and STR2.  The diff sequence is
  ;; alist of 3-element list whose car represent a diff type (a symbol,
  ;; either `unchanged', `replacement', `insertion', or `deletion') and two
  ;; integers denoting where the change took place.  These two integers are
  ;; an indices in STR1 in the case of `deletion', indices in STR2 otherwise.
  (add-unchanged (coalesce-edits (diff:edits str1 str2))
                 (string-length str2)))



;;;
;;; AST diffing.
;;;

(define %undiffable-markups
  ;; List of markups to not diff.
  '(ref url-ref bib-ref bib-ref+ line-ref unref
    figref ;; non-standard
    mark
    image symbol lout-illustration
    &the-bibliography
    toc
    index &index-entry &the-index &the-index-header))

(define (make-diff-document ast1 ast2)
  ;; Return a document based on AST2 that highlights differences between AST1
  ;; and AST2, enclosing unchanged parts in `unchanged' markups, etc.
  (let loop ((ast1 ast1)
             (ast2 ast2))
    ;;(format (current-error-port) "diff: ~a ~a~%" ast1 ast2)
    (cond ((string? ast2)
           (if (string? ast1)
               (reverse!
                (fold (lambda (edit result)
                        (let ((start (cadr edit))
                              (end   (+ 1 (caddr edit))))
                          (cons (case (car edit)
                                  ((insertion)
                                   (insertion (substring ast2 start end)))
                                  ((deletion)
                                   (deletion  (substring ast1 start end)))
                                  ((replacement)
                                   (replacement (substring ast2 start end)))
                                  ((unchanged)
                                   (unchanged (substring ast2 start end))))
                                result)))
                      '()
                      (string-diff-sequences ast1 ast2)))
               (insertion ast2)))

          ((document? ast2)
           (let ((ident (or (markup-ident ast2)
                            (ast->string (markup-option ast2 :title))
                            (symbol->string (gensym "document"))))
                 (opts  (markup-options ast2))
                 (class (markup-class ast2))
                 (body  (markup-body ast2)))
             (new document
                  (markup 'document)
                  (ident ident)
                  (class class)
                  (options opts)
                  (body (loop (if (markup? ast1)
                                  (markup-body ast1)
                                  ast1)
                              body))
                  (env (list (list 'chapter-counter 0) (list 'chapter-env '())
                             (list 'section-counter 0) (list 'section-env '())
                             (list 'footnote-counter 0)
                             (list 'footnote-env '())
                             (list 'figure-counter 0)
                             (list 'figure-env '()))))))

          ((container? ast2)
           (let ((kind  (markup-markup ast2))
                 (ident (markup-ident ast2))
                 (opts  (markup-options ast2))
                 (class (markup-class ast2))
                 (body  (markup-body ast2)))
             (new container
                  (markup  kind)
                  (ident   ident)
                  (class   class)
                  (options opts)
                  (body (loop (if (markup? ast1)
                                  (markup-body ast1)
                                  ast1)
                              body)))))

          ((markup? ast2)
           (let ((kind  (markup-markup ast2))
                 (ident (markup-ident ast2))
                 (opts  (markup-options ast2))
                 (class (markup-class ast2))
                 (body  (markup-body ast2)))
             (new markup
                  (markup  kind)
                  (ident   ident)
                  (class   class)
                  (options opts)
                  (body (if (memq kind %undiffable-markups)
                            body
                            (loop (if (markup? ast1)
                                      (markup-body ast1)
                                      ast1)
                                  body))))))

          ((list? ast2)
           (if (list? ast1)
               (map loop ast1 ast2)
               (map (lambda (x)
                      (loop ast1 x))
                    ast2)))

          (else
           (insertion ast2)))))



;;;
;;; Public API.
;;;

(define* (make-diff-document-from-files old-file new-file
                                        :key (reader (*document-reader*))
                                             (env '())
                                             (engine (*current-engine*)))
  ;; Return a document similar to NEW-FILE, where differences from OLD-FILE
  ;; are highlighted.
  (let ((ast1
         (parameterize ((*bib-table* (make-bib-table 'doc-1)))
           (evaluate-ast-from-port (open-input-file old-file)
                                   :reader reader
                                   :module (make-run-time-module))))
        (~~ (skribe-message "diff: first document loaded"))
        (ast2
         (parameterize ((*bib-table* (make-bib-table 'doc-2)))
           (evaluate-ast-from-port (open-input-file new-file)
                                   :reader reader
                                   :module (make-run-time-module))))
        (%% (skribe-message "diff: second document loaded")))

    (resolve! ast1 engine env)
    (resolve! ast2 engine env)
    (let ((diff (make-diff-document ast1 ast2)))
      (format (current-error-port) "diff-doc: ~a ~a~%"
              diff (document? diff))
      diff)))


;;; diff.scm ends here

;;; arch-tag: 69ad10fa-5688-4835-8956-439e44e26847
