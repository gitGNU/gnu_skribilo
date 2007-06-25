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
  :use-module (diff)
  :use-module (srfi srfi-1)
  :use-module (srfi srfi-14)
  :use-module (srfi srfi-39)
  :use-module (ice-9 optargs)

  :use-module (skribilo ast)
  :use-module (skribilo lib)
  :use-module (skribilo engine)
  :use-module (skribilo writer)
  :autoload   (skribilo output)        (output)
  :autoload   (skribilo reader)        (*document-reader*)
  :autoload   (skribilo module)        (make-run-time-module)
  :autoload   (skribilo resolve)       (resolve!)
  :autoload   (skribilo evaluator)     (evaluate-ast-from-port)
  :autoload   (skribilo biblio)        (*bib-table* make-bib-table)
  :use-module (skribilo package base)
  :use-module (skribilo utils syntax)

  :export (make-diff-document
           make-diff-document-from-files))

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

(define-markup (deletion :rest args :key loc)
  (new markup
       (markup 'diff:deletion)
       (ident  (gensym "diff:deletion"))
       (loc    (or loc &invocation-location))
       (body   args)))

(define-markup (insertion :rest args :key loc)
  (new markup
       (markup 'diff:insertion)
       (ident  (gensym "diff:insertion"))
       (loc    (or loc &invocation-location))
       (body   args)))

(define-markup (replacement :rest args :key loc)
  (new markup
       (markup 'diff:replacement)
       (ident  (gensym "diff:replacement"))
       (loc    (or loc &invocation-location))
       (body   args)))

(define-markup (unchanged :rest args :key loc)
  (new markup
       (markup 'diff:unchanged)
       (ident  (gensym "diff:unchanged"))
       (loc    (or loc &invocation-location))
       (body   args)))



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
  (define (coalesce-unchanged start end result)
    (if (null? result)
        `((unchanged ,start ,end))
        (let ((prev-unchanged? (eq? (caar result) 'unchanged))
              (prev-start      (cadr (car result))))
          (if prev-unchanged?
              (cons `(unchanged ,prev-start ,end)
                    (cdr result))
              (cons `(unchanged ,start ,end)
                    result)))))

  (let loop ((edits   edits)
             (result  '())
             (str-pos 0))
    (if (null? edits)
        (reverse! (if (< str-pos str-len)
                      (cons (list 'unchanged str-pos (- str-len 1))
                            result)
                      result))
        (let* ((change (car edits))
               (kind  (car change))
               (start (cadr change))
               (end   (caddr change)))

          (loop (cdr edits)
                (if (memq kind '(insertion replacement))
                    (if (> start str-pos)
                        (cons change
                              (coalesce-unchanged str-pos (- start 1)
                                                  result))
                        (cons change result))
                    (cons change result))
                (if (eq? kind 'deletion)
                    str-pos ;; deletion doesn't change string position
                    (+ end 1)))))))

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
  '(ref url-ref bib-ref bib-ref+ line-ref unref numref
    eq     ;; XXX: not supported because of the `eq-evaluate' thing
    figref ;; non-standard
    mark
    image symbol lout-illustration
    &the-bibliography
    toc
    index &index-entry &the-index &the-index-header))

(define %diffable-options
  ;; List of diffable options.
  '(:title :text))

(define (annotated-string-diff str1 str2)
  ;; Return a list (actually an AST) denoting the differences between STR1
  ;; and STR2.  The returned text is actually that of STR2 augmented with
  ;; `insertion', `deletion', `replacement', and `unchanged' markup.

  (define (space-preserving-substring str start end)
    ;; Return the substring of STR, possibly inserting unbreakable spaces at
    ;; the beginning/end if STR starts/ends in whitespaces.
    (let ((len (- end start)))
      (if (> len 0)
          (let* ((lead (string-ref str start))
                 (lead* (if (char-set-contains? char-set:whitespace
                                                lead)
                            (breakable-space)
                            (string lead))))
            (if (> len 1)
                (let* ((trail (string-ref str (- end 1)))
                       (trail* (if (char-set-contains? char-set:whitespace
                                                       trail)
                                   (breakable-space)
                                   (string trail))))
                  (list lead* (substring str (+ start 1) (- end 1))
                        trail*))
                (list lead* (substring str (+ start 1) end))))
          "")))

  (reverse!
   (fold (lambda (edit result)
           (let ((start (cadr edit))
                 (end   (+ 1 (caddr edit))))
             (cons (case (car edit)
                     ((insertion)
                      (insertion (space-preserving-substring str2 start
                                                             end)))
                     ((deletion)
                      (deletion  (space-preserving-substring str1 start
                                                             end)))
                     ((replacement)
                      (replacement (space-preserving-substring str2 start
                                                               end)))
                     ((unchanged)
                      (unchanged (space-preserving-substring str2 start
                                                             end))))
                   result)))
         '()
         (string-diff-sequences str1 str2))))

(define (make-diff-document ast1 ast2)
  ;; Return a document based on AST2 that highlights differences between AST1
  ;; and AST2, enclosing unchanged parts in `unchanged' markups, etc.  AST2
  ;; is used as the "reference" tree, thus changes from AST1 to AST2 are
  ;; shown in the resulting document.
  (define (undiffable? kind)
    (memq kind %undiffable-markups))

  (define (make-diff-options m1 m2 loop)
    ;; Return a list of options based on that of markup M2.
    (map (lambda (opt+val)
           (let ((opt (car opt+val)))
             (if (memq opt %diffable-options)
                 (cons opt
                       (loop (markup-option m1 opt)
                             (cdr opt+val)))
                 opt+val)))
         (markup-options m2)))


  (let loop ((ast1 ast1)
             (ast2 ast2))
    ;;(format (current-error-port) "diff: ~a ~a~%" ast1 ast2)
    (cond ((string? ast2)
           (if (string? ast1)
               (annotated-string-diff ast1 ast2)
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
                  (loc   (ast-loc ast2))
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
                 (class (markup-class ast2))
                 (body  (markup-body ast2)))
             (new container
                  (markup  kind)
                  (ident   ident)
                  (class   class)
                  (loc     (ast-loc ast2))
                  (options (if (or (undiffable? kind)
                                   (not (container? ast1)))
                               (markup-options ast2)
                               (make-diff-options ast1 ast2 loop)))
                  (body (if (undiffable? kind)
                            body
                            (loop (if (and (container? ast1)
                                           (is-markup? ast1 kind))
                                      (markup-body ast1)
                                      ast1)
                                  body))))))

          ((markup? ast2)
           (let ((kind  (markup-markup ast2))
                 (ident (markup-ident ast2))
                 (class (markup-class ast2))
                 (body  (markup-body ast2)))
             (new markup
                  (markup  kind)
                  (ident   ident)
                  (class   class)
                  (loc     (ast-loc ast2))
                  (options (if (or (undiffable? kind)
                                   (not (markup? ast1)))
                               (markup-options ast2)
                               (make-diff-options ast1 ast2 loop)))
                  (body (if (undiffable? kind)
                            body
                            (loop (if (is-markup? ast1 kind)
                                      (markup-body ast1)
                                      ast1)
                                  body))))))

          ((command? ast2)
           ;; Leave it untouched.
           (new command
                (loc  (ast-loc ast2))
                (fmt  (command-fmt ast2))
                (body (command-body ast2))))

          ((list? ast2)
           (if (list? ast1)
               (let liip ((ast1 ast1)
                          (ast2 ast2)
                          (result '()))
                 (if (null? ast2)
                     (reverse! result)
                     (liip (if (null? ast1) ast1 (cdr ast1))
                           (cdr ast2)
                           (cons (loop (if (null? ast1) #f (car ast1))
                                       (car ast2))
                                 result))))
               (map (lambda (x)
                      (loop ast1 x))
                    ast2)))

          ((equal? ast1 ast2)
           (unchanged :loc (ast-loc ast2) ast1))

          (else
           (let ((loc (and (ast? ast2) (ast-loc ast2))))
             (insertion :loc loc  ast2))))))



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
           (skribe-message "diff: loading first document~%")
           (evaluate-ast-from-port (open-input-file old-file)
                                   :reader reader
                                   :module (make-run-time-module))))
        (ast2
         (parameterize ((*bib-table* (make-bib-table 'doc-2)))
           (skribe-message "diff: loading second document~%")
           (evaluate-ast-from-port (open-input-file new-file)
                                   :reader reader
                                   :module (make-run-time-module)))))

    (resolve! ast1 engine env)
    (resolve! ast2 engine env)
    (make-diff-document ast1 ast2)))



;;;
;;; Default writers.
;;;

(markup-writer 'diff:deletion (find-engine 'base)
  :action (lambda (n e)
            ;;(color :fg "red" (symbol "middot"))

            ;; Output nothing by default so that the document remains
            ;; readable.
            #f))

(markup-writer 'diff:insertion (find-engine 'base)
  :action (lambda (n e)
            (output (color :fg "green" (markup-body n)) e)))

(markup-writer 'diff:replacement (find-engine 'base)
  :action (lambda (n e)
            (output (color :fg "orange" (markup-body n)) e)))

(markup-writer 'diff:unchanged (find-engine 'base)
  :action (lambda (n e)
            (output (markup-body n) e)))


;;; diff.scm ends here

;;; arch-tag: 69ad10fa-5688-4835-8956-439e44e26847
