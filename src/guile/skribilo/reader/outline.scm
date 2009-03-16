;;; outline.scm  --  A reader for Emacs' outline syntax.
;;;
;;; Copyright 2006, 2008, 2009  Ludovic Courtès <ludo@gnu.org>
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

(define-module (skribilo reader outline)
  :use-module (skribilo utils syntax)
  :use-module (skribilo reader)
  :use-module (ice-9 optargs)

  :use-module (srfi srfi-1)
  :use-module (srfi srfi-11)
  :use-module (srfi srfi-13)
  :use-module (srfi srfi-14)

  :autoload   (ice-9 rdelim) (read-line)
  :autoload   (ice-9 regex) (make-regexp)

  :export (reader-specification
           make-outline-reader))

(fluid-set! current-reader %skribilo-module-reader)

;;; Author:  Ludovic Courtès
;;;
;;; Commentary:
;;;
;;; A reader for Emacs' outline-mode syntax.
;;;
;;; Code:

;;; TODO:
;;;
;;; - add source position information;
;;; - handle `blockquote' (indented paragraph);
;;; - handle sublists (indented lists) --- optional;
;;; - handle inline Skribe code: `\n{skribe\n(table (tr ... ))\n}\n'




;;;
;;; Tools.
;;;

(define (apply-any procs args value-count)
  "Apply the procedure listed in @var{procs} to @var{args} until one of these
procedure returns true.  @var{value-count} is the number of values returned
by the procedures in @var{procs}."
  (let loop ((procs procs))
    (if (null? procs)
	(apply values (make-list value-count #f))
	(call-with-values
            (lambda ()
              (apply (car procs) args))
          (lambda results
            (if (every not results)
                (loop (cdr procs))
                (apply values results)))))))

(define (make-markup name body)
  "Return a clean markup form, i.e., an s-exp whose @code{car} is a symbol
equal to @var{name}, a markup name."
  (cond ((list? body)
	 (cond ((null? body) `(,name))
	       ((symbol? (car body)) `(,name ,body))
	       (else `(,name ,@body))))
	(else
	 (list name body))))


(define (append-trees . trees)
  "Append markup trees @var{trees}.  Trees whose car is a symbol (e.g.,
@code{(bold \"paf\")} will be considered as sub-trees of the resulting tree."
  (let loop ((trees trees)
	     (result '()))
    (if (null? trees)
	(let ((result (reverse! result)))
          (cond ((and (pair? result)
                      (not (symbol? (car result))))
                 ;; Make sure only symbols end up in the head.
                 (cons 'list result))
                (else
                 result)))
	(let ((tree (car trees)))
	  (loop (cdr trees)
		(append (if (list? tree)
                            (cond ((null? tree) '())
                                  ((symbol? (car tree)) (list tree))
                                  (else tree))
                            (list tree))
                        result))))))

(define (null-string? s)
  (and (string? s) (string=? s "")))


(define empty-line-rx (make-regexp "^([[:space:]]*|;.*)$"))
(define (empty-line? s)
  "Return true if string @var{s} denotes an ``empty'' line, i.e., a blank
line or a line comment."
  (regexp-exec empty-line-rx s))



;;;
;;; In-line markup, i.e., markup that doesn't span over multiple lines.
;;;

(define %inline-markup
  ;; Note: the order matters because, for instance, URLs must be searched for
  ;; _before_ italics (`/italic/').
  ;; XXX: This is much less efficient that a lexer as produced by, e.g., SILex.

  `(("_([^_]+)_" .                                 ;; emphasis
     ,(lambda (m)
	(values (match:prefix m)                           ;; before
		(match:substring m 1)                      ;; body
		(match:suffix m)                           ;; after
		(lambda (body) `(emph ,body)))))           ;; process-body

    ("\\[\\[([^]]+)\\]\\[([^]]+)\\]\\]" .          ;; Org-Mode hyperlink
     ,(let ((file-rx (make-regexp "^file:(.*)$" regexp/extended)))
        (lambda (m)
          (values (match:prefix m)
                  (match:substring m 2)
                  (match:suffix m)
                  (let ((xref (match:substring m 1)))
                    (lambda (body)
                      (cond ((regexp-exec file-rx xref)
                             =>
                             (lambda (m)
                               (let ((path (match:substring m 1)))
                                 `(ref :url ,(string-append "file://" path)
                                       :text ,body))))
                            (else
                             ;; XXX: We assume that everything that's not a
                             ;; `file:' link is a URL.
                             `(ref :url  ,xref
                                   :text ,body)))))))))
    ("(f|ht)tp://[a-zA-Z0-9\\._~%/-]+" .           ;; URL
     ,(lambda (m)
	(values (match:prefix m)
		(match:substring m)
		(match:suffix m)
		(lambda (url) `(ref :url ,url)))))

    ("\\/([^\\/]+)\\/" .                           ;; italic
     ,(lambda (m)
	(values (match:prefix m)
		(match:substring m 1)
		(match:suffix m)
		(lambda (body) `(it ,body)))))
    ("\\*([^\\*]+)\\*" .                           ;; bold
     ,(lambda (m)
	(values (match:prefix m)
		(match:substring m 1)
		(match:suffix m)
		(lambda (body) `(bold ,body)))))
    ("``(([^`^'])+)''" .                           ;; quote
     ,(lambda (m)
	(values (match:prefix m)
		(match:substring m 1)
		(match:suffix m)
		(lambda (body) `(q ,body)))))
    ("`(([^`^'])+)'" .                             ;; teletype
     ,(lambda (m)
	(values (match:prefix m)
		(match:substring m 1)
		(match:suffix m)
		(lambda (body) `(tt ,body)))))))

(define (make-markup-processor rx proc)
  (lambda (line)
    (let ((match (regexp-exec rx line)))
      (if match
	  (proc match)
	  #f))))

(define (make-line-processor markup-alist)
  "Returns a @dfn{line processor}.  A line processor is a procedure that
takes a string and returns a list."
  (let* ((markups (map (lambda (rx+proc)
			 (cons (make-regexp (car rx+proc) regexp/extended)
			       (cdr rx+proc)))
		       markup-alist))
	 (procs (map (lambda (rx+proc)
                       (make-markup-processor (car rx+proc) (cdr rx+proc)))
                     markups)))
    (lambda (line)
      (let self ((line line))
	;;(format #t "self: ~a~%" line)
	(cond ((string? line)
               (let-values (((before body after proc-body)
                             (apply-any procs (list line) 4)))
                 (if (and before body after proc-body)
                     (let ((body+
                            (if (string=? (string-append before body after)
                                          line)
                                body (self body))))
                       (if (and (null-string? before)
                                (null-string? after))
                           (proc-body body+)
                           (append-trees (self before)
                                         (proc-body body+)
                                         (self after))))
		     line)))
	      (else
	       (error "line-processor: internal error" line)))))))

(define %line-processor
  (make-line-processor %inline-markup))



;;;
;;; Large-scale structures: paragraphs, chapters, sections, etc.
;;;

(define (process-paragraph line line-proc port)
  (let loop ((line line)
	     (result '()))
    (if (or (eof-object? line) (empty-line? line))
	(cons 'p (list result))
	(loop (read-line port)
	      (let ((line (line-proc line)))
		(append-trees result line "\n"))))))

(define (make-list-processor rx node-type extract-line-proc line-proc
			     end-of-node?)
  "Return a procedure (a @dfn{list processor}) that takes a line and a port
and returns an AST node of type @var{node-type} (a symbol, typically
@code{itemize} or @code{enumerate}) along with a line.  If the processor is
not triggered, i.e., it is passed a line that does not match @var{rx}, then
it returns @code{#f}."
  (lambda (line port)
    (let ((match (regexp-exec rx line)))
      (if (not match)
	  #f
	  (let loop ((line line)
		     (contiguous-empty-lines 0)
		     (item '())
		     (body '()))
	      (if (eof-object? line)
		  (let ((body (if (null? item)
				  body
				  (cons `(item ,@(reverse! item)) body))))
		    (values line `(,node-type ,@(reverse! body))))
		  (let ((match (regexp-exec rx line)))
		    (cond (match
			   ;; reading the first line of an item
			   (loop (read-line port) 0
				 (append-trees
				  (line-proc (extract-line-proc match)))
				 body))

			  ((and (procedure? end-of-node?)
				(end-of-node? line))
			   (values line
				   `(,node-type ,@(reverse! body))))

			  ((empty-line? line)
			   (cond ((>= contiguous-empty-lines 1)
				  ;; end of list
				  (values line
					  `(,node-type ,@(reverse! body))))

				 ((= contiguous-empty-lines 0)
				  ;; end of item: add ITEM to BODY
				  (loop (read-line port) 1 '()
					(cons (make-markup 'item item)
					      body)))

				 (else
				  ;; skipping empty line
				  (loop (read-line port)
					(+ 1 contiguous-empty-lines)
					item body))))

			  (else
			   ;; reading an item: add LINE to ITEM
			   (loop (read-line port) 0
				 (append-trees item (line-proc line))
				 body))))))))))

(define (make-node-processor rx node-type title-proc line-proc
			     subnode-procs end-of-node?)
  "Return a procedure that reads the given string and return an AST node of
type @var{node-type} or @code{#f}.  When the original string matches the node
header, then the rest of the node is read from @var{port}.
@var{subnode-procs} is a list of node processors for node types subordinate
to @var{node-type}."
  (lambda (line port)
    (let ((match (regexp-exec rx line)))
      (if (not match)
	  #f
	  (let ((title (line-proc (title-proc match))))
	    (let loop ((line (read-line port))
		       (body '()))

              (let-values (((matching-line node)
                            (if (eof-object? line)
                                (values #f #f)
                                (apply-any subnode-procs
                                           (list line port)
                                           2))))
		(cond ((and matching-line node)
                       (loop matching-line (cons node body)))

		      ((or (eof-object? line)
			   (regexp-exec rx line)
			   (and (procedure? end-of-node?)
				(end-of-node? line)))
		       (values line
			       `(,node-type :title ,title ,@(reverse! body))))

		      ((empty-line? line)
		       (loop (read-line port) body))

		      (else
			   (let ((par (process-paragraph line line-proc port)))
			     (loop (read-line port)
				   (cons par body))))))))))))


(define (node-markup-line? line)
  (define node-rx (make-regexp "^\\*+ (.+)$" regexp/extended))
  (regexp-exec node-rx line))

(define %list-processors
  (list (make-list-processor (make-regexp "^[[:space:]]*[-~o] (.+)$" regexp/extended)
			     'itemize
			     (lambda (m) (match:substring m 1))
			     %line-processor
			     node-markup-line?)
	(make-list-processor (make-regexp "^[[:space:]]*([0-9]+)\\.? (.+)$"
					  regexp/extended)
			     'enumerate
			     (lambda (m) (match:substring m 2))
			     %line-processor
			     node-markup-line?)))

(define %node-processors
  (let* ((subsubsection-proc
	  (make-node-processor (make-regexp "^\\*\\*\\*\\* (.+)$"
					    regexp/extended)
			       'subsection
			       (lambda (m) (match:substring m 1))
			       %line-processor
			       %list-processors ;; no further subnodes
			       node-markup-line?))
	 (subsection-proc
	  (make-node-processor (make-regexp "^\\*\\*\\* (.+)$"
					    regexp/extended)
			       'subsection
			       (lambda (m) (match:substring m 1))
			       %line-processor
			       (append %list-processors
				       (list subsubsection-proc))
			       node-markup-line?))
	 (section-proc
	  (make-node-processor (make-regexp "^\\*\\* (.+)$" regexp/extended)
			       'section
			       (lambda (m) (match:substring m 1))
			       %line-processor
			       (append %list-processors
				       (list subsection-proc))
			       node-markup-line?)))
    (list (make-node-processor (make-regexp "^\\* (.+)$" regexp/extended)
			       'chapter
			       (lambda (m) (match:substring m 1))
			       %line-processor
			       (append %list-processors
				       (list section-proc))
			       #f))))




;;;
;;; The top-level parser.
;;;

(define (make-document-processor node-procs line-proc)
  (lambda (line port)
    (let self ((line line)
	       (doc '()))
      ;;(format #t "doc-proc: ~a~%" line)
      (if (eof-object? line)
	  (if (null? doc)
	      line
	      (reverse! doc))
	  (if (empty-line? line)
	      (self (read-line port) doc)
              (let-values (((matching-line node)
                            (apply-any node-procs (list line port) 2)))
		(if (and matching-line node)
                    (self matching-line (cons node doc))
		    (let ((par (process-paragraph line line-proc port)))
		      (self (read-line port)
			    (cons par doc))))))))))


(define* (outline-reader :optional (port (current-input-port)))
  (define modeline-rx
    (make-regexp "^[[:space:]]*-\\*- ([a-zA-Z0-9;:-]|[[:space:]])+ -\\*-[[:space:]]*$"))
  (define title-rx (make-regexp "^[Tt]itle: (.+)$" regexp/extended))
  (define author-rx (make-regexp "^[Aa]uthors?: (.+)$" regexp/extended))
  (define keywords-rx
    (make-regexp "^[Kk]ey ?[wW]ords?: (.+)$" regexp/extended))

  (define (extract-keywords str)
    (map string-trim-both
         (string-tokenize str (char-set-complement (char-set #\,)))))

  (let ((doc-proc (make-document-processor %node-processors %line-processor)))

    (let loop ((title #f)
	       (author #f)
               (keywords '())
	       (line (read-line port)))

      (if (eof-object? line)
	  (if (or title author)
	      `(document :title ,title :author (author :name ,author) '())
	      line)
	  (if (or (empty-line? line)
		  (regexp-exec modeline-rx line))
	      (loop title author keywords (read-line port))
	      (cond ((regexp-exec title-rx line)
                     =>
                     (lambda (title-match)
                       (loop (match:substring title-match 1)
                             author keywords (read-line port))))

                    ((regexp-exec author-rx line)
                     =>
                     (lambda (author-match)
                       (loop title (match:substring author-match 1)
                             keywords (read-line port))))

                    ((regexp-exec keywords-rx line)
                     =>
                     (lambda (kw-match)
                       (loop title author
                             (append keywords
                                     (extract-keywords
                                      (match:substring kw-match 1)))
                             (read-line port))))

                    (else
                     ;; Let's go.
                     `(document :title ,title
                                :author (author :name ,author)
                                :keywords ',keywords
                                ,@(doc-proc line port)))))))))


(define* (make-outline-reader :optional (version "0.1"))
  outline-reader)



;;;
;;; The reader specification.
;;;

(define-reader outline "0.1" make-outline-reader)


;;; Local Variables:
;;; coding: latin-1
;;; End:

;;; outline.scm ends here
