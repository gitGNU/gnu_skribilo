;;; outline.scm  --  A reader for Emacs' outline syntax.
;;;
;;; Copyright 2006  Ludovic Courtès <ludovic.courtes@laas.fr>
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

(define-module (skribilo reader outline)
  :use-module (skribilo utils syntax)
  :use-module (skribilo reader)
  :use-module (ice-9 optargs)
  :use-module (srfi srfi-11)

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


;;;
;;; In-line markup, i.e., markup that doesn't span over multiple lines.
;;;

(define %inline-markup
  `(("_([^_]+)_" .
     ,(lambda (m)
	(values (match:prefix m)                           ;; before
		(match:substring m 1)                      ;; body
		(match:suffix m)                           ;; after
		(lambda (body) `(emph ,body)))))           ;; process-body
    ("\\/([^\\/]+)\\/" .
     ,(lambda (m)
	(values (match:prefix m)
		(match:substring m 1)
		(match:suffix m)
		(lambda (body) `(it ,body)))))
    ("\\*([^\\*]+)\\*" .
     ,(lambda (m)
	(values (match:prefix m)
		(match:substring m 1)
		(match:suffix m)
		(lambda (body) `(bold ,body)))))
    ("`(([^`]|[^'])+)'" .
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

(define (append-trees . trees)
  "Append markup trees @var{trees}.  Trees whose car is a symbol will be
considered as sub-trees of the resulting tree."
  (let loop ((trees trees)
	     (result '()))
    (if (null? trees)
	result
	(let ((tree (car trees)))
	  (loop (cdr trees)
		(append result
			(if (list? tree)
			    (cond ((symbol? (car tree)) (list tree))
				  (else tree))
			    (list tree))))))))

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
	(format #t "self: ~a~%" line)
	(cond ((string? line)
	       (let loop ((procs procs))
		 (if (null? procs)
		     line
		     (let ((result (apply (car procs) (list line))))
		       (if result
			   (let-values (((before body after proc-body)
					 result))
			     (append-trees (self before)
					   (proc-body (self body))
					   (self after)))
			   (loop (cdr procs)))))))
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
    (if (or (eof-object? line) (string=? line ""))
	(cons 'p result)
	(loop (read-line port)
	      (let ((line (line-proc line)))
		(append result
			(if (list? line) line (list line))))))))

(define (make-node-processor rx node-type title-proc line-proc
			     subnode-proc end-of-node?)
  "Return a procedure that reads the given string and return an AST node of
type @var{node-type} or @code{#f}.  When the original string matches the node
header, then the rest of the node is read from @var{port}."
  (lambda (line port)
    (let ((match (regexp-exec rx line)))
      (if (not match)
	  #f
	  (let ((title (title-proc match)))
	    (let loop ((line (read-line port))
		       (body '()))
	      (cond ((or (eof-object? line)
			 (regexp-exec rx line)
			 (and (procedure? end-of-node?)
			      (end-of-node? line)))
		     (values line
			     `(,node-type :title ,title ,@(reverse! body))))

		    ((string=? "" line)
		     (loop (read-line port) body))

		    (else
		     (let ((subnode (and subnode-proc
					 (apply subnode-proc
						(list line port)))))
		       (if subnode
			   (let-values (((line node) subnode))
			     (loop line (cons node body)))
			   (let ((par (process-paragraph line line-proc port)))
			     (loop (read-line port)
				   (cons par body)))))))))))))


(define (node-markup-line? line)
  (define node-rx (make-regexp "^\\*+ (.+)$" regexp/extended))
  (regexp-exec node-rx line))

(define %node-processors
  (let ((section-proc
	 (make-node-processor (make-regexp "^\\*\\* (.+)$" regexp/extended)
			      'section
			      (lambda (m) (match:substring m 1))
			      %line-processor
			      #f
			      node-markup-line?)))
    (list (make-node-processor (make-regexp "^\\* (.+)$" regexp/extended)
			       'chapter
			       (lambda (m) (match:substring m 1))
			       %line-processor
			       section-proc
			       #f))))




;;;
;;; The top-level parser.
;;;

(define (make-document-processor node-procs line-proc port)
  (lambda (line port)
    (let self ((line line)
	       (doc '()))
      (format #t "doc-proc: ~a~%" line)
      (if (eof-object? line)
	  (reverse! doc)
	  (let loop ((node-procs node-procs))
	    (if (null? node-procs)
		(self (read-line port)
		      (cons (process-paragraph line line-proc port) doc))
		(let ((result (apply (car node-procs) (list line port))))
		  (if result
		      (let-values (((line node) result))
			(self line (cons node doc)))
		      (loop (cdr node-procs))))))))))


(define* (outline-reader :optional (port (current-input-port)))
  (define modeline-rx
    (make-regexp "^[[:space:]]*-\\*- [a-zA-Z-]+ -\\*-[[:space:]]*$"))
  (define title-rx (make-regexp "^[Tt]itle: (.+)$" regexp/extended))
  (define author-rx (make-regexp "^[Aa]uthor: (.+)$" regexp/extended))

  (let ((doc-proc (make-document-processor %node-processors %line-processor
					   port)))

    (let loop ((title #f)
	       (author #f)
	       (line (read-line port)))

      (if (eof-object? line)
	  `(document :title ,title :author (author :name ,author) '())
	  (if (or (string=? line "")
		  (regexp-exec modeline-rx line))
	      (loop title author (read-line port))
	      (let ((title-match (regexp-exec title-rx line)))
		(if title-match
		    (loop (match:substring title-match 1)
			  author (read-line port))
		    (let ((author-match (regexp-exec author-rx line)))
		      (if author-match
			  (loop title (match:substring author-match 1)
				(read-line port))

			  ;; Let's go.
			  `(document :title ,title
				     :author (author :name ,author)
				     ,@(doc-proc line port)))))))))))


(define* (make-outline-reader :optional (version "0.1"))
  outline-reader)



;;; The reader specification.

(define-reader outline "0.1" make-outline-reader)


;;; arch-tag: 53473e73-c811-4eed-a0b4-22ada4d6ef08

;;; outline.scm ends here

