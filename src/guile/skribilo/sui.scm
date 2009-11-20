;;; sui.scm -- Skribe URL Indices
;;;
;;; Copyright 2005, 2006, 2007, 2008, 2009  Ludovic Courtès <ludo@gnu.org>
;;; Copyright 2003, 2004  Manuel Serrano
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

(define-module (skribilo sui)
  :use-module (skribilo ast)
  :autoload   (skribilo parameters) (*verbose* *destination-file*
                                     *sui-path*)
  :autoload   (skribilo reader)     (make-reader)
  :autoload   (skribilo engine)     (find-engine)
  :autoload   (skribilo evaluator)  (evaluate-document)
  :use-module (skribilo condition)
  :use-module (skribilo utils strings)
  :use-module (skribilo utils syntax)
  :use-module (skribilo utils files)

  :use-module (ice-9 match)
  :use-module (srfi srfi-1)
  :autoload   (srfi srfi-34)        (raise)
  :use-module (srfi srfi-35)

  :export (load-sui sui-ref->url sui-title sui-file sui-key
           sui-find-ref sui-search-ref sui-filter
           document-sui sui-referenced-file sui-marks sui-blocks))

(skribilo-module-syntax)


;;; Author: Manuel Serrano, Ludovic Courtès
;;; Commentary:
;;;
;;; Library dealing with Skribe URL Indices (SUI).
;;;
;;; Code:


;;;
;;; Error conditions.
;;;

(define-condition-type &sui-error &skribilo-error
  sui-error?)

(define-condition-type &invalid-sui-error &sui-error
  invalid-sui-error?
  (sexp   invalid-sui-error:sexp))


(define (handle-sui-error c)
  ;; Issue a user-friendly error message for error condition C.
  (define (show-location sexp)
    (let* ((props  (and (pair? sexp) (source-properties sexp)))
           (file   (and props (assoc-ref props 'filename)))
           (line   (and props (assoc-ref props 'line)))
           (column (and props (assoc-ref props 'column))))
      (if (and file line column)
          (format (current-error-port) "~a:~a:~a: "
                  file line column))))

  (cond ((invalid-sui-error? c)
	 (let ((sexp (invalid-sui-error:sexp c)))
           (show-location sexp)
	   (format (current-error-port)
                   (_ "invalid SUI form: ~A~%")
                   sexp)))

	(else
	 (format (current-error-port)
                 (_ "undefined sui error: ~A~%")
		 c))))

(register-error-condition-handler! sui-error? handle-sui-error)


;*---------------------------------------------------------------------*/
;*    *sui-table* ...                                                  */
;*---------------------------------------------------------------------*/
;; FIXME: Remove global state.
(define *sui-table* (make-hash-table))

;*---------------------------------------------------------------------*/
;*    load-sui ...                                                     */
;*    -------------------------------------------------------------    */
;*    Returns a SUI sexp if already loaded. Load it otherwise.         */
;*    Raise an error if the file cannot be open.                       */
;*---------------------------------------------------------------------*/
(define (load-sui file)
  (let* ((path (search-path (*sui-path*) file))
         (sexp (and path (hash-ref *sui-table* path))))
    (if (not path)
        (raise (condition (&file-search-error (file-name file)
                                              (path (*sui-path*)))))
        (or sexp
            (begin
              (when (> (*verbose*) 0)
		(format (current-error-port) "  [loading sui: ~a]\n" path))
              (let ((p (open-input-file path))
                    (read (make-reader 'skribe)))
		(if (not (input-port? p))
                    (raise (condition (&file-open-error
                                       (file-name path))))
		    (unwind-protect
                     (let ((sexp (read p)))
                       (match sexp
                              (('sui (? string?) . _)
                               (hash-set! *sui-table* path sexp))
                              (else
                               (raise (condition (&invalid-sui-error
                                                  (sexp     sexp))))))
                       sexp)
                     (close-input-port p)))))))))

;*---------------------------------------------------------------------*/
;*    sui-ref->url ...                                                 */
;*---------------------------------------------------------------------*/
(define (sui-ref->url dir sui ident opts)
   (let ((refs (sui-find-ref sui ident opts)))
      (and (pair? refs)
	   (let ((base (sui-file sui))
		 (file (car (car refs)))
		 (mark (cdr (car refs))))
	      (format #f "~a/~a#~a" dir (or file base) mark)))))

;*---------------------------------------------------------------------*/
;*    sui-title ...                                                    */
;*---------------------------------------------------------------------*/
(define (sui-title sexp)
   (match sexp
      (('sui (and title (? string?)) . _)
       title)
      (else
       (raise (condition (&invalid-sui-error
                          (sexp sexp)))))))

;*---------------------------------------------------------------------*/
;*    sui-file ...                                                     */
;*---------------------------------------------------------------------*/
(define (sui-file sexp)
   (sui-key sexp :file))

;*---------------------------------------------------------------------*/
;*    sui-key ...                                                      */
;*---------------------------------------------------------------------*/
(define (sui-key sexp key)
   (match sexp
      (('sui _ . rest)
       (let loop ((rest rest))
	  (and (pair? rest)
	       (if (eq? (car rest) key)
		   (and (pair? (cdr rest))
			(cadr rest))
		   (loop (cdr rest))))))
      (else
       (raise (condition (&invalid-sui-error
                          (sexp sexp)))))))

;*---------------------------------------------------------------------*/
;*    sui-find-ref ...                                                 */
;*---------------------------------------------------------------------*/
(define (sui-find-ref sui ident opts)
   (let ((ident (assq :ident opts))
	 (mark (assq :mark opts))
	 (class (let ((c (assq :class opts)))
		   (and (pair? c) (cadr c))))
	 (chapter (assq :chapter opts))
	 (section (assq :section opts))
	 (subsection (assq :subsection opts))
	 (subsubsection (assq :subsubsection opts)))
      (match sui
	 (('sui (? string?) . refs)
	  (cond
	     (mark (sui-search-ref 'marks refs (cadr mark) class))
	     (chapter (sui-search-ref 'chapters refs (cadr chapter) class))
	     (section (sui-search-ref 'sections refs (cadr section) class))
	     (subsection (sui-search-ref 'subsections refs (cadr subsection) class))
	     (subsubsection (sui-search-ref 'subsubsections refs (cadr subsubsection) class))
	     (ident (sui-search-all-refs refs (cadr ident) class))
	     (else '())))
	 (else
          (raise (condition (&invalid-sui-error
                             (sexp sui))))))))

;*---------------------------------------------------------------------*/
;*    sui-search-all-refs ...                                          */
;*---------------------------------------------------------------------*/
(define (sui-search-all-refs refs id class)
  ;; Search any kind of object with ident ID among REFS.
  (define (find-mark full-ref)
    (let loop ((ref full-ref))
      (and (not (null? ref))
           (or (and (eq? (car ref) :mark)
                    (string=? (cadr ref) id)
                    (let ((f (memq :file full-ref))
                          (c (memq :mark full-ref)))
                      (list (cons (and (pair? f) (cadr f))
                                  (and (pair? c) (cadr c))))))
               (loop (cdr ref))))))

  (let loop ((ref-kind refs))
    (and (not (null? ref-kind))
         (or (and (pair? (car ref-kind))
                  (let liip ((refs (cdar ref-kind)))
                    (and (not (null? refs))
                         (or (find-mark (car refs))
                             (liip (cdr refs))))))
             (loop (cdr ref-kind))))))

;*---------------------------------------------------------------------*/
;*    sui-search-ref ...                                               */
;*---------------------------------------------------------------------*/
(define (sui-search-ref kind refs val class)
   (define (find-ref refs val class)
      (map (lambda (r)
	      (let ((f (memq :file r))
		    (c (memq :mark r)))
		 (cons (and (pair? f) (cadr f)) (and (pair? c) (cadr c)))))
	   (filter (if class
		       (lambda (m)
			  (and (pair? m)
			       (string? (car m))
			       (string=? (car m) val)
			       (let ((c (memq :class m)))
				  (and (pair? c)
				       (eq? (cadr c) class)))))
		       (lambda (m)
			  (and (pair? m)
			       (string? (car m))
			       (string=? (car m) val))))
		   refs)))
   (let loop ((refs refs))
      (if (pair? refs)
	  (if (and (pair? (car refs)) (eq? (caar refs) kind))
	      (find-ref (cdar refs) val class)
	      (loop (cdr refs)))
	  '())))

;*---------------------------------------------------------------------*/
;*    sui-filter ...                                                   */
;*---------------------------------------------------------------------*/
(define (sui-filter sui pred1 pred2)
   (match sui
      (('sui (? string?) . refs)
       (let loop ((refs refs)
		  (res '()))
	  (if (pair? refs)
	      (if (pred1 (car refs))
		  (loop (cdr refs)
			(cons (filter pred2 (cdar refs)) res))
		  (loop (cdr refs) res))
	      (reverse! res))))
      (else
       (raise (condition (&invalid-sui-error
                          (sexp sui)))))))

;*---------------------------------------------------------------------*/
;*    document-sui ...                                                 */
;*---------------------------------------------------------------------*/
(define (document-sui n e)
   (define (sui)
      (display "(sui \"")
      (evaluate-document (markup-option n :title) (find-engine 'html))
      (display "\"\n")
      (format #t "  :file ~s\n" (sui-referenced-file n e))
      (sui-marks n e)
      (sui-blocks 'chapter n e)
      (sui-blocks 'section n e)
      (sui-blocks 'subsection n e)
      (sui-blocks 'subsubsection n e)
      (display "  )\n"))
   (if (string? (*destination-file*))
       (let ((f (format #f "~a.sui" (file-prefix (*destination-file*)))))
	  (with-output-to-file f sui))
       (sui)))

;*---------------------------------------------------------------------*/
;*    sui-referenced-file ...                                          */
;*---------------------------------------------------------------------*/
(define (sui-referenced-file n e)

   ;; Hack to avoid a compile-time dependency on the HTML engine, which would
   ;; create a dependency loop:
   ;; (package base) -> (sui) -> (engine html) -> (package base).
   (define html-file
     (@ (skribilo engine html) html-file))

   (let ((file (html-file n e)))
      (if (member (file-suffix file) '("skb" "sui" "skr" "html"))
	  (string-append (strip-ref-base (file-prefix file)) ".html")
	  file)))

;*---------------------------------------------------------------------*/
;*    sui-marks ...                                                    */
;*---------------------------------------------------------------------*/
(define (sui-marks n e)
   (display "  (marks")
   (for-each (lambda (m)
		(format #t "\n    (~s" (markup-ident m))
		(format #t " :file ~s" (sui-referenced-file m e))
		(format #t " :mark ~s" (markup-ident m))
		(when (markup-class m)
		   (format #t " :class ~s" (markup-class m)))
		(display ")"))
	     (search-down (lambda (n) (is-markup? n 'mark)) n))
   (display ")\n"))

;*---------------------------------------------------------------------*/
;*    sui-blocks ...                                                   */
;*---------------------------------------------------------------------*/
(define (sui-blocks kind n e)
   (format #t "  (~as" kind)
   (for-each (lambda (chap)
		(display "\n    (\"")
		(evaluate-document (markup-option chap :title)
                                   (find-engine 'html))
		(format #t "\" :file ~s" (sui-referenced-file chap e))
		(format #t " :mark ~s" (markup-ident chap))
		(when (markup-class chap)
		   (format #t " :class ~s" (markup-class chap)))
		(display ")"))
	     (container-search-down (lambda (n) (is-markup? n kind)) n))
   (display ")\n"))


;;; Local Variables:
;;; coding: latin-1
;;; End:
