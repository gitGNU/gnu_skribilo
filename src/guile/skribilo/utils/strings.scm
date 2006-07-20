;;; strings.scm	-- Convenience functions to manipulate strings.
;;;
;;; Copyright 2003, 2004  Erick Gallesio - I3S-CNRS/ESSI <eg@unice.fr>
;;; Copyright 2005, 2006  Ludovic Courtès <ludovic.courtes@laas.fr>
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

(define-module (skribilo utils strings)
  :export (strip-ref-base string-canonicalize
	   make-string-replace)
  :autoload   (skribilo parameters) (*ref-base*)
  :use-module (skribilo lib)
  :use-module (srfi srfi-13))


;;;
;;; Utilities.
;;;

(define (strip-ref-base file)
  ;; Given FILE, a file path (a string), remove `(*ref-base*)'  from it.
  ;; This is useful, e.g., for hyperlinks.
  (if (not (string? (*ref-base*)))
      file
      (let ((l (string-length (*ref-base*))))
	(cond
	  ((not (> (string-length file) (+ l 2)))
	   file)
	  ((not (string-contains file (*ref-base*) 0 l))
	   file)
	  ((not (char=? (string-ref file l) #\/))
	   file)
	  (else
	   (substring file (+ l 1) (string-length file)))))))


(define (string-canonicalize old)
   ;; Return a string that is a canonical summarized representation of string
   ;; OLD.  This is a one-way function.
   (let* ((l (string-length old))
	  (new (make-string l)))
      (let loop ((r 0)
		 (w 0)
		 (s #f))
	 (cond
	    ((= r l)
	     (cond
		((= w 0)
		 "")
		((char-whitespace? (string-ref new (- w 1)))
		 (substring new 0 (- w 1)))
		((= w r)
		 new)
		(else
		 (substring new 0 w))))
	    ((char-whitespace? (string-ref old r))
	     (if s
		 (loop (+ r 1) w #t)
		 (begin
		    (string-set! new w #\-)
		    (loop (+ r 1) (+ w 1) #t))))
	    ((or (char=? (string-ref old r) #\#)
		 (>= (char->integer (string-ref old r)) #x7f))
	     (string-set! new w #\-)
	     (loop (+ r 1) (+ w 1) #t))
	    (else
	     (string-set! new w (string-ref old r))
	     (loop (+ r 1) (+ w 1) #f))))))




;;;
;;; String writing.
;;;

;;
;; (define (%make-html-replace)
;;   ;; Ad-hoc version for HTML, a little bit faster than the
;;   ;; make-general-string-replace define later (particularily if there
;;   ;; is nothing to replace since, it does not allocate a new string
;;   (let ((specials (string->regexp "&|\"|<|>")))
;;     (lambda (str)
;;       (if (regexp-match specials str)
;;	  (begin
;;	    (let ((out (open-output-string)))
;;	      (dotimes (i (string-length str))
;;		(let ((ch (string-ref str i)))
;;		  (case ch
;;		    ((#\") (display "&quot;" out))
;;		    ((#\&) (display "&amp;" out))
;;		    ((#\<) (display "&lt;" out))
;;		    ((#\>) (display "&gt;" out))
;;		    (else  (write-char ch out)))))
;;	      (get-output-string out)))
;;	  str))))


(define (%make-general-string-replace lst)
  ;; The general version
  (let ((chars (make-hash-table)))

    ;; Setup a hash table equivalent to LST.
    (for-each (lambda (chr)
		(hashq-set! chars (car chr) (cadr chr)))
	      lst)

    ;; Help the GC.
    (set! lst #f)

    (lambda (str)
      (let ((out (open-output-string)))
	(string-for-each (lambda (ch)
			   (let ((res (hashq-ref chars ch #f)))
			     (display (if res res ch) out)))
			 str)
	(get-output-string out)))))

(define %html-replacements
  '((#\" "&quot;") (#\& "&amp;") (#\< "&lt;") (#\> "&gt;")))

(define %string->html
  (%make-general-string-replace %html-replacements))

(define (make-string-replace lst)
  (let ((l (sort lst (lambda (r1 r2) (char<? (car r1) (car r2))))))
    (cond
      ((equal? l %html-replacements)
       %string->html)
      (else
       (%make-general-string-replace lst)))))

