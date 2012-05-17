;;; strings.scm	-- Convenience functions to manipulate strings.
;;;
;;; Copyright 2003, 2004  Erick Gallesio - I3S-CNRS/ESSI <eg@unice.fr>
;;; Copyright 2005, 2006, 2007  Ludovic Courtès <ludovic.courtes@laas.fr>
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

(define-module (skribilo utils strings)
  :export (strip-ref-base string-canonicalize
	   make-string-replace)
  :autoload   (skribilo parameters) (*ref-base*)
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

(define (%make-general-string-replace lst)
  ;; The general version
  (let ((chars (make-hash-table 200))
        (set   (apply char-set (map car lst))))

    ;; Setup a hash table equivalent to LST.
    (for-each (lambda (chr)
		(hashq-set! chars (car chr) (cadr chr)))
	      lst)

    ;; Help the GC.
    (set! lst #f)

    (lambda (str)
      ;; Note: This implementation is optimized for Guile 1.8 where
      ;; `string-index' is implemented in C and where `string-length' and
      ;; `string-ref' are O(1).  Consult the repository's history for a more
      ;; UTF-friendly implementation.
      (let ((len (string-length str)))
        (let loop ((pos 0)
                   (result '()))
          (if (>= pos len)
              (string-concatenate (reverse! result))
              (let ((idx (string-index str set pos)))
                (if idx
                    (loop (+ 1 idx)
                          (cons* (hashq-ref chars (string-ref str idx)
                                            #f)
                                 (substring str pos idx)
                                 result))
                    (loop len
                          (cons (substring str pos len)
                                result))))))))))

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

