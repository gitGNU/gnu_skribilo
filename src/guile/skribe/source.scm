;;;;
;;;; source.stk	-- Skibe SOURCE implementation stuff
;;;; 
;;;; Copyright © 2003-2004 Erick Gallesio - I3S-CNRS/ESSI <eg@essi.fr>
;;;; 
;;;; 
;;;; This program is free software; you can redistribute it and/or modify
;;;; it under the terms of the GNU General Public License as published by
;;;; the Free Software Foundation; either version 2 of the License, or
;;;; (at your option) any later version.
;;;; 
;;;; This program is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU General Public License for more details.
;;;; 
;;;; You should have received a copy of the GNU General Public License
;;;; along with this program; if not, write to the Free Software
;;;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, 
;;;; USA.
;;;; 
;;;;           Author: Erick Gallesio [eg@essi.fr]
;;;;    Creation date:  3-Sep-2003 12:22 (eg)
;;;; Last file update: 27-Oct-2004 20:09 (eg)
;;;;



(define-module (skribe source)
   :export (source-read-lines source-read-definition source-fontify))


;; Temporary solution
(define (language-extractor lang)
  (slot-ref lang 'extractor))

(define (language-fontifier lang)
  (slot-ref lang 'fontifier))


;*---------------------------------------------------------------------*/
;*    source-read-lines ...                                            */
;*---------------------------------------------------------------------*/
(define (source-read-lines file start stop tab)
   (let ((p (find-path file (skribe-source-path))))
     (if (or (not (string? p)) (not (file-exists? p)))
	  (skribe-error 'source
			(format "Can't find `~a' source file in path" file)
			(skribe-source-path))
	  (with-input-from-file p
	     (lambda ()
		(if (> *skribe-verbose* 0)
		    (format (current-error-port) "  [source file: ~S]\n" p))
		(let ((startl (if (string? start) (string-length start) -1))
		      (stopl  (if (string? stop)  (string-length stop)  -1)))
		   (let loop ((l      1)
			      (armedp (not (or (integer? start) (string? start))))
			      (s      (read-line))
			      (r      '()))
		      (cond
			 ((or (eof-object? s)
			      (and (integer? stop) (> l stop))
			      (and (string? stop) (substring=? stop s stopl)))
			  (apply string-append (reverse! r)))
			 (armedp
			  (loop (+ l 1)
				#t
				(read-line)
				(cons* "\n" (untabify s tab) r)))
			 ((and (integer? start) (>= l start))
			  (loop (+ l 1)
				#t
				(read-line)
				(cons* "\n" (untabify s tab) r)))
			 ((and (string? start) (substring=? start s startl))
			  (loop (+ l 1) #t (read-line) r))
			 (else
			  (loop (+ l 1) #f (read-line) r))))))))))

;*---------------------------------------------------------------------*/
;*    untabify ...                                                     */
;*---------------------------------------------------------------------*/
(define (untabify obj tab)
   (if (not tab)
       obj
       (let ((len (string-length obj))
	     (tabl tab))
	  (let loop ((i 0)
		     (col 1))
	     (cond
		((= i len)
		 (let ((nlen (- col 1)))
		    (if (= len nlen)
			obj
			(let ((new (make-string col #\space)))
			   (let liip ((i 0)
				      (j 0)
				      (col 1))
			      (cond
				 ((= i len)
				  new)
				 ((char=? (string-ref obj i) #\tab)
				  (let ((next-tab (* (/ (+ col tabl)
							    tabl)
						       tabl)))
				     (liip (+ i 1)
					   next-tab
					   next-tab)))
				 (else
				  (string-set! new j (string-ref obj i))
				  (liip (+ i 1) (+ j 1) (+ col 1)))))))))
		((char=? (string-ref obj i) #\tab)
		 (loop (+ i 1)
		       (* (/ (+ col tabl) tabl) tabl)))
		(else
		 (loop (+ i 1) (+ col 1))))))))

;*---------------------------------------------------------------------*/
;*    source-read-definition ...                                       */
;*---------------------------------------------------------------------*/
(define (source-read-definition file definition tab lang)
   (let ((p (find-path file (skribe-source-path))))
      (cond
	 ((not (language-extractor lang))
	  (skribe-error 'source
			"The specified language has not defined extractor"
			(slot-ref lang 'name)))
	 ((or (not p) (not (file-exists? p)))
	  (skribe-error 'source
			(format "Can't find `~a' program file in path" file)
			(skribe-source-path)))
	 (else
	  (let ((ip (open-input-file p)))
	     (if (> *skribe-verbose* 0)
		 (format (current-error-port) "  [source file: ~S]\n" p))
	     (if (not (input-port? ip))
		 (skribe-error 'source "Can't open file for input" p)
		 (unwind-protect
		    (let ((s ((language-extractor lang) ip definition tab)))
		       (if (not (string? s))
			   (skribe-error 'source
					 "Can't find definition"
					 definition)
			   s))
		    (close-input-port ip))))))))

;*---------------------------------------------------------------------*/
;*    source-fontify ...                                               */
;*---------------------------------------------------------------------*/
(define (source-fontify o language)
   (define (fontify f o)
      (cond
	 ((string? o) (f o))
	 ((pair? o) (map (lambda (s) (if (string? s) (f s) (fontify f s))) o))
	 (else o)))
   (let ((f (language-fontifier language)))
      (if (procedure? f)
	  (fontify f o)
	  o)))

;*---------------------------------------------------------------------*/
;*    split-string-newline ...                                         */
;*---------------------------------------------------------------------*/
(define (split-string-newline str)
   (let ((l (string-length str)))
      (let loop ((i 0)
		 (j 0)
		 (r '()))
	 (cond
	    ((= i l)
	     (if (= i j)
		 (reverse! r)
		 (reverse! (cons (substring str j i) r))))
	    ((char=? (string-ref str i) #\Newline)
	     (loop (+ i 1)
		   (+ i 1)
		   (if (= i j)
		       (cons 'eol r)
		       (cons* 'eol (substring str j i) r))))
	    ((and (char=? (string-ref str i) #\cr)
		  (< (+ i 1) l)
		  (char=? (string-ref str (+ i 1)) #\Newline))
	     (loop (+ i 2)
		   (+ i 2)
		   (if (= i j)
		       (cons 'eol r)
		       (cons* 'eol (substring str j i) r))))
	    (else
	     (loop (+ i 1) j r))))))

