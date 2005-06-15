;;;;
;;;; reader.stk	-- Reader hook for the open bracket
;;;; 
;;;; Copyright (C) 2001-2003 Erick Gallesio - I3S-CNRS/ESSI <eg@unice.fr>
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
;;;;           Author: Erick Gallesio [eg@unice.fr]
;;;;    Creation date:  6-Dec-2001 22:59 (eg)
;;;; Last file update: 28-Feb-2004 10:22 (eg)
;;;;

;; Examples of ISO-2022-JP (here for cut'n paste tests, since my japanese
;; is *very*  limited ;-).
;;
;; "Japan" 日本
;; "China and Chinese music" 中国と中国の音楽 


;; 
;; This function is a hook for the standard reader. After defining,
;; %read-bracket, the reader calls it when it encounters an open
;; bracket


(define (%read-bracket in)

  (define (read-japanese in)
    ;; This function reads an ISO-2022-JP sequence. Susch s sequence is coded
    ;; as "^[$B......^[(B" . When entering in this function the current
    ;; character is 'B' (the opening sequence one). Function reads until the
    ;; end of the sequence and return it as a string
    (read-char in) ;; to skip the starting #\B
    (let ((res (open-output-string)))
      (let Loop ((c (peek-char in)))
	(cond 
	  ((eof-object? c) 		;; EOF
	   (error '%read-bracket "EOF encountered"))
	  ((char=? c #\escape)
	   (read-char in)
	   (let ((next1 (peek-char in)))
	     (if (char=? next1 #\()
		 (begin
		   (read-char in)
		   (let ((next2 (peek-char in)))
		     (if (char=? next2 #\B)
			 (begin
			   (read-char in)
			   (format "\033$B~A\033(B" (get-output-string res)))
			 (begin
			   (format res "\033~A" next1)
			   (Loop next2)))))
		 (begin
		   (display #\escape res)
		   (Loop next1)))))
	  (else (display (read-char in) res)
		(Loop (peek-char in)))))))
  ;;
  ;; Body of %read-bracket starts here
  ;;
  (let ((out       (open-output-string))
	(res       '())
	(in-string? #f))
    
    (read-char in)	; skip open bracket

    (let Loop ((c (peek-char in)))
      (cond 
         ((eof-object? c) 				;; EOF
	  	(error '%read-bracket "EOF encountered"))

	 ((char=? c #\escape)				;; ISO-2022-JP string?
	  	(read-char in)
		(let ((next1 (peek-char in)))
		  (if (char=? next1 #\$)
		      (begin
			(read-char in)
			(let ((next2 (peek-char in)))
			  (if (char=? next2 #\B)
			      (begin
				(set! res
				  (append! res
					   (list (get-output-string out)
						 (list 'unquote
						       (list 'jp
							     (read-japanese in))))))
				(set! out (open-output-string)))
			      (format out "\033~A" next1))))
		      (display #\escape out)))
		(Loop (peek-char in)))

	 ((char=? c #\\)				;; Quote char
	  	(read-char in)
		(display (read-char in)  out)
		(Loop (peek-char in)))
	 
	 ((and (not in-string?) (char=? c #\,))		;; Comma
	        (read-char in)
		(let ((next (peek-char in)))
		  (if (char=? next #\()
		      (begin
			(set! res (append! res (list (get-output-string out)
						     (list 'unquote
							   (read in)))))
			(set! out (open-output-string)))
		      (display #\, out))
		  (Loop (peek-char in))))

	 ((and (not in-string?) (char=? c #\[))		;; Open bracket
		(display (%read-bracket in) out)
		(Loop (peek-char in)))

	 ((and (not in-string?) (char=? c #\]))		;; Close bracket
	  	(read-char in)
		(let ((str (get-output-string out)))
		  (list 'quasiquote
			(append! res (if (string=? str "") '() (list str))))))

	 (else (when (char=? c #\") (set! in-string? (not in-string?)))
	       (display (read-char in) out)
	       (Loop (peek-char in)))))))

