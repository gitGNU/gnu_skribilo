;*=====================================================================*/
;*    serrano/prgm/project/skribe/src/bigloo/read.scm                  */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Dec 27 11:16:00 1994                          */
;*    Last change :  Mon Nov  8 13:30:32 2004 (serrano)                */
;*    -------------------------------------------------------------    */
;*    Skribe's reader                                                  */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    Le module                                                        */
;*---------------------------------------------------------------------*/
(module skribe_read
   (export (skribe-read . port)))

;*---------------------------------------------------------------------*/
;*    Global counteurs ...                                             */
;*---------------------------------------------------------------------*/
(define *par-open*  0)

;*---------------------------------------------------------------------*/
;*    Parenthesis mismatch (or unclosing) errors.                      */
;*---------------------------------------------------------------------*/
(define *list-error-level* 20)
(define *list-errors*      (make-vector *list-error-level* #unspecified))
(define *vector-errors*    (make-vector *list-error-level* #unspecified))

;*---------------------------------------------------------------------*/
;*    Control variables.                                               */
;*---------------------------------------------------------------------*/
(define *end-of-list*       (cons 0 0))
(define *dotted-mark*       (cons 1 1))

;*---------------------------------------------------------------------*/
;*    skribe-reader-reset! ...                                         */
;*---------------------------------------------------------------------*/
(define (skribe-reader-reset!)
   (set! *par-open* 0))

;*---------------------------------------------------------------------*/
;*    read-error ...                                                   */
;*---------------------------------------------------------------------*/
(define (read-error msg obj port)
   (let* ((obj-loc (if (epair? obj)
		       (match-case (cer obj)
			  ((at ?fname ?pos ?-)
			   pos)
			  (else
			   #f))
		       #f))
	  (loc (if (number? obj-loc)
		   obj-loc
		   (cond
		      ((>fx *par-open* 0)
		       (let ((open-key (-fx *par-open* 1)))
			  (if (<fx open-key (vector-length *list-errors*))
			      (vector-ref *list-errors* open-key)
			      #f)))
		      (else
		       #f)))))
      (if (fixnum? loc)
	  (error/location "skribe-read" msg obj (input-port-name port) loc)
	  (error "skribe-read" msg obj))))

;*---------------------------------------------------------------------*/
;*    make-list! ...                                                   */
;*---------------------------------------------------------------------*/
(define (make-list! l port)
   (define (reverse-proper-list! l)
      (let nr ((l l)
	       (r '()))
	 (cond
	    ((eq? (car l) *dotted-mark*)
	     (read-error "Illegal pair" r port))
	    ((null? (cdr l))
	     (set-cdr! l r)
	     l)
	    (else
	     (let ((cdrl (cdr l)))
		(nr cdrl
		    (begin (set-cdr! l r)
			   l)))))))
   (define (reverse-improper-list! l)
      (let nr ((l (cddr l))
	       (r (car l)))
	 (cond
	    ((eq? (car l) *dotted-mark*)
	     (read-error "Illegal pair" r port))
	    ((null? (cdr l))
	     (set-cdr! l r)
	     l)
	    (else
	     (let ((cdrl (cdr l)))
		(nr cdrl
		    (begin (set-cdr! l r)
			   l)))))))
   (cond
      ((null? l)
       l)
      ((and (pair? l) (pair? (cdr l)) (eq? (cadr l) *dotted-mark*))
       (if (null? (cddr l))
	   (car l)
	   (reverse-improper-list! l)))
      (else
       (reverse-proper-list! l)))) 

;*---------------------------------------------------------------------*/
;*    make-at ...                                                      */
;*---------------------------------------------------------------------*/
(define (make-at name pos)
   (cond-expand
      ((or bigloo2.4 bigloo2.5 bigloo2.6)
       `(at ,name ,pos _))
      (else
       `(at ,name ,pos))))

;*---------------------------------------------------------------------*/
;*    collect-up-to ...                                                */
;*    -------------------------------------------------------------    */
;*    The first pair of the list is special because of source file     */
;*    location. We want the location to be associated to the first     */
;*    open parenthesis, not the last character of the car of the list. */
;*---------------------------------------------------------------------*/
(define-inline (collect-up-to ignore kind port)
   (let ((name (input-port-name port)))
      (let* ((pos  (input-port-position port))
	     (item (ignore)))
	 (if (eq? item *end-of-list*)
	     '()
	     (let loop ((acc (econs item '() (make-at name pos))))
		(let ((item (ignore)))
		   (if (eq? item *end-of-list*)
		       acc
		       (loop (let ((new-pos  (input-port-position port)))
				(econs item
				       acc
				       (make-at name new-pos)))))))))))

;*---------------------------------------------------------------------*/
;*    read-quote ...                                                   */
;*---------------------------------------------------------------------*/
(define (read-quote kwote port ignore)
   (let* ((pos (input-port-position port))
	  (obj (ignore)))
      (if (or (eof-object? obj) (eq? obj *end-of-list*))
	  (error/location "read"
			  "Illegal quotation"
			  kwote
			  (input-port-name port)
			  pos))
      (econs kwote
	     (cons obj '())
	     (make-at (input-port-name port) pos))))

;*---------------------------------------------------------------------*/
;*    *sexp-grammar* ...                                               */
;*---------------------------------------------------------------------*/
(define *sexp-grammar*
   (regular-grammar ((float    (or (: (* digit) "." (+ digit))
			 	   (: (+ digit) "." (* digit))))
		     (letter   (in ("azAZ") (#a128 #a255)))
		     (special  (in "!@~$%^&*></-_+\\=?.:{}"))
		     (kspecial (in "!@~$%^&*></-_+\\=?."))
		     (quote    (in "\",'`"))
		     (paren    (in "()"))
		     (id       (: (* digit)
				  (or letter special)
				  (* (or letter special digit (in ",'`")))))
		     (kid      (: (* digit)
				  (or letter kspecial)
				  (* (or letter kspecial digit (in ",'`")))))
		     (blank    (in #\Space #\Tab #a012 #a013)))
      
      ;; newlines
      ((+ #\Newline)
       (ignore))
      
      ;; blank lines
      ((+ blank)
       (ignore))
      
      ;; comments
      ((: ";" (* all))
       (ignore))
      
      ;; the interpreter header or the dsssl named constants
      ((: "#!" (+ (in letter)))
       (let* ((str (the-string)))
	  (cond
	     ((string=? str "#!optional")
	      boptional)
	     ((string=? str "#!rest")
	      brest)
	     ((string=? str "#!key")
	      bkey)
	     (else
	      (ignore)))))
      
      ;; characters
      ((: (uncase "#a") (= 3 digit))
       (let ((string (the-string)))
	  (if (not (=fx (the-length) 5))
	      (error/location "skribe-read"
			      "Illegal ascii character"
			      string
			      (input-port-name     (the-port))
			      (input-port-position (the-port)))
	      (integer->char (string->integer (the-substring 2 5))))))
      ((: "#\\" (or letter digit special (in "|#; []" quote paren)))
       (string-ref (the-string) 2))
      ((: "#\\" (>= 2 letter))
       (let ((char-name (string->symbol
			 (string-upcase!
			  (the-substring 2 (the-length))))))
	  (case char-name
	     ((NEWLINE)
	      #\Newline)
	     ((TAB)
	      #\tab)
	     ((SPACE)
	      #\space)
	     ((RETURN)
	      (integer->char 13))
	     (else
	      (error/location "skribe-read"
			      "Illegal character"
			      (the-string)
			      (input-port-name     (the-port))
			      (input-port-position (the-port)))))))
      
      ;; ucs-2 characters
      ((: "#u" (= 4 xdigit))
       (integer->ucs2 (string->integer (the-substring 2 6) 16)))
      
      ((: "\"" (* (or (out #a000 #\\ #\") (: #\\ all))) "\"")
       (let ((str (the-substring 1 (-fx (the-length) 1))))
	  (let ((str (the-substring 0 (-fx (the-length) 1))))
	     (escape-C-string str))))
      ;; ucs2 strings
      ((: "#u\"" (* (or (out #a000 #\\ #\") (: #\\ all))) "\"")
       (let ((str (the-substring 3 (-fx (the-length) 1))))
  	  (utf8-string->ucs2-string str)))
      
      ;; fixnums
      ((: (? (in "-+")) (+ digit))
       (the-fixnum))
      ((: "#o" (? (in "-+")) (+ (in ("07"))))
       (string->integer (the-substring 2 (the-length)) 8))
      ((: "#d" (? (in "-+")) (+ (in ("09"))))
       (string->integer (the-substring 2 (the-length)) 10))
      ((: "#x" (? (in "-+")) (+ (in (uncase (in ("09af"))))))
       (string->integer (the-substring 2 (the-length)) 16))
      ((: "#e" (? (in "-+")) (+ digit))
       (string->elong (the-substring 2 (the-length)) 10))
      ((: "#l" (? (in "-+")) (+ digit))
       (string->llong (the-substring 2 (the-length)) 10))
      
      ;; flonum
      ((: (? (in "-+"))
	  (or float
	      (: (or float (+ digit)) (in "eE") (? (in "+-")) (+ digit))))
       (the-flonum))
      
      ;; doted pairs
      ("."
       (if (<=fx *par-open* 0)
	   (error/location "read"
			   "Illegal token"
			   #\.
			   (input-port-name     (the-port))
			   (input-port-position (the-port)))
	   *dotted-mark*))
      
      ;; unspecified and eof-object
      ((: "#" (in "ue") (+ (in "nspecified-objt")))
       (let ((symbol (string->symbol
		      (string-upcase!
		       (the-substring 1 (the-length))))))
	  (case symbol
	     ((UNSPECIFIED)
	      unspec)
	     ((EOF-OBJECT)
	      beof)
	     (else
	      (error/location "read"
			      "Illegal identifier"
			      symbol
			      (input-port-name     (the-port))
			      (input-port-position (the-port)))))))
      
      ;; booleans
      ((: "#" (uncase #\t))
       #t)
      ((: "#" (uncase #\f))
       #f)
      
      ;; keywords
      ((or (: ":" kid) (: kid ":"))
       ;; since the keyword expression is also matched by the id
       ;; rule, keyword rule has to be placed before the id rule.
       (the-keyword))
      
      ;; identifiers
      (id
       ;; this rule has to be placed after the rule matching the `.' char
       (the-symbol))
      ((: "|" (+ (or (out #a000 #\\ #\|) (: #\\ all))) "|")
       (if (=fx (the-length) 2)
	   (the-symbol)
	   (let ((str (the-substring 0 (-fx (the-length) 1))))
	      (string->symbol (escape-C-string str)))))
      
      ;; quotations 
      ("'"
       (read-quote 'quote (the-port) ignore))
      ("`"
       (read-quote 'quasiquote (the-port) ignore))
      (","
       (read-quote 'unquote (the-port) ignore))
      (",@"
       (read-quote 'unquote-splicing (the-port) ignore))
      
      ;; lists
      (#\(
       ;; if possible, we store the opening parenthesis.
       (if (and (vector? *list-errors*)
		(<fx *par-open* (vector-length *list-errors*)))
	   (vector-set! *list-errors*
			*par-open*
			(input-port-position (the-port))))
       ;; we increment the number of open parenthesis
       (set! *par-open* (+fx 1 *par-open*))
       ;; and then, we compute the result list...
       (make-list! (collect-up-to ignore "list" (the-port)) (the-port)))
      (#\)
       ;; we decrement the number of open parenthesis
       (set! *par-open* (-fx *par-open* 1))
       (if (<fx *par-open* 0)
	   (begin
	      (warning/location (input-port-name (the-port))
				(input-port-position (the-port))
				"read"
				"Superfluous closing parenthesis `"
				(the-string)
				"'")
	      (set! *par-open* 0)
	      (ignore))
	   *end-of-list*))

      ;; list of strings
      (#\[
       (let ((exp (read/rp *text-grammar* (the-port))))
	  (list 'quasiquote exp)))
      
      ;; vectors
      ("#("
       ;; if possible, we store the opening parenthesis.
       (if (and (vector? *vector-errors*)
		(<fx *par-open* (vector-length *vector-errors*)))
	   (let ((pos (input-port-position (the-port))))
	      (vector-set! *vector-errors* *par-open* pos)))
       ;; we increment the number of open parenthesis
       (set! *par-open* (+fx 1 *par-open*))
       (list->vector (reverse! (collect-up-to ignore "vector" (the-port)))))
      
      ;; error or eof
      (else
       (let ((port (the-port))
	     (char (the-failure)))
	  (if (eof-object? char)
	      (cond
		 ((>fx *par-open* 0)
		  (let ((open-key (-fx *par-open* 1)))
		     (skribe-reader-reset!)
		     (if (and (<fx open-key (vector-length *list-errors*))
			      (fixnum? (vector-ref *list-errors* open-key)))
			 (error/location "skribe-read"
					 "Unclosed list"
					 char
					 (input-port-name port)
					 (vector-ref *list-errors* open-key))
			 (error "skribe-read"
				"Unexpected end-of-file"
				"Unclosed list"))))
		 (else
		  (reset-eof port)
		  char))
	      (error/location "skribe-read"
			      "Illegal char"
			      (illegal-char-rep char)
			      (input-port-name     port)
			      (input-port-position port)))))))

;*---------------------------------------------------------------------*/
;*    *text-grammar* ...                                               */
;*    -------------------------------------------------------------    */
;*    The grammar that parses texts (the [...] forms).                 */
;*---------------------------------------------------------------------*/
(define *text-grammar*
   (regular-grammar ()
      ((: (* (out ",[]\\")) #\])
       (let* ((port (the-port))
	      (name (input-port-name port))
	      (pos (input-port-position port))
	      (loc (make-at name pos))
	      (item (the-substring 0 (-fx (the-length) 1))))
	  (econs item '() loc)))
      ((: (* (out ",[\\")) ",]")
       (let* ((port (the-port))
	      (name (input-port-name port))
	      (pos (input-port-position port))
	      (loc (make-at name pos))
	      (item (the-substring 0 (-fx (the-length) 1))))
	  (econs item '() loc)))
      ((: (* (out ",[]\\")) #\,)
       (let* ((port (the-port))
	      (name (input-port-name port))
	      (pos (input-port-position port))
	      (loc (make-at name pos))
	      (item (the-substring 0 (-fx (the-length) 1)))
	      (sexp (read/rp *sexp-grammar* (the-port)))
	      (rest (ignore)))
	  (if (string=? item "")
	      (cons (list 'unquote sexp) rest)
	      (econs item (cons (list 'unquote sexp) rest) loc))))
      ((or (+ (out ",[]\\"))
	   (+ #\Newline)
	   (: (* (out ",[]\\")) #\, (out "([]\\")))
       (let* ((port (the-port))
	      (name (input-port-name port))
	      (pos (input-port-position port))
	      (loc (make-at name pos))
	      (item (the-string))
	      (rest (ignore)))
	  (econs item rest loc)))
      ("\\\\"
       (cons "\\" (ignore)))
      ("\\n"
       (cons "\n" (ignore)))
      ("\\t"
       (cons "\t" (ignore)))
      ("\\]"
       (cons "]" (ignore)))
      ("\\["
       (cons "[" (ignore)))
      ("\\,"
       (cons "," (ignore)))
      (#\\
       (cons "\\" (ignore)))
      (else
       (let ((c (the-failure))
	     (port (the-port)))
	  (define (err msg)
	     (error/location "skribe-read-text"
			     msg
			     (the-failure)
			     (input-port-name port)
			     (input-port-position port)))
	  (cond
	     ((eof-object? c)
	      (err "Illegal `end of file'"))
	     ((char=? c #\[)
	      (err "Illegal nested `[...]' form"))
	     (else
	      (err "Illegal string character")))))))

;*---------------------------------------------------------------------*/
;*    skribe-read ...                                                  */
;*---------------------------------------------------------------------*/
(define (skribe-read . input-port)
   (cond
      ((null? input-port)
       (read/rp *sexp-grammar* (current-input-port)))
      ((not (input-port? (car input-port)))
       (error "read" "type `input-port' expected" (car input-port)))
      (else
       (let ((port (car input-port)))
	  (if (closed-input-port? port)
	      (error "read" "Illegal closed input port" port)
	      (read/rp *sexp-grammar* port))))))

