;*=====================================================================*/
;*    .../skribe/tools/skribebibtex/bigloo/skribebibtex.scm            */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Oct 12 14:57:58 2001                          */
;*    Last change :  Sun Apr 10 09:10:02 2005 (serrano)                */
;*    Copyright   :  2001-05 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    The bibtex->skribe translator                                    */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module skribebibtex
   (export (skribebibtex in)))

;*---------------------------------------------------------------------*/
;*    skribebibtex ...                                                 */
;*---------------------------------------------------------------------*/
(define (skribebibtex in)
   (let* ((port (if (string? in)
		    (let ((p (open-input-file in)))
		       (if (not (input-port? p))
			   (error "skribebibtext"
				  "Can't read input file"
				  in)
			   p))
		    (current-input-port)))
	  (sexp (parse-bibtex port)))
      (for-each (lambda (e)
		   (match-case e
		      ((?kind ?ident . ?fields)
		       (display* "("
				 (string-downcase (symbol->string kind))
				 " \"" ident "\"")
		       (for-each (lambda (f)
				    (display* "\n   (" (car f) " ")
				    (write (cdr f))
				    (display ")"))
				 fields)
		       (print ")\n"))))
		sexp)))

;*---------------------------------------------------------------------*/
;*    *bibtex-string-table* ...                                        */
;*---------------------------------------------------------------------*/
(define *bibtex-string-table* #unspecified)

;*---------------------------------------------------------------------*/
;*    make-bibtex-hashtable ...                                        */
;*---------------------------------------------------------------------*/
(define (make-bibtex-hashtable)
   (let ((table (make-hashtable)))
      (for-each (lambda (k)
		   (let ((cp (string-capitalize k)))
		      (hashtable-put! table k cp)
		      (hashtable-put! table cp cp)))
		'("jan" "feb" "mar" "apr" "may" "jun" "jul"
			"aug" "sep" "oct" "nov" "dec"))
      table))

;*---------------------------------------------------------------------*/
;*    parse-bibtex ...                                                 */
;*---------------------------------------------------------------------*/
(define (parse-bibtex port::input-port)
   (set! *bibtex-string-table* (make-bibtex-hashtable))
   (cond-expand
      (bigloo2.6
       (try (read/lalrp bibtex-parser bibtex-lexer port)
	    (lambda (escape proc mes obj)
	       (match-case obj
		  ((?token (?fname . ?pos) . ?val)
		   (error/location proc "bibtex parse error" token fname pos))
		  (else
		   (notify-error proc mes obj)
		   (error proc mes obj))))))
      (else
       (with-exception-handler
	  (lambda (e)
	     (if (&io-parse-error? e)
		 (let ((o (&error-obj e)))
		    (match-case o
		       ((?token (?fname . ?pos) . ?val)
			(error/location (&error-proc e)
					"bibtex parse error"
					token
					fname
					pos))
		       (else
			(raise e))))
		 (raise e)))
	  (lambda ()
	     (read/lalrp bibtex-parser bibtex-lexer port))))))

;*---------------------------------------------------------------------*/
;*    the-coord ...                                                    */
;*---------------------------------------------------------------------*/
(define (the-coord port)
   (cons (input-port-name port) (input-port-position port)))

;*---------------------------------------------------------------------*/
;*    bibtex-lexer ...                                                 */
;*---------------------------------------------------------------------*/
(define bibtex-lexer
   (regular-grammar ((blank (in " \t\n")))
      ;; separators
      ((+ blank)
       (list 'BLANK (the-coord (the-port))))
      ;; comments
      ((: "%" (* all))
       (ignore))
      ;; egal sign
      (#\=
       (list 'EGAL (the-coord (the-port))))
      ;; sharp sign
      ((: (* blank) #\# (* blank))
       (list 'SHARP (the-coord (the-port))))
      ;; open bracket
      (#\{
       (list 'BRA-OPEN (the-coord (the-port))))
      ;; close bracket
      (#\}
       (list 'BRA-CLO (the-coord (the-port))))
      ;; comma
      (#\,
       (list 'COMMA (the-coord (the-port))))
      ;; double quote
      ((: #\\ (in "\"\\_"))
       (list 'CHAR (the-coord (the-port)) (the-character)))
      ;; optional linebreak
      ((: #\\ #\-)
       (ignore))
      ;; special latin characters
      ((or "{\\'e}" "\\'e")
       (list 'CHAR (the-coord (the-port)) "é"))
      ((or "{\\o}" "\\o")
       (list 'CHAR (the-coord (the-port)) "ø"))
      ((or "{\\~{n}}" "\\~{n}")
       (list 'CHAR (the-coord (the-port)) "ñ"))
      ((or "{\\~{N}}" "\\~{N}")
       (list 'CHAR (the-coord (the-port)) "Ñ"))
      ((or "{\\^{o}}" "\\^{o}")
       (list 'CHAR (the-coord (the-port)) "ô"))
      ((or "{\\^{O}}" "\\^{O}")
       (list 'CHAR (the-coord (the-port)) "Ô"))
      ((or "{\\\"{o}}" "\\\"{o}")
       (list 'CHAR (the-coord (the-port)) "ö"))
      ((or "{\\\"{O}}" "\\\"{O}")
       (list 'CHAR (the-coord (the-port)) "Ö"))
      ((or "{\\`e}" "\\`e")
       (list 'CHAR (the-coord (the-port)) "è"))
      ((or "{\\`a}" "\\`a")
       (list 'CHAR (the-coord (the-port)) "à"))
      ((or "{\\\"i}" "{\\\"{i}}" "\\\"i" "\\\"{i}")
       (list 'CHAR (the-coord (the-port)) "ï"))
      ((or "{\\\"u}" "\\\"u")
       (list 'CHAR (the-coord (the-port)) "ü"))
      ((or "{\\`u}" "\\`u")
       (list 'CHAR (the-coord (the-port)) "ù"))
      ;; latex commands
      ((: #\\ alpha (+ (or alpha digit)))
       (let ((s (the-substring 1 (the-length))))
	  (cond
	     ((member s '("pi" "Pi" "lambda" "Lambda"))
	      (list 'IDENT (the-coord (the-port)) s))
	     (else
	      (ignore)))))
      ;; strings
      ((: "\"" (* (or (out #a000 #\\ #\") (: #\\ all))) "\"")
       (list 'STRING
	     (the-coord (the-port))
	     (the-substring 1 (-fx (the-length) 1))))
      ;; commands
      ((: "@" (+ alpha))
       (let* ((str (string-upcase (the-substring 1 (the-length))))
	      (sym (string->symbol str)))
	  (case sym
	     ((STRING)
	      (list 'BIBSTRING (the-coord (the-port))))
	     (else
	      (list 'BIBITEM (the-coord (the-port)) sym)))))
      ;; digit
      ((+ digit)
       (list 'NUMBER (the-coord (the-port)) (the-string)))
      ;; ident
      ((+ (or alpha digit (in ".:-&/?+*")))
       (list 'IDENT (the-coord (the-port)) (the-string)))
      ;; default
      (else
       (let ((c (the-failure)))
	  (if (eof-object? c)
	      c
	      (list 'CHAR (the-coord (the-port)) c))))))

;*---------------------------------------------------------------------*/
;*    bibtex-parser ...                                                */
;*---------------------------------------------------------------------*/
(define bibtex-parser
   (lalr-grammar
      ;; tokens
      (CHAR IDENT STRING COMMA BRA-OPEN BRA-CLO SHARP BLANK NUMBER EGAL
	    BIBSTRING BIBITEM)
      
      ;; bibtex
      (bibtex
       (()
	'())
       ((bibtex string-def)
	bibtex)
       ((bibtex bibtex-entry)
	(cons bibtex-entry bibtex))
       ((bibtex BLANK)
	bibtex))
      
      ;; blank*
      (blank*
       (() '())
       ((blank* BLANK) '()))
      
      ;; string-def
      (string-def
       ((BIBSTRING BRA-OPEN blank* IDENT blank* EGAL blank* bibtex-entry-value BRA-CLO)
	(bibtex-string-def! (cadr IDENT) bibtex-entry-value)))
      
      ;; bibtex-entry
      (bibtex-entry
       ((BIBITEM blank* BRA-OPEN blank* IDENT blank* COMMA
		 bibtex-entry-item* BRA-CLO)
	(make-bibtex-entry (cadr BIBITEM)
			   (cadr IDENT)
			   bibtex-entry-item*)))
      
      ;; bibtex-entry-item*
      (bibtex-entry-item*
       ((blank*)
	'())
       ((bibtex-entry-item)
	(list bibtex-entry-item))
       ((bibtex-entry-item COMMA bibtex-entry-item*)
	(cons bibtex-entry-item bibtex-entry-item*)))
      
      ;; bibtex-entry-item
      (bibtex-entry-item
       ((blank* IDENT blank* EGAL blank* bibtex-entry-value blank*)
	(cons (cadr IDENT) bibtex-entry-value)))
      
      ;; bibtex-entry-value
      (bibtex-entry-value
       ((NUMBER)
	(list (cadr NUMBER)))
       ((bibtex-entry-value-string)
	bibtex-entry-value-string)
       ((BRA-OPEN bibtex-entry-value-block* BRA-CLO)
	bibtex-entry-value-block*))
      
      ;; bibtex-entry-value-string
      (bibtex-entry-value-string
       ((bibtex-entry-value-string-simple)
	(list bibtex-entry-value-string-simple))
       ((bibtex-entry-value-string SHARP bibtex-entry-value-string-simple)
	`(,@bibtex-entry-value-string ,bibtex-entry-value-string-simple)))
      
      ;; bibtex-entry-value-string-simple
      (bibtex-entry-value-string-simple
       ((STRING)
	(cadr STRING))
       ((IDENT)
	`(ref ,(cadr IDENT))))
      
      ;; bibtex-entry-value-block*
      (bibtex-entry-value-block*
       (()
	'())
       ((bibtex-entry-value-block* bibtex-entry-value-block)
	(append bibtex-entry-value-block* bibtex-entry-value-block)))
      
      ;; bibtex-entry-value-block
      (bibtex-entry-value-block
       ((BRA-OPEN bibtex-entry-value-block* BRA-CLO)
	bibtex-entry-value-block*)
       ((COMMA)
	(list ","))
       ((IDENT)
	(list (cadr IDENT)))
       ((BLANK)
	(list " "))
       ((EGAL)
	(list "="))
       ((CHAR)
	(list (cadr CHAR)))
       ((NUMBER)
	(list (cadr NUMBER)))
       ((STRING)
	(list (string-append "\"" (cadr STRING) "\""))))))

;*---------------------------------------------------------------------*/
;*    bibtex-string-def! ...                                           */
;*---------------------------------------------------------------------*/
(define (bibtex-string-def! ident value)
   (define (->string value)
      (if (string? value)
	  value
	  (match-case value
	     (((and ?s (? string?)))
	      s)
	     (((and ?n (? number?)))
	      (number->string n))
	     (else
	      (apply string-append (map ->string value))))))
   (hashtable-put! *bibtex-string-table* ident (->string value)))

;*---------------------------------------------------------------------*/
;*    make-bibtex-entry ...                                            */
;*---------------------------------------------------------------------*/
(define (make-bibtex-entry kind ident value)
   (define (parse-entry-value line)
      (let ((name (car line))
	    (val (cdr line)))
	 (let loop ((val (reverse val))
		    (res ""))
	    (cond
	       ((null? val)
		(cons name (untexify res)))
	       ((char? (car val))
		(loop (cdr val) (string-append (string (car val)) res)))
	       ((string? (car val))
		(loop (cdr val) (string-append (car val) res)))
	       (else
		(match-case (car val)
		   ((ref ?ref)
		    (let ((h (hashtable-get *bibtex-string-table* ref)))
		       (loop (cdr val)
			     (if (string? h)
				 (string-append h res)
				 res))))
		   (else
		    (loop (cdr val) res))))))))
   (let ((fields (map parse-entry-value value)))
      `(,kind ,ident ,@fields)))

;*---------------------------------------------------------------------*/
;*    untexify ...                                                     */
;*---------------------------------------------------------------------*/
(define (untexify val)
   (define (untexify-math-string str)
      (string-case str
	 ((+ (out #\_ #\^ #\space #\Newline #\tab))
	  (let ((s (the-string)))
	     (string-append s (ignore))))
	 ((+ (in "^_"))
	  (ignore))
	 ((+ (in " \n\t"))
	  (string-append " " (ignore)))
	 (else
	  "")))
   (define (untexify-string str)
      (let ((s (pregexp-replace* "C[$]\\^[$]_[+][+][$][$]" str "C++")))
	 (string-case (pregexp-replace* "[{}]" s "")
	    ((+ (out #\\ #\$ #\space #\Newline #\tab #\~))
	     (let ((s (the-string)))
		(string-append s (ignore))))
	    ((: #\\ (+ (or (: "c" (out #\h))
			   (: "ch" (out #\a))
			   (: "cha" (out #\r))
			   (: "char" (out digit))
			   (out #\\ #\space #\c))))
	     (ignore))
	    ((: #\\ "char" (+ digit))
	     (string-append
	      (string
	       (integer->char
		(string->integer
		 (the-substring 5 (the-length)))))
	      (ignore)))
	    ((: #\$ (* (out #\$)) #\$)
	     (let ((s (the-substring 1 (-fx (the-length) 1))))
		(string-append (untexify-math-string s) (ignore))))
	    ((+ (in " \n\t~"))
	     (string-append " " (ignore)))
	    (else
	     ""))))
   (if (string? val)
       (untexify-string val)
       (map untexify val)))
