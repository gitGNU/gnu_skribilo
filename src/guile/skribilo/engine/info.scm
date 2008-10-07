;*=====================================================================*/
;*    serrano/prgm/project/scribe/scribetext/info.scm                  */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Sep 23 14:03:53 2001                          */
;*    Last change :  Mon Oct 21 10:59:41 2002 (serrano)                */
;*    Copyright   :  2001-02 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    The translator scribe->text                                      */
;*=====================================================================*/


;*---------------------------------------------------------------------*/
;*    info-dest ...                                                    */
;*---------------------------------------------------------------------*/
(define (info-dest)
   (if (string? *scribe-dest*)
       *scribe-dest*
       "anonymous.info"))

;*---------------------------------------------------------------------*/
;*    info-node ...                                                    */
;*---------------------------------------------------------------------*/
(define (info-node node next prev up)
   (print "")
   (print "File: " (info-dest)
	  ",  Node: " node
	  ",  Next: " next
	  ",  Prev: " prev
	  ",  Up: " up)
   (newline))

;*---------------------------------------------------------------------*/
;*    node-next+prev+top ::%container ...                              */
;*---------------------------------------------------------------------*/
(define-generic (node-next+prev+top obj::%container))

;*---------------------------------------------------------------------*/
;*    node-next+prev+top ::%document ...                               */
;*---------------------------------------------------------------------*/
(define-method (node-next+prev+top obj::%document)
   (with-access::%container obj (children)
      (let loop ((c children))
	 (cond
	    ((null? c)
	     (values "Top" "(dir)" "(dir)"))
	    ((or (%chapter? (car c)) (%section? (car c)))
	     (values (block-title (car c)) "(dir)" "(dir)"))
	    (else
	     (loop (cdr c)))))))

;*---------------------------------------------------------------------*/
;*    node-next+prev+top ...                                           */
;*---------------------------------------------------------------------*/
(define-method (node-next+prev+top obj::%block)
   (with-access::%block obj (parent)
      (let ((top (if (%document? parent)
		     "Top"
		     (block-title parent))))
	 (let loop ((els (%container-children parent))
		    (prev #f))
	    (cond
	       ((null? els)
		(values top top top))
	       ((eq? (car els) obj)
		(let ((p (if prev
			     (block-title prev)
			     top))
		      (n (if (null? (cdr els))
			     top
			     (block-title (cadr els)))))
		   (values p n top)))
	       (else
		(loop (cdr els) (car els))))))))

;*---------------------------------------------------------------------*/
;*    node-menu ...                                                    */
;*---------------------------------------------------------------------*/
(define (node-menu obj::%container)
   (with-access::%container obj (children)
      (if (pair? (filter (lambda (x) (or (%chapter? x) (%section? x)))
			 children))
	  (begin
	     (newline)
	     (print "* Menu:")
	     (newline)
	     (for-each (lambda (c)
			  (if (%block? c)
			      (print "* " (block-title c) "::")))
		       (reverse children))))
      (newline)))

;*---------------------------------------------------------------------*/
;*    block-title ::%block ...                                         */
;*---------------------------------------------------------------------*/
(define-generic (block-title obj::%block)
   "")

;*---------------------------------------------------------------------*/
;*    block-title ::%chapter ...                                       */
;*---------------------------------------------------------------------*/
(define-method (block-title obj::%chapter)
   (with-access::%chapter obj (title subtitle)
      (let ((title (if title title subtitle)))
	 (if (string? title)
	     title
	     (with-output-to-string 
		(lambda () (info title)))))))

;*---------------------------------------------------------------------*/
;*    block-title ::%section ...                                       */
;*---------------------------------------------------------------------*/
(define-method (block-title obj::%section)
   (with-access::%section obj (title)
      (if (string? title)
	  title
	  (with-output-to-string 
	     (lambda () (info title))))))

;*---------------------------------------------------------------------*/
;*    block-title ::%subsection ...                                    */
;*---------------------------------------------------------------------*/
(define-method (block-title obj::%subsection)
   (with-access::%subsection obj (title)
      (if (string? title)
	  title
	  (with-output-to-string 
	     (lambda () (info title))))))

;*---------------------------------------------------------------------*/
;*    block-title ::%subsection ...                                    */
;*---------------------------------------------------------------------*/
(define-method (block-title obj::%subsubsection)
   (with-access::%subsubsection obj (title)
      (if (string? title)
	  title
	  (with-output-to-string 
	     (lambda () (info title))))))

;*---------------------------------------------------------------------*/
;*    *text-string-processor* ...                                      */
;*---------------------------------------------------------------------*/
(define *text-string-processor*
   (lambda (x) x))

;*---------------------------------------------------------------------*/
;*    info ::obj ...                                                   */
;*---------------------------------------------------------------------*/
(define-generic (info obj::obj)
   (cond
      ((and (procedure? obj) (correct-arity? obj 0))
       (info (obj)))
      ((string? obj)
       (output (*text-string-processor* obj)))
      ((number? obj)
       (output (*text-string-processor* (number->string obj))))
      ((char? obj)
       (output (*text-string-processor* (string obj))))
      ((eq? obj #unspecified)
       obj)
      ((list? obj)
       (for-each info obj))
      ((or (symbol? obj) (boolean? obj))
       "")
      (else
       (with-access::%node obj (loc)
	  (error/location "info"
			  "Can't find method for node"
			  (find-runtime-type obj)
			  (car loc)
			  (cdr loc))))))

;*---------------------------------------------------------------------*/
;*    info ::%document ...                                             */
;*---------------------------------------------------------------------*/
(define-method (info obj::%document)
   (with-document
    obj
    (lambda ()
       (with-access::%document obj (title authors body footnotes)
	  (scribe-document->info obj (if title title "") authors body)
	  (if (pair? footnotes)
	      (begin
		 (with-justification
		  (make-justifier *text-column-width* 'left)
		  (lambda ()
		     (newline)
		     (newline)
		     (print "-------------")
		     (for-each (lambda (fn)
				  (with-access::%footnote fn (number note id)
				     (output (string-append
					      "*"
					      (number->string number)
					      ": "))
				     (info note)
				     (output-newline)))
			       footnotes)))))))))

;*---------------------------------------------------------------------*/
;*     scribe-document->info ...                                       */
;*---------------------------------------------------------------------*/
(define (scribe-document->info obj title authors body)
   (define (info-authors1 author)
      (info author)
      (output-newline)
      (output-newline))
   (define (info-authorsN authors cols first)
      (define (make-row authors . opt)
	 (apply tr (map (lambda (v)
			   (apply td :align 'center :valign 'top v opt))
			authors)))
      (define (make-rows authors)
	 (let loop ((authors authors)
		    (rows '())
		    (row '())
		    (cnum 0))
	    (cond
	       ((null? authors)
		(reverse! (cons (make-row (reverse! row)) rows)))
	       ((= cnum cols)
		(loop authors
		      (cons (make-row (reverse! row)) rows)
		      '()
		      0))
	       (else
		(loop (cdr authors)
		      rows
		      (cons (car authors) row)
		      (+fx cnum 1))))))
      (info (apply table
		    (if first
			(cons (make-row (list (car authors)) :colspan cols)
			      (make-rows (cdr authors)))
			(make-rows authors)))))
   (define (info-authors authors)
      (if (pair? authors)
	  (begin
	     (output-newline)
	     (output "--o-0-o--")
	     (output-newline)
	     (output-newline)
	     (let ((len (length authors)))
		(case len
		   ((1)
		    (info-authors1 (car authors)))
		   ((2 3)
		    (info-authorsN authors len #f))
		   ((4)
		    (info-authorsN authors 2 #f))
		   (else
		    (info-authorsN authors 3 #t)))))))
   ;; display the title and the authors
   (define (info-title title authors)
      (with-justification
       (make-justifier (justification-width) 'center)
       (lambda ()
	  (output (make-string *text-column-width* #\=))
	  (output-newline)
	  (if (string? title)
	      (output (list->string
		       (apply append
			      (map (lambda (c) (list c #a008))
				   (string->list title)))))
	      (info title))
	  (output-newline)
	  (info-authors authors)
	  (output (make-string *text-column-width* #\=))
	  (output-newline)
	  (output-newline)
	  (output-flush *margin*))))
;; display the footer
   (define (info-footer)
      (if *scribe-footer* (info *scribe-footer*)))
   ;; the main node
   (multiple-value-bind (next prev top)
      (node-next+prev+top obj)
      (newline)
      (info-node "Top" next prev top))
   ;; the title
   (info-title title authors)
   (output-flush 0)
   ;; the main info menu
   (node-menu obj)
   ;; the body
   (info body)
   (output-flush 0)
   ;; the footer of the document
   (info-footer)
   (output-flush 0)
   ;; we are done
   (newline)
   (newline))

;*---------------------------------------------------------------------*/
;*    info ::%author ...                                               */
;*---------------------------------------------------------------------*/
(define-method (info obj::%author)
   (with-access::%author obj (name title affiliation email url address phone)
      (if (or (pair? name) (string? name))
	  (info name))
      (if title (begin (output-newline) (output title)))
      (if affiliation (begin (output-newline) (output affiliation)))
      (if (pair? address)
	  (for-each (lambda (x) (output-newline) (output x)) address))
      (if email (begin (output-newline) (output email)))
      (if url (begin (output-newline) (output url)))
      (if phone (begin (output-newline) (output phone)))
      (output-newline)))
   
;*---------------------------------------------------------------------*/
;*    scribe->html ::%toc ...                                          */
;*---------------------------------------------------------------------*/
(define-method (info obj::%toc)
   (node-menu (current-document)))

;*---------------------------------------------------------------------*/
;*    info ::%text ...                                                 */
;*---------------------------------------------------------------------*/
(define-method (info obj::%text)
   (info (%text-body obj)))

;*---------------------------------------------------------------------*/
;*    info ::%linebreak ...                                            */
;*---------------------------------------------------------------------*/
(define-method (info obj::%linebreak)
   (let loop ((num (%linebreak-repetition obj)))
      (output-newline)
      (if (>fx num 1)
	  (begin
	     (output-newline)
	     (loop (-fx num 1))))))

;*---------------------------------------------------------------------*/
;*    info ::%center ...                                               */
;*---------------------------------------------------------------------*/
(define-method (info obj::%center)
   (with-justification (make-justifier (justification-width) 'center)
		       (lambda ()
			  (info (%center-body obj)))))

;*---------------------------------------------------------------------*/
;*    info ::%flush ...                                                */
;*---------------------------------------------------------------------*/
(define-method (info obj::%flush)
   (with-access::%flush obj (side)
      (with-justification (make-justifier (justification-width) side)
			  (lambda ()
			     (info (%flush-body obj))))))

;*---------------------------------------------------------------------*/
;*    info ::%atom ...                                                 */
;*---------------------------------------------------------------------*/
(define-method (info obj::%atom)
   (output (%atom-value obj)))

;*---------------------------------------------------------------------*/
;*    *ornaments* ...                                                  */
;*---------------------------------------------------------------------*/
(define *ornaments*
   `((bold "{\\textbf{" "}}")
     (emph "*" "*")
     (underline "_" "_")
     (it "{\\textit{" "}}")
     (samp "{\\textit{" "}}")
     (sc "{\\sc{" "}}")
     (sup "^" "")
     (sub "_" "")
     (code "`" "'")
     (samp "`" "'")))

;*---------------------------------------------------------------------*/
;*    info ::%ornament ...                                             */
;*---------------------------------------------------------------------*/
(define-method (info obj::%ornament)
   (with-access::%ornament obj (body kind)
      (case kind
	 ((var)
	  (let ((old *text-string-processor*))
	     (set! *text-string-processor* string-upcase)
	     (let ((res (info body)))
		(set! *text-string-processor* old)
		res)))
	 (else
	  (let ((d (assq kind *ornaments*)))
	     (if (not (pair? d))
		 (info body)
		 (let ((start (cadr d))
		       (stop (caddr d)))
		    (display start)
		    (info body)
		    (display stop))))))))
			  
;*---------------------------------------------------------------------*/
;*    info ::%pre ...                                                  */
;*---------------------------------------------------------------------*/
(define-method (info obj::%pre)
   (with-justification (make-justifier *text-column-width* 'verbatim)
		       (lambda ()
			  (info (%pre-body obj))
			  (output-newline))))

;*---------------------------------------------------------------------*/
;*    info ::%mark ...                                                 */
;*---------------------------------------------------------------------*/
(define-method (info obj::%mark)
   #unspecified)

;*---------------------------------------------------------------------*/
;*    info ::%reference ...                                            */
;*---------------------------------------------------------------------*/
(define-method (info obj::%reference)
   (with-access::%reference obj (body anchor)
      (multiple-value-bind (file mark)
	 (find-reference obj (current-document))
	 (if (not mark)
	     (begin
		(warning "ref" "Can't find reference -- " anchor)
		(output "reference:???"))
	     (begin
		(output "*Note ")
		(info body)
		(output ":: "))))))

;*---------------------------------------------------------------------*/
;*    info ::%sui-ref ...                                              */
;*---------------------------------------------------------------------*/
(define-method (info obj::%sui-ref)
   (info (scribe-url-ref obj)))

;*---------------------------------------------------------------------*/
;*    info ::%url-ref ...                                              */
;*---------------------------------------------------------------------*/
(define-method (info obj::%url-ref)
   (with-access::%url-ref obj (url anchor body)
      (if (and body (not (equal? body url)))
	  (begin
	     (output "*Note ")
	     (info body)
	     (output " (")))
      (info url)
      (if (or (pair? anchor)
	      (and (string? anchor) (>fx (string-length anchor) 0)))
	  (begin
	     (output "#")
	     (info anchor)))
      (if (and body (not (equal? body url))) (output ")"))
      (output ":: ")))
   
;*---------------------------------------------------------------------*/
;*    info ::%chapter-ref ...                                          */
;*---------------------------------------------------------------------*/
(define-method (info obj::%chapter-ref)
   (multiple-value-bind (_ chapter)
      (find-reference obj (current-document))
      (if (not chapter)
	  (with-access::%chapter-ref obj (anchor)
	     (warning "ref" "Can't find chapter -- " anchor)
	     (output "chapter:???"))
	  (info-chapter-ref chapter))))

;*---------------------------------------------------------------------*/
;*    info-chapter-ref ...                                             */
;*---------------------------------------------------------------------*/
(define (info-chapter-ref obj::%chapter)
   (output "*Note ")
   (output (block-title obj))
   (output ":: "))

;*---------------------------------------------------------------------*/
;*    info ::%section-ref ...                                          */
;*---------------------------------------------------------------------*/
(define-method (info obj::%section-ref)
   (multiple-value-bind (_ section)
      (find-reference obj (current-document))
      (if (not (%section? section))
	  (with-access::%section-ref obj (anchor)
	     (warning "ref" "Can't find section -- " anchor)
	     (output "section:???"))
	  (info-section-ref section))))

;*---------------------------------------------------------------------*/
;*    info-section-ref ...                                             */
;*---------------------------------------------------------------------*/
(define (info-section-ref obj::%section)
   (with-access::%section obj (title)
      (output "*Note ")
      (output title)
      (output ":: ")))
   
;*---------------------------------------------------------------------*/
;*    info ::%subsection-ref ...                                       */
;*---------------------------------------------------------------------*/
(define-method (info obj::%subsection-ref)
   (multiple-value-bind (_ subsection)
      (find-reference obj (current-document))
      (if (not (%subsection? subsection))
	  (with-access::%subsection-ref obj (anchor)
	     (warning "ref" "Can't find subsection -- " anchor)
	     (output "subsection:???"))
	  (info-subsection-ref subsection))))

;*---------------------------------------------------------------------*/
;*    info-subsection-ref ...                                          */
;*---------------------------------------------------------------------*/
(define (info-subsection-ref obj::%subsection)
   (with-access::%subsection obj (title)
      (output "*Note ")
      (output title)
      (output ":: ")))
   
;*---------------------------------------------------------------------*/
;*    info ::%subsubsection-ref ...                                    */
;*---------------------------------------------------------------------*/
(define-method (info obj::%subsubsection-ref)
   (multiple-value-bind (_ subsubsection)
      (find-reference obj (current-document))
      (if (not (%subsubsection? subsubsection))
	  (with-access::%subsubsection-ref obj (anchor)
	     (warning "ref" "Can't find subsubsection -- " anchor)
	     (output "subsubsection:???"))
	  (info-subsubsection-ref subsubsection))))

;*---------------------------------------------------------------------*/
;*    info-subsubsection-ref ...                                       */
;*---------------------------------------------------------------------*/
(define (info-subsubsection-ref obj::%subsubsection)
   (with-access::%subsubsection obj (title)
      (output "*Note ")
      (output title)
      (output ":: ")))

;*---------------------------------------------------------------------*/
;*    info ::%biblio-ref ...                                           */
;*---------------------------------------------------------------------*/
(define-method (info obj::%biblio-ref)
   (with-access::%biblio-ref obj (anchor body)
      (if body (info body))
      (output " [")
      (let loop ((a+ anchor))
	 (if (null? a+)
	     (output "]")
	     (let ((a (car a+)))
		(cond
		   ((%bibentry? a)
		    (output (number->string (%bibentry-number a))))
		   ((string? a)
		    (output "???")
		    (output a)
		    (output "???"))
		   (else
		    (display "bibref:???")))
		(if (pair? (cdr a+))
		    (output ","))
		(loop (cdr a+)))))))

;*---------------------------------------------------------------------*/
;*    mailto ...                                                       */
;*---------------------------------------------------------------------*/
(define-method (info obj::%mailto)
   (with-access::%mailto obj (email body)
      (if (pair? body)
	  (info body)
	  (output email))))

;*---------------------------------------------------------------------*/
;*    info ::%item ...                                                 */
;*---------------------------------------------------------------------*/
(define-method (info obj::%item)
   (with-access::%item obj (value body)
      (if (not (null? value))
	  (begin
	     (info value)
	     (display ": ")))
      (info body)))

;*---------------------------------------------------------------------*/
;*    info ::%list ...                                                 */
;*---------------------------------------------------------------------*/
(define-method (info obj::%list)
   (with-access::%list obj (items kind)
      (case kind
	 ((itemize)
	  (for-each (lambda (item)
		       (with-justification (make-justifier
					    (-fx (justification-width) 3)
					    'left)
					   (lambda ()
					      (output "- ")
					      (info item))
					   3))
		    items))
	 ((enumerate)
	  (let loop ((num 1)
		     (items items))
	     (if (pair? items)
		 (let ((item (car items)))
		    (with-justification (make-justifier
					 (-fx (justification-width) 3)
					 'left)
					(lambda ()
					   (output (integer->string num))
					   (output " - ")
					   (info item))
					3)
		    (loop (+fx num 1) (cdr items))))))
	 ((description)
	  (for-each (lambda (item)
		       (with-justification
			(make-justifier
			 (-fx (justification-width) 3)
			 'left)
			(lambda ()
			   (with-access::%item item (value body)
			      (output "*")
			      (if (pair? value)
				  (let loop ((vs value))
				     (info (car vs))
				     (if (pair? (cdr vs))
					 (begin
					    (output " ")
					    (loop (cdr vs)))))
				  (info value))
			      (output "* ")
			      (info body)))
			3))
		    items))
	 (else
	  (error "info" "Illegal list" kind)))))
      
;*---------------------------------------------------------------------*/
;*    info ::%section ...                                              */
;*---------------------------------------------------------------------*/
(define-method (info obj::%section)
   (with-access::%section obj (body title)
      (output-newline)
      (output-flush *margin*)
      (let ((t (block-title obj)))
	 (multiple-value-bind (next prev top)
	    (node-next+prev+top obj)
	    (info-node t next prev top)
	    (print t)
	    (print (make-string (string-length t) #\=))))
      (node-menu obj)
      (with-justification (make-justifier *text-column-width*
					  *text-justification*)
			  (lambda () (info body)))))

;*---------------------------------------------------------------------*/
;*    info ::%subsection ...                                           */
;*---------------------------------------------------------------------*/
(define-method (info obj::%subsection)
   (with-access::%subsection obj (body title)
      (output-flush *margin*)
      (let ((t (block-title obj)))
	 (multiple-value-bind (next prev top)
	    (node-next+prev+top obj)
	    (info-node t next prev top)
	    (print t)
	    (print (make-string (string-length t) #\-))))
      (info body)))

;*---------------------------------------------------------------------*/
;*    info ::%subsubsection ...                                        */
;*---------------------------------------------------------------------*/
(define-method (info obj::%subsubsection)
   (with-access::%subsubsection obj (body title)
      (output-flush *margin*)
      (let ((t (block-title obj)))
	 (multiple-value-bind (next prev top)
	    (node-next+prev+top obj)
	    (info-node t next prev top)
	    (print t)
	    (print (make-string (string-length t) #\~))))
      (info body)))

;*---------------------------------------------------------------------*/
;*    info ::%paragraph ...                                            */
;*---------------------------------------------------------------------*/
(define-method (info obj::%paragraph)
   (with-access::%paragraph obj (body)
      (output-newline)
      (output-flush *margin*)
      (info body)))

;*---------------------------------------------------------------------*/
;*    info ::%chapter ...                                              */
;*---------------------------------------------------------------------*/
(define-method (info obj::%chapter)
   (with-access::%chapter obj (body file title subtitle)
      (output-newline)
      (output-flush *margin*)
      (let ((t (block-title obj)))
	 (multiple-value-bind (next prev top)
	    (node-next+prev+top obj)
	    (info-node t next prev top)
	    (print t)
	    (print (make-string (string-length t) #\*))))
      (node-menu obj)
      (info body)))

;*---------------------------------------------------------------------*/
;*    info ::%hrule ...                                                */
;*---------------------------------------------------------------------*/
(define-method (info obj::%hrule)
   (with-access::%hrule obj (width)
      (let ((w (if (= width 100)
		   (justification-width)
		   (inexact->exact (* (exact->inexact (justification-width))
				      (/ (exact->inexact width) 100.))))))
	 (output (make-string w #\-)))))

;*---------------------------------------------------------------------*/
;*    info ::%font ...                                                 */
;*---------------------------------------------------------------------*/
(define-method (info obj::%font)
   (with-access::%font obj (body)
      (info body)))

;*---------------------------------------------------------------------*/
;*    info ::%image ...                                                */
;*---------------------------------------------------------------------*/
(define-method (info obj::%image)
   #unspecified)

;*---------------------------------------------------------------------*/
;*    info ::%table ...                                                */
;*---------------------------------------------------------------------*/
(define-method (info obj::%table)
   (with-access::%table obj (border loc)
      (output-flush *margin*)
      (if border
	  (border-table->info obj)
	  (table->ascii obj info))
      (output-flush *margin*)))

;*---------------------------------------------------------------------*/
;*    border-table->info ...                                           */
;*---------------------------------------------------------------------*/
(define (border-table->info table)
   (table->ascii table info))

;*---------------------------------------------------------------------*/
;*    info ::%character ...                                            */
;*---------------------------------------------------------------------*/
(define-method (info obj::%character)
   (case (%character-value obj)
      ((copyright)
       (display "(c)"))
      ((#\space)
       (display #\space))
      ((#\tab)
       (display #\tab))))

;*---------------------------------------------------------------------*/
;*    info ::%hook ...                                                 */
;*---------------------------------------------------------------------*/
(define-method (info obj::%hook)
   (with-access::%hook obj (body before after process)
      (if (procedure? before)
	  (let ((bef (before)))
	     (if process (info bef))))
      (call-next-method)
      (if (procedure? after)
	  (let ((af (after)))
	     (if process (info af))))))

;*---------------------------------------------------------------------*/
;*    info ::%figure ...                                               */
;*---------------------------------------------------------------------*/
(define-method (info obj::%figure)
   (with-access::%figure obj (body legend number)
      (output-newline)
      (info body)
      (output-newline)
      (output-newline)
      (output "Fig. ")
      (output (number->string number))
      (output ": ")
      (info legend)
      (output-newline)))

;*---------------------------------------------------------------------*/
;*    info ::%footnote ...                                             */
;*---------------------------------------------------------------------*/
(define-method (info obj::%footnote)
   (with-access::%footnote obj (number note body)
      (info body)
      (output (string-append "(*" (number->string number) ")"))))



;=============== ~/prgm/project/scribe/scribetext/justify.scm ================

;-----|-----|-----|-----|-----|-----|-----|-----|-----|-----|-----|-----|-----
;*=====================================================================*/
;*    serrano/prgm/project/scribe/scribetext/justify.scm               */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Nov  1 09:21:20 2001                          */
;*    Last change :  Sun Dec  9 14:59:11 2001 (serrano)                */
;*    Copyright   :  2001 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    The justifiers                                                   */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __scribetext_justify
   
   (export (make-justifier::procedure ::int ::symbol)
	   (output-flush ::int)
	   
	   *text-column-width*
	   *text-justification*
	   *margin*
	   
	   (output ::bstring)
	   (output-token ::bstring)
	   (output-center ::bstring)
	   (output-newline)
	   (justification-width::int)
	   (with-justification ::procedure ::procedure . margin)
	   (with-justification/noflush ::procedure ::procedure . margin))
   
   (eval   (export *text-column-width*)
	   (export *text-justification*)))

;*---------------------------------------------------------------------*/
;*    *text-column-width* ...                                          */
;*---------------------------------------------------------------------*/
(define *text-column-width* 79)
(define *text-justification* 'left)

;*---------------------------------------------------------------------*/
;*    text-string ...                                                  */
;*---------------------------------------------------------------------*/
(define (text-string str)
   (let ((len (string-length str)))
      (let loop ((r 0))
	 (cond
	    ((=fx r len)
	     str)
	    ((char=? (string-ref str r) #a008)
	     (string-set! str r #\Space)
	     (loop (+fx r 1)))
	    (else
	     (loop (+fx r 1)))))))

;*---------------------------------------------------------------------*/
;*    string-replace ...                                               */
;*---------------------------------------------------------------------*/
(define (string-replace str1 c1 c2)
   (let* ((len (string-length str1))
	  (str2 (make-string len)))
      (let loop ((r 0))
	 (if (=fx r len)
	     str2
	     (let ((c (string-ref str1 r)))
		(if (char=? c c1)
		    (string-set! str2 r c2)
		    (string-set! str2 r c))
		(loop (+fx r 1)))))))

;*---------------------------------------------------------------------*/
;*    output-center ...                                                */
;*---------------------------------------------------------------------*/
(define (output-center str)
   (let ((justifier (make-justifier (justification-width) 'center)))
      (with-justification justifier
			  (lambda ()
			     (output str)))))

;*---------------------------------------------------------------------*/
;*    *justifiers* ...                                                 */
;*---------------------------------------------------------------------*/
(define *justifiers* (list (make-justifier *text-column-width*
					   *text-justification*)))
(define *margin* 0)

;*---------------------------------------------------------------------*/
;*    output ...                                                       */
;*---------------------------------------------------------------------*/
(define (output str)
   ((car *justifiers*) 'output str))

;*---------------------------------------------------------------------*/
;*    output-token ...                                                 */
;*    -------------------------------------------------------------    */
;*    Display one string as if it is one token. No matter if it        */
;*    contains #\spaces.                                               */
;*---------------------------------------------------------------------*/
(define (output-token str)
   ((car *justifiers*) 'output (string-replace str #\space #a008)))

;*---------------------------------------------------------------------*/
;*    output-newline ...                                               */
;*---------------------------------------------------------------------*/
(define (output-newline)
   ((car *justifiers*) 'newline))
   
;*---------------------------------------------------------------------*/
;*    pre-output ...                                                   */
;*---------------------------------------------------------------------*/
(define (pre-output val)
   ((car *justifiers*) 'pre val))
   
;*---------------------------------------------------------------------*/
;*    post-output ...                                                  */
;*---------------------------------------------------------------------*/
(define (post-output val)
   ((car *justifiers*) 'post val))
   
;*---------------------------------------------------------------------*/
;*    output-flush ...                                                 */
;*---------------------------------------------------------------------*/
(define (output-flush margin)
   (for-each (if (>fx margin 0)
		 (let ((m (make-string margin #\space)))
		    (lambda (x) (print m (text-string x))))
		 (lambda (x) (print (text-string x))))
	     ((car *justifiers*) 'flush)))

;*---------------------------------------------------------------------*/
;*    justification-width ...                                          */
;*---------------------------------------------------------------------*/
(define (justification-width)
   ((car *justifiers*) 'width))

;*---------------------------------------------------------------------*/
;*    with-justification ...                                           */
;*---------------------------------------------------------------------*/
(define (with-justification justifier thunk . margin)
   (output-flush *margin*)
   (let ((old-margin *margin*))
      (if (pair? margin) (set! *margin* (+fx *margin* (car margin))))
      (set! *justifiers* (cons justifier *justifiers*))
      (thunk)
      (output-flush *margin*)
      (set! *justifiers* (cdr *justifiers*))
      (set! *margin* old-margin)))

;*---------------------------------------------------------------------*/
;*    with-justification/noflush ...                                   */
;*---------------------------------------------------------------------*/
(define (with-justification/noflush justifier thunk . margin)
   (let ((old-margin *margin*))
      (if (pair? margin) (set! *margin* (+fx *margin* (car margin))))
      (set! *justifiers* (cons justifier *justifiers*))
      (thunk)
      (let ((res ((car *justifiers*) 'flush)))
	 (set! *justifiers* (cdr *justifiers*))
	 (set! *margin* old-margin)
	 res)))

;*---------------------------------------------------------------------*/
;*    *spaces* ...                                                     */
;*---------------------------------------------------------------------*/
(define *spaces* '(#\Space #\Tab #\Newline))

;*---------------------------------------------------------------------*/
;*    strtok ...                                                       */
;*---------------------------------------------------------------------*/
(define (strtok str delims)
   (reverse (kotrts str delims)))

;*---------------------------------------------------------------------*/
;*    kotrts ...                                                       */
;*---------------------------------------------------------------------*/
(define (kotrts str::bstring delims::pair)
   (let ((stop (string-length str)))
      (let loop ((cur  0)
                 (mark #f)
                 (acc  '()))
         (cond
            ((= cur stop)
             (if (number? mark)
		 (cons (substring str mark cur) acc)
                 acc))
            ((memq (string-ref str cur) delims)
             (loop (+ cur 1)
                   #f
                   (if (number? mark)
                       (cons (substring str mark cur)
                             acc)
                       acc)))
            (else
             (loop (+ cur 1)
                   (if (number? mark) mark cur)
                   acc))))))

;*---------------------------------------------------------------------*/
;*    string-insert! ...                                               */
;*---------------------------------------------------------------------*/
(define (string-insert! str-to::bstring str-from::bstring offset::int)
   (let ((len1 (string-length str-to))
         (len2 (string-length str-from)))
      (if (> (+ len2 offset) len1)
          (error "string-insert!" "String too long" str-from)
          (let loop ((i 0))
             (if (= i len2)
                 str-to
                 (begin
                    (string-set! str-to
                                 (+ i offset)
                                 (string-ref str-from i))
                    (loop (+ i 1))))))))

;*---------------------------------------------------------------------*/
;*    make-justified-line ...                                          */
;*---------------------------------------------------------------------*/
(define (make-justified-line tokens::pair-nil width::int)
   (let ((result (make-string width #\space)))
      (cond
         ((null? tokens)
          result)
         ((null? (cdr tokens))
          (string-insert! result (car tokens) 0))
         (else
          (let* ((nb-tokens  (length tokens))
                 (nb-chars   (apply + (map string-length
                                           tokens)))
                 (all-spaces (- width nb-chars))
                 (one-spaces (/ all-spaces
                                (- nb-tokens 1)))
                 (cursor     (string-length (car tokens))))
             (string-insert! result (car tokens) 0)
             (let loop ((tokens (cdr tokens))
                        (cursor cursor))
                (if (null? (cdr tokens))
                    (let* ((len (string-length
                                 (car tokens)))
                           (cursor (- width len)))
                       (string-insert! result
                                       (car tokens)
                                       cursor)
                       result)
                    (let* ((token    (car tokens))
                           (token-ln (string-length token))
                           (n-cursor (+ cursor
                                        token-ln
                                        one-spaces))
                           (offset   (inexact->exact
                                      (round
                                       (+ cursor
                                          one-spaces)))))
                       (string-insert! result token offset)
                       (loop (cdr tokens) n-cursor)))))))))

;*---------------------------------------------------------------------*/
;*    make-formated-line ...                                           */
;*---------------------------------------------------------------------*/
(define (make-formated-line tokens::pair-nil width::int cursor::int)
   (let ((result (make-string width #\space)))
      (if (null? tokens)
          result
          (let loop ((toks tokens)
                     (cur cursor))
             (if (null? toks)
                 result
                 (begin
                    (string-insert! result (car toks) cur)
                    (loop (cdr toks)
                          (+ 1
                             cur
                             (string-length
			      (car toks))))))))))

;*---------------------------------------------------------------------*/
;*    make-centered-line ...                                           */
;*---------------------------------------------------------------------*/
(define (make-centered-line tokens::pair-nil width::int)
   (make-formated-line tokens
		       width
		       (quotient (- width
				    (+ (apply + (map string-length tokens))
				       (- (length tokens) 1)))
				 2)))

;*---------------------------------------------------------------------*/
;*    make-flushleft-line ...                                          */
;*---------------------------------------------------------------------*/
(define (make-flushleft-line tokens::pair-nil width::int)
   (make-formated-line tokens width 0))

;*---------------------------------------------------------------------*/
;*    make-flushright-line ...                                         */
;*---------------------------------------------------------------------*/
(define (make-flushright-line tokens::pair-nil width::int)
   (make-formated-line tokens
		       width
		       (- width
			  (+ (apply + (map string-length tokens))
			     (- (length tokens) 1)))))

;*---------------------------------------------------------------------*/
;*    tokens-justify ...                                               */
;*---------------------------------------------------------------------*/
(define (tokens-justify justifier::procedure tokens::pair-nil width::int)
   (define (reverse-line lines)
      (let ((nl (string #\Newline)))
         (let loop ((ls lines)
                    (acc ""))
            (if (null? ls)
                acc
                (loop (cdr ls) (string-append (car ls) nl acc))))))
   (let loop ((tokens    tokens)
              (line-len  0)
              (line     '())
              (acc      '()))
      (if (null? tokens)
          (reverse! (cons (justifier (reverse line) width) acc))
          (let ((tok (car tokens)))
	     (cond
		((eq? tok 'NEWLINE)
		 (loop (cdr tokens)
		       0
		       '()
		       (cons (justifier (reverse line) width) acc)))
		(else
		 (let ((toklen (string-length tok)))
		    (cond
		       ((>= toklen width)
			(let ((jl (justifier (list (substring tok 0 width))
					     width))
			      (ll (if (pair? line)
				      (cons (justifier (reverse line) width)
					    acc)
				      acc)))
			   (loop (cdr tokens)
				 0
				 '()
				 (cons jl ll))))
		       ((>= (+ toklen line-len) width)
			(loop tokens
			      0
			      '()
			      (cons (justifier (reverse line) width) acc)))
		       (else
			(loop (cdr tokens)
			      (+ line-len toklen 1)
			      (cons tok line)
			      acc))))))))))
 
;*---------------------------------------------------------------------*/
;*    make-justifier ...                                               */
;*---------------------------------------------------------------------*/
(define (make-justifier width policy)
   (let ((tokens '()))
      (if (eq? policy 'verbatim)
	  (lambda (cmd . vals)
	     (case cmd
		((output)
		 (set! tokens (append (reverse vals) tokens)))
		((newline)
		 (set! tokens (cons "\n" tokens)))
		((flush)
		 (let ((str (apply string-append (reverse! tokens))))
		    (set! tokens '())
		    (list str)))
		((width)
		 width)))
	  (let ((justifier (case policy
			      ((center)
			       make-centered-line)
			      ((flushleft left)
			       make-flushleft-line)
			      ((flushright right)
			       make-flushright-line)
			      ((justify)
			       make-justified-line)
			      (else
			       make-justified-line)))
		(last ""))
	     (lambda (cmd . vals)
		(case cmd
		   ((newline)
		    (set! tokens (cons 'NEWLINE
				       (append (kotrts last *spaces*) tokens)))
		    (set! last ""))
		   ((output)
		    (if (pair? vals)
			(let* ((val0 (string-append last (car vals)))
			       (vals (cons val0 (cdr vals))))
			   (let loop ((vals vals)
				      (toks tokens))
			      (cond
				 ((null? vals)
				  (set! last "")
				  (set! tokens toks))
				 ((and (null? (cdr vals))
				       (string? (car vals)))
				  (set! last (car vals))
				  (set! tokens toks))
				 (else
				  (loop (cdr vals)
					(append (kotrts (car vals) *spaces*)
						toks))))))))
		   ((flush)
		    (let ((ntokens (append (kotrts last *spaces*) tokens)))
		       (set! last "")
		       (if (pair? ntokens)
			   (let ((toks (reverse! ntokens)))
			      (set! tokens '())
			      (tokens-justify justifier toks width))
			   '())))
		   ((width)
		    width)
		   (else
		    (error "justifier" "Illegal command" cmd))))))))

(define (my-string-append . s)
   (newline (current-error-port))
   (fprint (current-error-port) "s: " s)
   (apply string-append s))

