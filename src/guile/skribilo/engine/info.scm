;;; info.scm  --  GNU Info engine.
;;; -*- coding: iso-8859-1 -*-
;;;
;;; Copyright 2008, 2009, 2012  Ludovic Courtès <ludo@gnu.org>
;;; Copyright 2001, 2002  Manuel Serrano
;;;
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

(define-module (skribilo engine info)
  :use-module (skribilo lib)
  :use-module (skribilo ast)
  :use-module (skribilo engine)
  :use-module (skribilo writer)
  :use-module (skribilo utils syntax)
  :use-module (skribilo package base)
  :autoload   (skribilo parameters)    (*destination-file*)
  :autoload   (skribilo output)        (output)
  :autoload   (skribilo utils justify) (output-justified make-justifier
                                        with-justification)
  :autoload   (skribilo utils text-table) (table->ascii)
  :autoload   (srfi srfi-1)            (fold)
  :use-module (srfi srfi-8)
  :use-module (srfi srfi-11)
  :use-module (srfi srfi-13)
  :use-module (srfi srfi-26)
  :use-module (ice-9 match)
  :use-module (ice-9 format)

  :export (info-engine))

(skribilo-module-syntax)


(define info-engine
  (make-engine 'info
     :version 1.0
     :format "info"
     :delegate (find-engine 'base)
     :filter (lambda (str)
               ;; Justify all the strings that are to be output.
               (with-output-to-string
                 (lambda ()
                   (output-justified str))))
     :custom '()))

;*---------------------------------------------------------------------*/
;*    info-dest ...                                                    */
;*---------------------------------------------------------------------*/
(define (info-dest)
   (if (string? (*destination-file*))
       (*destination-file*)
       "anonymous.info"))

;;
;; Convenience functions.
;;

(define (print . args)
  (for-each display args)
  (newline))

(define (%block? obj)
  (and (markup? obj)
       (memq (markup-markup obj)
             '(chapter section subsection subsubsection))))

;*---------------------------------------------------------------------*/
;*    info-node ...                                                    */
;*---------------------------------------------------------------------*/
(define (info-node node next prev up)
   (print "\n")
   (format #t "File: ~a,  Node: ~a,  ~:[~*~;Next: ~a,  ~]~:[~*~;Prev: ~a,  ~]Up: ~a~%"
           (info-dest) node next next prev prev up))

;*---------------------------------------------------------------------*/
;*    node-next+prev+up ...                                           */
;*---------------------------------------------------------------------*/
(define (node-next+prev+up section e)
  ;; Return the next, previous, and up node of SECTION.

  (define (ast-prev+next n)
    ;; Return the nodes around N at its level.
    (define p
      (ast-parent n))

    (let loop ((nodes (filter %block? (node-children p)))
               (prev  (and (eq? p (ast-document n)) p)))
      (match nodes
        (((? (cut eq? n <>)))
         (values prev #f))
        (((? (cut eq? n <>)) next _ ...)
         (values prev next))
        ((prev rest ...)
         (loop rest prev)))))

  (define (title n)
    (if (document? n)
        "Top"
        (block-title n e)))

  (if (document? section)
      (let loop ((c (markup-body section)))
        (cond
         ((null? c)
          (values "Top" "(dir)" "(dir)"))
         ((or (is-markup? (car c) 'chapter)
              (is-markup? (car c) 'section))
          (values (block-title (car c) e) "(dir)" "(dir)"))
         (else
          (loop (cdr c)))))
      (let-values (((parent)    (ast-parent section))
                   ((prev next) (ast-prev+next section)))
        (values (and=> next title)
                (and=> prev title)
                (title parent)))))

;*---------------------------------------------------------------------*/
;*    node-menu ...                                                    */
;*---------------------------------------------------------------------*/
(define (node-menu container e)
  (let ((children (filter %block? (node-children container))))
    (if (pair? children)
        (begin
          (newline)
          (print "* Menu:")
          (newline)
          (for-each (lambda (c)
                      (print "* " (block-title c e) "::"))
                    children)))
    (newline)))

;*---------------------------------------------------------------------*/
;*    block-title ::%chapter ...                                       */
;*---------------------------------------------------------------------*/
(define (block-title obj e)
  (or (let ((title (markup-option obj :info-node)))
        (and title (ast->string title)))
      (let ((title    (markup-option obj :title))
            (subtitle (markup-option obj :subtitle)))
        (let ((title (if title title subtitle)))
          (if (string? title)
              title
              (ast->string title))))))

;*---------------------------------------------------------------------*/
;*    check-node-title-conflicts ...                                   */
;*---------------------------------------------------------------------*/
(define (check-node-title-conflicts doc e)
  ;; Check whether Info node titles are unique and issue a warning if they're
  ;; not.  Since we compute node titles based on the `:title' option of
  ;; sections, we can't guarantee uniqueness so the best we can do is report
  ;; about it.
  (let ((sections (search-down %block? doc)))
    (fold (lambda (section section+title)
            (let* ((title      (block-title section e))
                   (same-named (assoc title section+title string=?)))
              (if (pair? same-named)
                  (begin
                    (skribe-warning/ast 1 section
                                        (format #f
                                                (_ "Info node title `~A' already used")
                                                title))
                    (skribe-warning/ast 1 (cdr same-named)
                                        (_ "previous occurrence was here"))
                    section+title)
                  (alist-cons title section section+title))))
          '()
          sections)
    #t))

;*---------------------------------------------------------------------*/
;*    info ::%document ...                                             */
;*---------------------------------------------------------------------*/
(markup-writer 'document info-engine
  :options '(:title :author :ending)
  :action (lambda (doc e)
            (check-node-title-conflicts doc e)

            (cond-expand
             (guile-2 (set-port-encoding! (current-output-port) "UTF-8"))
             (else #t))

            (let ((title     (markup-option doc :title))
                  (authors   (markup-option doc :author))
                  (body      (markup-body doc))
                  (footnotes (reverse!
                              (container-env-get doc 'footnote-env))))
              (scribe-document->info doc (if title title "")
                                     (if (list? authors)
                                         authors
                                         (list authors))
                                     body e)
              (if (pair? footnotes)
                  (begin
                    (with-justification
                     (make-justifier *text-column-width* 'left)
                     (lambda ()
                       (newline)
                       (newline)
                       (print "-------------")
                       (for-each (lambda (fn)
                                   (let ((label (markup-option fn :label))
                                         (note  (markup-body fn)))
                                     (output (list "*" label ": ") e)
                                     (output note e)
                                     (output-newline)))
                                 footnotes)
                       ))))
              ;; FIXME: Handle `:ending'.
              )))

;*---------------------------------------------------------------------*/
;*     scribe-document->info ...                                       */
;*---------------------------------------------------------------------*/
(define (scribe-document->info obj title authors body e)
   (define (info-authors1 author)
      (output author e)
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
		      (+ cnum 1))))))
      (output (apply table
		    (if first
			(cons (make-row (list (car authors)) :colspan cols)
			      (make-rows (cdr authors)))
			(make-rows authors)))
              e))
   (define (info-authors authors)
      (if (pair? authors)
	  (begin
	     (output-newline)
	     (output-justified "--o-0-o--")
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
      (let ((title (ast->string title)))
        (display title)
        (newline)
        (display (make-string (string-length title) #\*))
        (newline))

      (with-justification
       (make-justifier (justification-width) 'center)
       (lambda ()
	  (output-newline)
	  (info-authors authors)
	  (output-newline)
	  (output-flush *margin*))))

   ;; the main node
   (receive (next prev top)
      (node-next+prev+up obj e)
      (newline)
      (info-node "Top" next prev top))
   ;; the title
   (info-title title authors)
   (output-flush 0)
   ;; the main info menu
   (node-menu obj e)
   ;; the body
   (output body e)
   (output-flush 0)
   ;; the footer of the document
   ;(info-footer)
   (output-flush 0)
   ;; we are done
   (newline)
   (newline))

;*---------------------------------------------------------------------*/
;*    info ::%author ...                                               */
;*---------------------------------------------------------------------*/
(markup-writer 'author info-engine
  :options '(:name :title :affiliation :email :url :address :phone
             :photo :align)  ;; XXX: These two aren't actually supported.

  :action (lambda (n e)
            (let ((name        (markup-option n :name))
                  (title       (markup-option n :title))
                  (affiliation (markup-option n :affiliation))
                  (email       (markup-option n :email))
                  (url         (markup-option n :url))
                  (address     (markup-option n :address))
                  (phone       (markup-option n :phone)))
              (if (or (pair? name) (string? name))
                  (output name e))
              (if title (begin (output-newline) (output title e)))
              (if affiliation (begin (output-newline) (output affiliation e)))
              (if (pair? address)
                  (for-each (lambda (x) (output-newline) (output x e)) address))
              (if email (begin (output-newline) (output email e)))
              (if url (begin (output-newline) (output url e)))
              (if phone (begin (output-newline) (output phone e)))
              (output-newline))))
   
;*---------------------------------------------------------------------*/
;*    scribe->html ::%toc ...                                          */
;*---------------------------------------------------------------------*/
(markup-writer 'toc info-engine
  :action (lambda (n e)
            (node-menu (ast-document n) e)))

;*---------------------------------------------------------------------*/
;*    info ::%linebreak ...                                            */
;*---------------------------------------------------------------------*/
(markup-writer 'linebreak info-engine
  :action (lambda (n e)
            (output-newline)))

;*---------------------------------------------------------------------*/
;*    info ::%center ...                                               */
;*---------------------------------------------------------------------*/
(markup-writer 'center info-engine
  :action (lambda (n e)
            (with-justification (make-justifier (justification-width) 'center)
                                (lambda ()
                                  (output (markup-body n) e)))))

;*---------------------------------------------------------------------*/
;*    info ::%flush ...                                                */
;*---------------------------------------------------------------------*/
(markup-writer 'flush info-engine
  :options '(:side)
  :action (lambda (n e)
            (let ((side (markup-option n :side)))
              (with-justification (make-justifier (justification-width) side)
                                  (lambda ()
                                    (output (markup-body n) e))))))

;*---------------------------------------------------------------------*/
;*    ~ ...                                                           */
;*---------------------------------------------------------------------*/
(markup-writer '~
  ;; FIXME: This isn't actually breakable.
  :action (lambda (n e) (output-justified " ")))

;*---------------------------------------------------------------------*/
;*    breakable-space ...                                              */
;*---------------------------------------------------------------------*/
(markup-writer 'breakable-space
  :action (lambda (n e) (output-justified " ")))

;*---------------------------------------------------------------------*/
;*    *ornaments* ...                                                  */
;*---------------------------------------------------------------------*/
(define %ornaments
   `((bold      "*" "*")
     (emph      "_" "_")
     (underline "*" "*")
     (it        "_" "_")
     (samp      "_" "_")
     (sc        "" "")
     (sup       "^" "")
     (sub       "_" "")
     (code      "`" "'")
     (tt        "`" "'")
     (samp      "`" "'")))

;*---------------------------------------------------------------------*/
;*    info ::%ornament ...                                             */
;*---------------------------------------------------------------------*/
(for-each (lambda (ornament)
            (let ((name   (car ornament))
                  (before (cadr ornament))
                  (after  (caddr ornament)))
              (markup-writer name info-engine
                             :before (lambda (n e)
                                       (output-justified before))
                             :after  (lambda (n e)
                                       (output-justified after)))))
          %ornaments)

;*---------------------------------------------------------------------*/
;*    info ::%pre ...                                                  */
;*---------------------------------------------------------------------*/
(markup-writer 'pre info-engine
  :action (lambda (n e)
            (with-justification (make-justifier *text-column-width* 'verbatim)
                                (lambda ()
                                  (output (markup-body n) e)
                                  (output-newline)))))

;*---------------------------------------------------------------------*/
;*    info ::%mark ...                                                 */
;*---------------------------------------------------------------------*/
(markup-writer 'mark info-engine
  :action #f)

;*---------------------------------------------------------------------*/
;*    info ::%reference ...                                            */
;*---------------------------------------------------------------------*/
(markup-writer 'ref info-engine
  :options '(:text :page text kind
             :chapter :section :subsection :subsubsection
             :figure :mark :handle :ident)

  :action (lambda (n e)
            (let ((target (handle-ast (markup-body n))))
              (case (markup-markup target)
                ((chapter)
                 (info-chapter-ref target e))
                ((section)
                 (info-section-ref target e))
                ((subsection)
                 (info-subsection-ref target e))
                ((subsubsection)
                 (info-subsubsection-ref target e))
                ((mark)
                 ;; We can't refer directly to marks, so refer to the
                 ;; enclosing section as an approximation.
                 (let ((parent (find1-up %block? target)))
                   (info-chapter-ref parent e)))
                (else
                 (skribe-warning/ast 1 target
                                     "ref: don't know how to refer to target")
                 (output-justified "section:???"))))))

;*---------------------------------------------------------------------*/
;*    info ::%url-ref ...                                              */
;*---------------------------------------------------------------------*/
(markup-writer 'url-ref info-engine
  :options '(:url :text)
  :action (lambda (n e)
            (let ((url  (markup-option n :url))
                  (text (markup-option n :text)))
              (and text
                   (begin
                     (output text e)
                     (output-justified " (")))
              (output-justified url)
              (and text (output-justified ")")))))

;*---------------------------------------------------------------------*/
;*    info-chapter-ref ...                                             */
;*---------------------------------------------------------------------*/
(define (info-chapter-ref obj e)
   (output-justified "*Note ")
   (output (block-title obj e) e)
   (output-justified ":: "))

;*---------------------------------------------------------------------*/
;*    info-section-ref ...                                             */
;*---------------------------------------------------------------------*/
(define (info-section-ref obj e)
   (let ((title (markup-option obj :title)))
      (output-justified "*Note ")
      (output title e)
      (output-justified ":: ")))

;*---------------------------------------------------------------------*/
;*    info-subsection-ref ...                                          */
;*---------------------------------------------------------------------*/
(define (info-subsection-ref obj e)
   (let ((title (markup-option obj :title)))
      (output-justified "*Note ")
      (output title e)
      (output-justified ":: ")))

;*---------------------------------------------------------------------*/
;*    info-subsubsection-ref ...                                       */
;*---------------------------------------------------------------------*/
(define (info-subsubsection-ref obj e)
   (let ((title (markup-option obj :title)))
      (output-justified "*Note ")
      (output title e)
      (output-justified ":: ")))

;*---------------------------------------------------------------------*/
;*    info ::%biblio-ref ...                                           */
;*---------------------------------------------------------------------*/
(markup-writer 'bib-ref info-engine
  :options '(:text :bib)
  :action (lambda (n e)
            ;; XXX: Produce hyperlink to `the-bibliography'?
            (let ((text (markup-option n :text))
                  (bib  (markup-option n :bib)))
              (if text (output text e))
              (output-justified " [")
              (output bib e)
              (output-justified "]"))))

;*---------------------------------------------------------------------*/
;*    mailto ...                                                       */
;*---------------------------------------------------------------------*/
(markup-writer 'mailto info-engine
  :options '(:text)
  :action (lambda (n e)
            (let ((email (markup-body n))
                  (text  (markup-option n :text)))
              (if text (output text e))
              (output email e))))

;*---------------------------------------------------------------------*/
;*    info ::%item ...                                                 */
;*---------------------------------------------------------------------*/
(markup-writer 'item info-engine
 :options '(:key)
 :action (lambda (n e)
           (let ((k (markup-option n :key)))
             (if k
                 (begin
                   (output k e)
                   (output-newline)
                   ;; FIXME: We should indent the body.
                   ))
             (output (markup-body n) e))))

;*---------------------------------------------------------------------*/
;*    info ::%list ...                                                 */
;*---------------------------------------------------------------------*/
(markup-writer 'itemize info-engine
  :options '(:symbol)
  :action (lambda (n e)
            (output-newline)
            (for-each (lambda (item)
                        (with-justification (make-justifier
                                             (- (justification-width) 3)
                                             'left)
                                            (lambda ()
					      (output-justified "- ")
					      (output item e))
                                            3))
                      (markup-body n))
            (output-newline)))

(markup-writer 'enumerate info-engine
  :options '(:symbol)
  :action (lambda (n e)
            (output-newline)
            (let loop ((num   1)
                       (items (markup-body n)))
              (if (pair? items)
                  (let ((item (car items)))
		    (with-justification (make-justifier
					 (- (justification-width) 3)
					 'left)
					(lambda ()
                                          (output-justified (number->string num))
                                          (output-justified " - ")
                                          (output item e))
					3)
		    (loop (+ num 1) (cdr items)))))
            (output-newline)))

(markup-writer 'description info-engine
  :options '(:symbol)
  :action (lambda (n e)
            (output-newline)
            (for-each (lambda (item)
                        (with-justification
                         (make-justifier
                          (- (justification-width) 3)
                          'left)
                         (lambda ()
                           (output item e))
                         3))
                      (markup-body n))
            (output-newline)))

;*---------------------------------------------------------------------*/
;*    info ::%section ...                                              */
;*---------------------------------------------------------------------*/
(markup-writer 'section info-engine
  :options '(:title :info-node :number :toc :file :env)
  :action (lambda (n e)
            (let ((body (markup-body n)))
              (output-newline)
              (output-flush *margin*)
              (let ((t (block-title n e)))
                (receive (next prev top)
                    (node-next+prev+up n e)
                  (info-node t next prev top)
                  (print t)
                  (print (make-string (string-length t) #\=))))
              (node-menu n e)
              (with-justification (make-justifier *text-column-width*
                                                  *text-justification*)
                                  (lambda () (output body e))))))

;*---------------------------------------------------------------------*/
;*    info ::%subsection ...                                           */
;*---------------------------------------------------------------------*/
(markup-writer 'subsection info-engine
  :options '(:title :info-node :number :toc :env :file)
  :action (lambda (n e)
            (let ((body (markup-body n)))
              (output-flush *margin*)
              (let ((t (block-title n e)))
                (receive (next prev top)
                    (node-next+prev+up n e)
                  (info-node t next prev top)
                  (print t)
                  (print (make-string (string-length t) #\-))))
              (output body e))))

;*---------------------------------------------------------------------*/
;*    info ::%subsubsection ...                                        */
;*---------------------------------------------------------------------*/
(markup-writer 'subsubsection info-engine
  :options '(:title :info-node :number :toc :env :file)
  :action (lambda (n e)
            (let ((body (markup-body n)))
              (output-flush *margin*)
              (let ((t (block-title n e)))
                (receive (next prev top)
                    (node-next+prev+up n e)
                  (info-node t next prev top)
                  (print t)
                  (print (make-string (string-length t) #\~))))
              (output body e))))

;*---------------------------------------------------------------------*/
;*    info ::%paragraph ...                                            */
;*---------------------------------------------------------------------*/
(markup-writer 'paragraph info-engine
  :action (lambda (n e)
            (output-newline)
            (output-flush *margin*)
            (or (first-paragraph? n)
                (display "   "))
            (output (markup-body n) e)))

;*---------------------------------------------------------------------*/
;*    info ::%chapter ...                                              */
;*---------------------------------------------------------------------*/
(markup-writer 'chapter info-engine
  :options '(:title :number :file :toc :info-node :env)
  :action (lambda (n e)
            (let ((body (markup-body n)))
              (output-newline)
              (output-flush *margin*)
              (let ((t (block-title n e)))
                (receive (next prev top)
                    (node-next+prev+up n e)
                  (info-node t next prev top)
                  (print t)
                  (print (make-string (string-length t) #\*))))
              (node-menu n e)
              (output body e))))

;*---------------------------------------------------------------------*/
;*    info ::%hrule ...                                                */
;*---------------------------------------------------------------------*/
(markup-writer 'hrule info-engine
  :options '(:width)
  :action (lambda (n e)
            (let ((width  (markup-option n :width)))
              (let ((w (if (= width 100)
                           (justification-width)
                           (inexact->exact
                            (* (exact->inexact (justification-width))
                               (/ (exact->inexact width) 100.))))))
                (output-justified (make-string w #\-))))))

;*---------------------------------------------------------------------*/
;*    info ::%table ...                                                */
;*---------------------------------------------------------------------*/
(markup-writer 'table info-engine
  :options '(:border :width
             ;; FIXME: We don't actually support the following.
             :frame :rules :cellpadding :rulecolor)
  :action (lambda (n e)
            (let ((border (markup-option n :border)))
              (output-flush *margin*)
              (if border
                  (border-table->info n)
                  (table->ascii n (lambda (obj)
                                    (output obj e))))
              (output-flush *margin*))))

;*---------------------------------------------------------------------*/
;*    info ::&the-bibliography ...                                     */
;*---------------------------------------------------------------------*/
(markup-writer '&the-bibliography info-engine
  :action (lambda (n e)
            (output-justified "[FIXME: Bibliography not implemented yet.]")))

;*---------------------------------------------------------------------*/
;*    border-table->info ...                                           */
;*---------------------------------------------------------------------*/
(define (border-table->info table)
   (table->ascii table (lambda (obj)
                         (output obj info-engine))))

;*---------------------------------------------------------------------*/
;*    info ::%figure ...                                               */
;*---------------------------------------------------------------------*/
(markup-writer 'figure info-engine
  :options '(:legend :number :multicolumns)
  :action (lambda (n e)
            (let ((body   (markup-body n))
                  (legend (markup-option n :legend))
                  (number (markup-option n :number)))
              (output-newline)
              (output body e)
              (output-newline)
              (output-newline)
              (output-justified "Fig. ")
              (and (number? number)
                   (output-justified (number->string number)))
              (output-justified ": ")
              (output legend e)
              (output-newline))))

;*---------------------------------------------------------------------*/
;*    info ::%footnote ...                                             */
;*---------------------------------------------------------------------*/
(markup-writer 'footnote info-engine
  :options '(:label)
  :action (lambda (n e)
            (let ((label (markup-option n :label)))
              (output (markup-body n) e)
              (output "(*" e)
              (output label e)
              (output ")" e))))


;;; info.scm ends here
