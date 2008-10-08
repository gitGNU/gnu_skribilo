;;; info.scm  --  GNU Info engine.
;;;
;;; Copyright 2008  Ludovic Courtès <ludo@gnu.org>
;;; Copyright 2001, 2002  Manuel Serrano
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

(define-module (skribilo engine info)
  :use-module (skribilo lib)
  :use-module (skribilo ast)
  :use-module (skribilo engine)
  :use-module (skribilo writer)
  :use-module (skribilo location)
  :use-module (skribilo utils strings)
  :use-module (skribilo utils syntax)
  :use-module (skribilo package base)
  :autoload   (skribilo parameters)    (*destination-file*)
  :autoload   (skribilo evaluator)     (evaluate-document)
  :autoload   (skribilo output)        (output)
  :autoload   (skribilo debug)         (*debug*)
  :autoload   (skribilo utils justify) (make-justifier)
  :use-module (srfi srfi-8)
  :use-module (srfi srfi-13)

  :export (info-engine))

(fluid-set! current-reader %skribilo-module-reader)



(define info-engine
  (make-engine 'info
     :version 1.0
     :format "info"
     :delegate (find-engine 'base)
     :filter #f ;; XXX: Do we need something?
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
  (for-each display args))

(define (%block? obj)
  (and (markup? obj)
       (memq (markup-markup obj)
             '(chapter section subsection subsubsection))))

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
;*    node-next+prev+top ::%document ...                               */
;*---------------------------------------------------------------------*/
(markup-writer 'document info-engine
  :action (lambda (doc e)
            (let loop ((c (ast-body doc)))
              (cond
               ((null? c)
                (values "Top" "(dir)" "(dir)"))
               ((or (is-markup? (car c) 'chapter)
                    (is-markup? (car c) 'section))
                (values (block-title (car c)) "(dir)" "(dir)"))
               (else
                (loop (cdr c)))))))

;*---------------------------------------------------------------------*/
;*    node-next+prev+top ...                                           */
;*---------------------------------------------------------------------*/
(define (node-next+prev+top section)
  (let ((parent (ast-parent section)))
      (let ((top (if (document? parent)
		     "Top"
		     (block-title parent))))
	 (let loop ((els (ast-body parent))
		    (prev #f))
	    (cond
	       ((null? els)
		(values top top top))
	       ((eq? (car els) section)
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
(define (node-menu container e)
  (let ((children (ast-body container)))
      (if (pair? (filter (lambda (x) (or (%chapter? x) (%section? x)))
			 children))
	  (begin
	     (newline)
	     (print "* Menu:")
	     (newline)
	     (for-each (lambda (c)
			  (if (%block? c)
			      (print "* " (block-title c e) "::")))
		       (reverse children))))
      (newline)))

;*---------------------------------------------------------------------*/
;*    block-title ::%chapter ...                                       */
;*---------------------------------------------------------------------*/
(define (block-title obj e)
  (let ((title    (markup-option obj :title))
        (subtitle (markup-option obj :subtitle)))
      (let ((title (if title title subtitle)))
	 (if (string? title)
	     title
	     (with-output-to-string 
		(lambda () (output title e)))))))

;*---------------------------------------------------------------------*/
;*    info ::%document ...                                             */
;*---------------------------------------------------------------------*/
(markup-writer 'document info-engine
  :action (lambda (doc e)
            (let ((title  (markup-option doc :title))
                  (author (markup-option doc :author))
                  (body   (markup-body doc)))
              (scribe-document->info doc (if title title "")
                                     (if (list? authors)
                                         authors
                                         (list authors))
                                     body)
              (if (pair? footnotes)
                  (begin
                    (with-justification
                     (make-justifier *text-column-width* 'left)
                     (lambda ()
                       (newline)
                       (newline)
                       (print "-------------")
                       ;; FIXME: Handle footnotes.
;;                        (for-each (lambda (fn)
;;                                    (with-access::%footnote fn (number note id)
;;                                                            (output (string-append
;;                                                                     "*"
;;                                                                     (number->string number)
;;                                                                     ": "))
;;                                                            (info note)
;;                                                            (output-newline)))
;;                                  footnotes)
                       )))))))

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
   (receive (next prev top)
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
(markup-writer 'author info-engine
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
                                  (output (%center-body obj) e)))))

;*---------------------------------------------------------------------*/
;*    info ::%flush ...                                                */
;*---------------------------------------------------------------------*/
(markup-writer 'flush info-engine
  :options '(:side)
  :action (lambda (n e)
            (let ((side (markup-option :side)))
              (with-justification (make-justifier (justification-width) side)
                                  (lambda ()
                                    (output (%flush-body obj) e))))))

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
(for-each (lambda (ornament)
            (let ((name   (car ornament))
                  (before (cadr ornament))
                  (after  (caddr ornament)))
              (markup-writer name info-engine
                             :before before
                             :after after)))
          *ornaments*)

;*---------------------------------------------------------------------*/
;*    info ::%pre ...                                                  */
;*---------------------------------------------------------------------*/
(markup-writer 'pre info-engine
  :action (lambda (n e)
            (with-justification (make-justifier *text-column-width* 'verbatim)
                                (lambda ()
                                  (output (ast-body obj) e)
                                  (output-newline)))))

;*---------------------------------------------------------------------*/
;*    info ::%mark ...                                                 */
;*---------------------------------------------------------------------*/
(markup-writer 'mark info-engine
  :action #f)

;*---------------------------------------------------------------------*/
;*    info ::%reference ...                                            */
;*---------------------------------------------------------------------*/
;; FIXME: Implement `ref' using `info-chapter-ref', etc.
;; (markup-writer 'ref info-engine
;;   :action (lambda (n e)
;;             #f))

;*---------------------------------------------------------------------*/
;*    info ::%url-ref ...                                              */
;*---------------------------------------------------------------------*/
(markup-writer 'url-ref info-engine
  :options '(:url :text)
  :action (lambda (n e)
            (let ((url  (markup-option :url))
                  (text (markup-option :text)))
              (if text
                  (begin
                    (output "*Note ")
                    (output text e)
                    (output " (")))
              (output url e)
              (if text (output ")"))
              (output ":: "))))

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
(define (info obj::%section-ref)
   (receive (_ section)
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
(define (info obj::%subsection-ref)
   (receive (_ subsection)
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
(define (info obj::%subsubsection-ref)
   (receive (_ subsubsection)
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
(markup-writer 'bib-ref info-engine
  :options '(:text :bib)
  :action (lambda (n e)
            ;; XXX: Produce hyperlink to `the-bibliography'?
            (let ((text (markup-option n :text))
                  (bib  (markup-option n :bib)))
              (if text (output text e))
              (output " [")
              (output bib e)
              (output "]"))))

;*---------------------------------------------------------------------*/
;*    mailto ...                                                       */
;*---------------------------------------------------------------------*/
(markup-writer 'mailto info-engine
  :options (:text)
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
                   (display ": ")))
             (output (markup-body n) e))))

;*---------------------------------------------------------------------*/
;*    info ::%list ...                                                 */
;*---------------------------------------------------------------------*/
(markup-writer 'itemize info-engine
  :action (lambda (n e)
            (for-each (lambda (item)
                        (with-justification (make-justifier
                                             (-fx (justification-width) 3)
                                             'left)
                                            (lambda ()
					      (output "- ")
					      (output item e))
                                            3))
                      items)))

(markup-writer 'enumerate info-engine
  :action (lambda (n e)
            (let loop ((num   1)
                       (items (markup-body n)))
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
		    (loop (+ num 1) (cdr items)))))))

(markup-writer 'description info-engine
  :action (lambda (n e)
            (for-each (lambda (item)
                        (with-justification
                         (make-justifier
                          (-fx (justification-width) 3)
                          'left)
                         (output item e)
                         3))
                      items)))

;*---------------------------------------------------------------------*/
;*    info ::%section ...                                              */
;*---------------------------------------------------------------------*/
(markup-writer 'section info-engine
  :options '(:title :html-title :number :toc :file :env)
  :action (lambda (n e)
            (let ((body  (markup-body n))
                  (title (markup-option n :title)))
              (output-newline)
              (output-flush *margin*)
              (let ((t (block-title n)))
                (receive (next prev top)
                    (node-next+prev+top n)
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
  :options '(:title :html-title :number :toc :env :file)
  :action (lambda (n e)
            (let ((body  (markup-body n))
                  (title (markup-option n :title)))
              (output-flush *margin*)
              (let ((t (block-title n)))
                (receive (next prev top)
                    (node-next+prev+top n)
                  (info-node t next prev top)
                  (print t)
                  (print (make-string (string-length t) #\-))))
              (output body e))))

;*---------------------------------------------------------------------*/
;*    info ::%subsubsection ...                                        */
;*---------------------------------------------------------------------*/
(markup-writer 'subsubsection info-engine
  :options '(:title :html-title :number :toc :env :file)
  :action (lambda (n e)
            (let ((body  (markup-body n))
                  (title (markup-option n :title)))
              (output-flush *margin*)
              (let ((t (block-title n)))
                (receive (next prev top)
                    (node-next+prev+top n)
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
            (output (markup-body n) e)))

;*---------------------------------------------------------------------*/
;*    info ::%chapter ...                                              */
;*---------------------------------------------------------------------*/
(markup-writer 'chapter info-engine
  :options '(:title :number :file :toc :html-title :env)
  :action (lambda (n e)
            (let ((body   (markup-body n))
                  (file   (markup-option n :file))
                  (title  (markup-option n :title)))
              (output-newline)
              (output-flush *margin*)
              (let ((t (block-title n)))
                (receive (next prev top)
                    (node-next+prev+top n)
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
                (output (make-string w #\-))))))

;*---------------------------------------------------------------------*/
;*    info ::%table ...                                                */
;*---------------------------------------------------------------------*/
(markup-writer 'table info-engine
  :options (:border)
  :action (lambda (n e)
            (let ((border (markup-option n :border)))
              (output-flush *margin*)
              (if border
                  (border-table->info n)
                  (table->ascii n info))
              (output-flush *margin*))))

;*---------------------------------------------------------------------*/
;*    border-table->info ...                                           */
;*---------------------------------------------------------------------*/
(define (border-table->info table)
   (table->ascii table info))

;*---------------------------------------------------------------------*/
;*    info ::%figure ...                                               */
;*---------------------------------------------------------------------*/
(markup-writer 'figure info-engine
  :options '(:legend :number)
  :action (lambda (n e)
            (let ((body   (markup-body n))
                  (legend (markup-option n :legend))
                  (number (markup-option n :number)))
              (output-newline)
              (output body e)
              (output-newline)
              (output-newline)
              (output "Fig. ")
              (output (number->string number))
              (output ": ")
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

