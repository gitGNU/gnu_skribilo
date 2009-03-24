;;; biblio.scm  --  Bibliography functions.
;;;
;;; Copyright 2005, 2006, 2007, 2009  Ludovic Courtès <ludo@gnu.org>
;;; Copyright 2001, 2002, 2003, 2004  Manuel Serrano
;;; Copyright 2003, 2004  Erick Gallesio - I3S-CNRS/ESSI <eg@essi.fr>
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
;;; USA.main.st



(define-module (skribilo biblio)
  :use-module (skribilo utils syntax) ;; `when', `unless'

  :use-module (srfi srfi-1)
  :autoload   (srfi srfi-34)         (raise)
  :use-module (srfi srfi-35)
  :use-module (srfi srfi-39)
  :use-module (skribilo condition)

  :autoload   (skribilo reader)       (*document-reader*)
  :autoload   (skribilo parameters)   (*bib-path*)
  :autoload   (skribilo ast)          (<markup> <handle> is-markup?)
  :autoload   (skribilo lib)          (skribe-warning)
  :autoload   (skribilo biblio author)(short-author-names)

  :use-module (ice-9 optargs)
  :use-module (oop goops)

  :export (bib-table? make-bib-table *bib-table*
	   bib-add! bib-duplicate bib-for-each bib-map
	   open-bib-file parse-bib

           bib-load! resolve-bib resolve-the-bib make-bib-entry

           ;; entry labels
           assign-entries-numbers! assign-entries-name+years!

           ;; sorting the bibliography
           bib-sort/authors bib-sort/idents bib-sort/dates

           ;; sorting consecutive entries in a `ref'
           bib-sort-refs/number

           ;; error conditions
           &biblio-error &biblio-entry-error &biblio-template-error
           &biblio-parse-error
           biblio-error? biblio-entry-error? biblio-template-error?
           biblio-parse-error?
           biblio-entry-error:entry
           biblio-template-error:expression
           biblio-template-error:template
           biblio-parse-error:sexp))

;;; Author: Erick Gallesio, Manuel Serrano, Ludovic Courtès
;;;
;;; Commentary:
;;;
;;; Provides the bibliography data type and basic bibliography handling,
;;; including simple procedures to sort bibliography entries.
;;;
;;; Code:

(skribilo-module-syntax)



;;;
;;; Error conditions.
;;;

(define-condition-type &biblio-error &skribilo-error
  biblio-error?)

(define-condition-type &biblio-entry-error &biblio-error
  biblio-entry-error?
  (entry biblio-entry-error:entry))

(define-condition-type &biblio-template-error &biblio-error
  biblio-template-error?
  (expression  biblio-template-error:expression)
  (template    biblio-template-error:template))

(define-condition-type &biblio-parse-error &biblio-error
  biblio-parse-error?
  (sexp biblio-parse-error:sexp))


(define (handle-biblio-error c)
  ;; Issue a user-friendly error message for error condition C.
  (cond ((biblio-entry-error? c)
         (let* ((entry (biblio-entry-error:entry c))
                (file  (source-property entry 'filename))
                (col   (source-property entry 'column))
                (line  (source-property entry 'line)))
           (if (and file col line)
               (format (current-error-port)
                       (_ "~a:~a:~a: invalid bibliography entry: ~a~%")
                       file line col)
               (format (current-error-port)
                       (_ "invalid bibliography entry: ~a~%")
                       entry))))
	((biblio-template-error? c)
	 (format (current-error-port)
                 (_ "invalid bibliography entry template: `~a', in `~a'~%")
                 (biblio-template-error:expression c)
                 (biblio-template-error:template c)))
        ((biblio-parse-error? c)
         (format (current-error-port)
                 (_ "invalid bibliography entry s-exp: `~a'~%")
                 (biblio-parse-error:sexp c)))
	(else
	 (format (current-error-port)
                 (_ "undefined bibliography error: ~a~%")
		 c))))

(register-error-condition-handler! biblio-error?
				   handle-biblio-error)



;;;
;;; Accessors.
;;;

(define (make-bib-table ident)
   (make-hash-table))

(define (bib-table? obj)
  (hash-table? obj))

;; The current bib table.
(define *bib-table*
  (make-parameter (make-bib-table "default-bib-table")))

(define (bib-add! table . entries)
  (if (not (bib-table? table))
      (raise (condition
              (&invalid-argument-error (proc-name "bib-add!")
                                       (argument table))))
      (for-each (lambda (entry)
		  (cond
		    ((and (list? entry) (> (length entry) 2))
		     (let* ((kind   (car entry))
			    (key    (format #f "~A" (cadr entry)))
			    (fields (cddr entry))
			    (old    (hash-ref table key)))
		       (if old
			   (bib-duplicate key #f old)
			   (hash-set! table key
				      (make-bib-entry kind key fields #f)))))
		    (else
                     (raise (condition
                             (&biblio-entry-error (entry entry)))))))
		entries)))

(define* (bib-for-each proc :optional (table (*bib-table*)))
  (hash-for-each (lambda (ident entry)
		   (proc ident entry))
		 table))

(define* (bib-map proc :optional (table (*bib-table*)))
  (hash-map->list (lambda (ident entry)
		    (proc ident entry))
		  table))

(define (bib-duplicate ident from old)
  (let ((ofrom (markup-option old 'from)))
    (skribe-warning 2
		    'bib
		    (format #f "duplicated bibliographic entry ~a'.\n" ident)
		    (if ofrom
			(format #f " using version of `~a'.\n" ofrom)
			"")
		    (if from
			(format #f " ignoring version of `~a'." from)
			" ignoring redefinition."))))



;;;
;;; Parsing.
;;;

(define (parse-bib table port)
  (let ((read (*document-reader*)))
    (if (not (bib-table? table))
        (raise (condition
                (&invalid-argument-error (proc-name "parse-bib")
                                         (argument table))))
	(let ((from (port-filename port)))
	  (let Loop ((entry (read port)))
	    (unless (eof-object? entry)
	      (cond
	       ((and (list? entry) (> (length entry) 2))
		(let* ((kind   (car entry))
		       (key    (format #f "~A" (cadr entry)))
		       (fields (cddr entry))
		       (old    (hash-ref table key)))
		  (if old
		      (bib-duplicate key from old)
		      (hash-set! table key
				 (make-bib-entry kind key fields from)))
		  (Loop (read port))))
	       (else
                (raise (condition
                        (&biblio-entry-error (entry entry))))))))))))

(define* (open-bib-file file :optional (command #f))
 (let ((path (search-path (*bib-path*) file)))
   (if (string? path)
       (begin
	 (when (> (*verbose*) 0)
	   (format (current-error-port)
                   "  [loading bibliography: ~S]\n" path))
         ;; FIXME: The following `open-input-file' won't work with actual
         ;; commands.  We need to use `(ice-9 popen)'.
	 (open-input-file (if (string? command)
			      (string-append "| "
					     (format #f command path))
			      path)))
       (raise (condition (&file-search-error (file-name file)
					     (path (*bib-path*))))))))


;;;
;;; High-level API.
;;;
;;; The contents of the file below are unchanged compared to Skribe 1.2d's
;;; `bib.scm' file found in the `common' directory.  The copyright notice for
;;; this file was:
;;;
;;;  Copyright 2001, 2002, 2003, 2004  Manuel Serrano
;;;


;*---------------------------------------------------------------------*/
;*    bib-load! ...                                                    */
;*---------------------------------------------------------------------*/
(define (bib-load! table filename command)
   (if (not (bib-table? table))
       (raise (condition
               (&invalid-argument-error (proc-name "bib-load!")
                                        (argument table))))
       ;; read the file
       (let ((p (open-bib-file filename command)))
         (unwind-protect
           (parse-bib table p)
           (close-input-port p)))))

;*---------------------------------------------------------------------*/
;*    resolve-bib ...                                                  */
;*---------------------------------------------------------------------*/
(define (resolve-bib table ident)
   (if (not (bib-table? table))
       (raise (condition
               (&invalid-argument-error (proc-name "resolve-bib")
                                        (argument table))))
       (let* ((i (cond
		    ((string? ident) ident)
		    ((symbol? ident) (symbol->string ident))
		    (else
                     (raise (condition
                             (&invalid-argument-error
                              (proc-name "resolve-bib")
                              (argument ident)))))))
	      (en (hash-ref table i)))
	  (if (is-markup? en '&bib-entry)
	      en
	      #f))))

;*---------------------------------------------------------------------*/
;*    make-bib-entry ...                                               */
;*---------------------------------------------------------------------*/
(define (make-bib-entry kind ident fields from)
   (let* ((m (make <markup>
		:markup '&bib-entry
		:ident ident
		:options `((kind ,kind) (from ,from))))
	  (h (make <handle> :ast m)))
      (for-each (lambda (f)
		   (if (and (pair? f)
			    (pair? (cdr f))
			    (null? (cddr f))
			    (symbol? (car f)))
		       (markup-option-add! m 
					   (car f)
					   (make <markup>
					      :markup (symbol-append
						       '&bib-entry-
						       (car f))
					      :parent h
					      :body (cadr f)))
                       (raise (condition (&biblio-parse-error
                                          (sexp f))))))
		fields)
      m))


;;;
;;; Sorting the bibliography.
;;;

;*---------------------------------------------------------------------*/
;*    bib-sort/authors ...                                             */
;*---------------------------------------------------------------------*/
(define (bib-sort/authors l)
   (define (cmp i1 i2 def)
      (cond
	 ((and (markup? i1) (markup? i2))
	  (cmp (markup-body i1) (markup-body i2) def))
	 ((markup? i1)
	  (cmp (markup-body i1) i2 def))
	 ((markup? i2)
	  (cmp i1 (markup-body i2) def))
	 ((and (string? i1) (string? i2))
	  (if (string=? i1 i2)
	      (def)
	      (string<? i1 i2)))
	 ((string? i1)
	  #f)
	 ((string? i2)
	  #t)
	 (else
	  (def))))
   (sort l (lambda (e1 e2)
	      (cmp (markup-option e1 'author)
		   (markup-option e2 'author)
		   (lambda ()
		      (cmp (markup-option e1 'year)
			   (markup-option e2 'year)
			   (lambda ()
			      (cmp (markup-option e1 'title)
				   (markup-option e2 'title)
				   (lambda ()
				      (cmp (markup-ident e1)
					   (markup-ident e2)
					   (lambda ()
					      #t)))))))))))

;*---------------------------------------------------------------------*/
;*    bib-sort/idents ...                                              */
;*---------------------------------------------------------------------*/
(define (bib-sort/idents l)
   (sort l (lambda (e f) (string<? (markup-ident e) (markup-ident f)))))

;*---------------------------------------------------------------------*/
;*    bib-sort/dates ...                                               */
;*---------------------------------------------------------------------*/
(define (bib-sort/dates l)
   (sort l (lambda (p1 p2)
	      (define (month-num m)
		 (let ((body (markup-body m)))
		    (if (not (string? body))
			13
			(let* ((sy (string->symbol (string-downcase body)))
			       (c (assq sy '((jan . 1)
					     (feb . 2)
					     (mar . 3)
					     (apr . 4)
					     (may . 5)
					     (jun . 6)
					     (jul . 7)
					     (aug . 8)
					     (sep . 9)
					     (oct . 10)
					     (nov . 11)
					     (dec . 12)))))
			   (if (pair? c) (cdr c) 13)))))
	      (let ((d1 (markup-option p1 'year))
		    (d2 (markup-option p2 'year)))
		 (cond
		    ((not (markup? d1)) #f)
		    ((not (markup? d2)) #t)
		    (else
		     (let ((y1 (markup-body d1))
			   (y2 (markup-body d2)))
			(cond
			   ((string>? y1 y2) #t)
			   ((string<? y1 y2) #f)
			   (else
			    (let ((d1 (markup-option p1 'month))
				  (d2 (markup-option p2 'month)))
			       (cond
				  ((not (markup? d1)) #f)
				  ((not (markup? d2)) #t)
				  (else
				   (let ((m1 (month-num d1))
					 (m2 (month-num d2)))
				      (> m1 m2))))))))))))))


;;;
;;; Sorting consecutive entries in a `ref'.
;;;

;; The following procedure handles sorting entries in a `ref' with multiple
;; entries:
;;
;;   (ref :bib '("smith81:disintegration" "corgan07:zeitgeist"))
;;
;; This is pleasant when entries are numbered since it allows them to appear
;; in the right order, e.g., "[2,5]".

(define (bib-sort-refs/number entry1 entry2)
  ;; Default implementation of the `bib-refs-sort-proc' custom.  Compare
  ;; bibliography entries `entry1' and `entry2' (of type `&bib-entry') for
  ;; use by `sort' in `bib-ref+'.
  (let ((ident1 (markup-option entry1 :title))
	(ident2 (markup-option entry2 :title)))
    (and (markup? ident1) (markup? ident2)
         (let ((n1 (markup-option ident1 'number))
               (n2 (markup-option ident2 'number)))
           (and (number? n1) (number? n2)
                (< n1 n2))))))



;;;
;;; Bibliography creation and entry name assignment.
;;;

(define (assign-entries-numbers! entries)
  ;; Traverse `&bib-entry' markups in ENTRIES and add them a `:title' option
  ;; whose content is a `&bib-entry-ident' markup suitably numbered.
  (let loop ((es entries)
             (i 1))
    (if (pair? es)
        (begin
          (markup-option-add! (car es)
                              :title
                              (make <markup>
                                :markup '&bib-entry-ident
                                :parent (car es)
                                :options `((number ,i))
                                :body (make <handle> :ast (car es))))
          (loop (cdr es) (+ i 1))))))

(define (assign-entries-name+years! entries)
  ;; Assign name+year-style labels to bibliography entries in ENTRIES (a list
  ;; of `&bib-entry' markups.  Such labels will look like this: "[Smith
  ;; 1984]", "[Smith & Johnson 1979]", "[Smith et al. 1980]".

  (define %letters
    '(#\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m #\n #\o #\p #\q #\r
#\s #\t #\u #\v #\w #\x #\y #\z #\A #\B #\C #\D #\E #\F #\G #\H #\I #\J #\K
#\L #\M #\N #\O #\P #\Q #\R #\S #\T #\U #\V #\W #\X #\Y #\Z))

  (define (assign-label! entry label)
    (markup-option-add! entry :title
                        (make <markup>
                          :markup '&bib-entry-ident
                          :parent entry
                          :options `((number ,label))
                          :body (make <handle> :ast entry))))

  (let ((name+year-table (make-hash-table)))
    ;; Construct NAME+YEAR-TABLE such that keys are name+year labels and
    ;; values are a list of matching entries.
    (let loop ((entries entries))
      (if (pair? entries)
          (let* ((e (car entries))
                 (author (markup-body (markup-option e 'author)))
                 (name (if (string? author)
                           (short-author-names author)
                           author))
                 (year (let ((m (markup-option e 'year)))
                         (and (markup? m) (markup-body m))))
                 (name+year (list name " " year)))
            (let ((same-named (hash-create-handle! name+year-table
                                                   name+year '())))
              (set-cdr! same-named (cons e (cdr same-named)))
              (loop (cdr entries))))))

    ;; Actually assign labels to entries.  When there are several entries per
    ;; author-year tuple (e.g., several "[Smith et al. 1984]"), they are
    ;; assigned distinguishing labels by adding a letter at the end of the
    ;; label (e.g., "[Smith et al. 1984a]").
    (hash-for-each (lambda (name+year entries)
                     (if (null? (cdr entries))
                         (assign-label! (car entries) name+year)
                         (let loop ((entries (reverse! entries))
                                    (letters %letters))
                           (if (not (null? entries))
                               (let ((letter (string (car letters))))
                                 ;; Disambiguate same-named entries.
                                 (assign-label! (car entries)
                                                (append name+year
                                                        (list letter)))
                                 (loop (cdr entries)
                                       (cdr letters)))))))
                   name+year-table)))

;*---------------------------------------------------------------------*/
;*    resolve-the-bib ...                                              */
;*---------------------------------------------------------------------*/
(define* (resolve-the-bib table n sort pred count opts
                          :optional (assign-entries-identifiers!
                                     assign-entries-numbers!))
   (if (not (bib-table? table))
       (raise (condition
               (&invalid-argument-error (proc-name "resolve-the-bib")
                                        (argument table))))
       (let* ((es (sort (hash-map->list (lambda (key val) val) table)))
	      (fes (filter (if (procedure? pred)
			       (lambda (m) (pred m n))
			       (lambda (m) (pair? (markup-option m 'used))))
			   es)))

          ;; XXX: Assigning identifiers through side-effects is somewhat
          ;; flawed since it precludes the production of several
          ;; bibliographies with different styles in a single document (e.g.,
          ;; the user manual cannot illustrate more than one style).
	  (assign-entries-identifiers! (if (eq? count 'full) es fes))

	  (make <markup>
	     :markup '&the-bibliography
	     :options opts
	     :body fes))))


;;; biblio.scm ends here
