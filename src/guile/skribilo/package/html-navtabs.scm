;;; html-navtabs.scm  --  Producing HTML navigation tabs.
;;; -*- coding: iso-8859-1 -*-
;;;
;;; Copyright 2004  Manuel Serrano
;;; Copyright 2007  Ludovic Courtès <ludo@gnu.org>
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

(define-module (skribilo package html-navtabs)
  :use-module (skribilo lib)
  :use-module (skribilo ast)
  :use-module (skribilo engine)
  :use-module (skribilo writer)
  :autoload   (skribilo output)        (output)
  :autoload   (skribilo package base)  (handle)
  :autoload   (skribilo engine html)   (html-width html-class html-file)
  :autoload   (skribilo parameters)    (*destination-file*)
  :use-module (skribilo utils strings)
  :use-module (skribilo utils syntax))

(skribilo-module-syntax)


(define (unspecified? obj)
  ;; Return true if OBJ is "unspecified" (see, e.g., `engine-custom').
  (eq? obj 'unspecified))

;*---------------------------------------------------------------------*/
;*    &html-navtabs-css-title ...                                      */
;*---------------------------------------------------------------------*/
(define (&html-navtabs-css-title n e)
   (display "  div.navtabs-title {
    padding: 0 0 0 0;
    margin: 0 0 0 0;
    border: 0 0 0 0;
    text-align: left;
")
   (let ((bg (engine-custom e 'title-background)))
      (unless (unspecified? bg)
	 (display "    background: ")
	 (output bg e)
	 (display ";\n")))
   (display "  }\n"))

;*---------------------------------------------------------------------*/
;*    &html-navtabs-css-tabs ...                                       */
;*---------------------------------------------------------------------*/
(define (&html-navtabs-css-tabs n e)
   (display "  div.navtabs-tabs {
    clear: left;
    margin: 0 0 0 0;
    border: 0 0 0 0;
    text-align: left;
")
   (let ((pd (engine-custom e 'html-navtabs-padding))
	 (ls (engine-custom e 'left-margin-size)))
      (display "    padding: 0 0 0 ")
      (if (and (unspecified? pd) (unspecified? ls))
	  (display "20%;\n")
	  (begin
	     (output (if (unspecified? pd)
			 (html-width ls)
			 (html-width pd))
		     e)
	     (display ";\n"))))
   (let ((bg (engine-custom e 'title-background)))
      (unless (unspecified? bg)
	 (display "    background: ")
	 (output bg e)
	 (display ";\n")))
   (display "  }\n"))

;*---------------------------------------------------------------------*/
;*    &html-navtabs-css-bar ...                                        */
;*---------------------------------------------------------------------*/
(define (&html-navtabs-css-bar n e)
   (display "  div.navtabs-bar {
    margin: 0 0 0 0;
    border: 0 0 0 0;
    text-align: left;
    border-top-color: black;
    border-top-style: solid;
    border-top-width: 1px;
")
   (let ((pd (engine-custom e 'html-navtabs-padding))
	 (ls (engine-custom e 'left-margin-size)))
      (display "    padding: 0 0 0 ")
      (if (and (unspecified? pd) (unspecified? ls))
	  (display "20%;\n")
	  (begin
	     (output (if (unspecified? pd)
			 (html-width ls)
			 (html-width pd))
		     e)
	     (display ";\n"))))
   (let ((bg1 (engine-custom e 'html-navtabs-bar-background))
	 (bg2 (engine-custom e 'left-margin-background)))
      (unless (and (unspecified? bg1) (unspecified? bg2))
	 (display "    background: ")
	 (output (if (unspecified? bg1) bg2 bg1) e)
	 (display ";\n")))
   (display "  }\n"))

;*---------------------------------------------------------------------*/
;*    &html-navtabs-css-active ...                                     */
;*---------------------------------------------------------------------*/
(define (&html-navtabs-css-active n e)
   (display "  div.navtabs-tabs a.active {
    color: black;
    border-width: 1px;
    border-color: black;
    border-style: solid;
    padding: 2px 10px 0px 10px;
    margin: 0 1px 0 0;
    text-decoration: none;
")
   (let ((bg (engine-custom e 'left-margin-background)))
      (unless (unspecified? bg)
	 (display "    background: ")
	 (output bg e)
	 (display ";\n")
	 (display "    border-bottom-color: ")
	 (output bg e)
	 (display ";\n")))
   (display "  }\n"))

;*---------------------------------------------------------------------*/
;*    &html-navtabs-css-active ...                                     */
;*---------------------------------------------------------------------*/
(define (&html-navtabs-css-inactive n e)
   (display "  div.navtabs-tabs a.inactive {
    background: white;
    color: black;
    border-width: 1px;
    border-color: black;
    border-style: solid;
    padding: 2px 10px 0px 10px;
    margin: 0 1px 0 0;
    text-decoration: none;
  }
  div.navtabs-tabs a.inactive:hover {
     color: ")
   (let ((bg (engine-custom e 'title-background)))
      (if (not (unspecified? bg))
	  (output bg e)
	  (display "#ff0000")))
   (display "
  }
"))

;*---------------------------------------------------------------------*/
;*    &html-navtabs-css-margins ...                                    */
;*---------------------------------------------------------------------*/
(define (&html-navtabs-css-margins n e)
   ;; FIXME: This is both outdated and questionable.
   (display "  td div.skribe-left-margin {
    border-width: 0 1px 0 0;
    border-right-style: solid;
    border-right-color: black;
    margin: 0;
    height: 100%;
  }
    table.skribe-margins td.skribe-left-margin {
    border-bottom-width:1px;
    border-bottom-style: solid;
    border-bottom-color: black;
  }
  table.skribe-margins td div.skribe-body {
    border-width: 1px 0 0 0;
    border-style: solid;
    border-color: black;
    margin: 0;
    height: 100%;
  }
  td div.skribe-right-margin {
    border-width: 0 0 0 1px;
    border-left-style: solid;
    border-left-color: black;
    margin: 0;
    height: 100%;
  }
  table.skribe-margins td.skribe-right-margin {
    border-bottom-width: 1px;
    border-bottom-style: solid;
    border-bottom-color: black;
  }
"))

;*---------------------------------------------------------------------*/
;*    &html-navtabs-default-tabs ...                                   */
;*---------------------------------------------------------------------*/
(define (&html-navtabs-default-tabs n e)
   (let* ((main (ast-document n))
	  (children (container-search-down
		     (lambda (c)
			(and (container? c)
			     (not (markup-option c :no-tabs))
			     (markup-option c :file)))
		     main)))
      (cons (handle main) (map handle children))))

;*---------------------------------------------------------------------*/
;*    &html-navtabs-old-generic-title ...                              */
;*---------------------------------------------------------------------*/
(define &html-navtabs-old-generic-title #f)

;*---------------------------------------------------------------------*/
;*    &html-navtabs-generic-title ...                                  */
;*---------------------------------------------------------------------*/
(define (&html-navtabs-generic-title n e)
   (display "<div class=\"navtabs-title\">\n")
   (let ((nn (if (document? n)
		 n
		 (new markup
		    (markup (markup-markup n))
		    (options (markup-options n))
		    (body (markup-option (ast-document n) :title))))))
      (output nn e &html-navtabs-old-generic-title))
   (display "</div>\n")
   (display "<div class=\"navtabs-tabs\">\n")
   (let* ((et (engine-custom e 'html-navtabs))
	  (tabs (if (procedure? et)
		    (et n e)
		    (&html-navtabs-default-tabs n e))))
      (for-each (lambda (t)
		   (if (handle? t)
		       (output (new markup
				  (markup '&html-tabs-tab)
				  (parent (ast-parent n))
				  (body t))
			       e)
		       (skribe-type-error 'tr "Invalid tabs, " t "handle")))
		tabs))
   (display "</div>\n")
   (output (new markup
	      (markup '&html-tabs-bar)
	      (body (markup-option (ast-parent n) :html-tabs-bar) e))
	   e))


;*---------------------------------------------------------------------*/
;*    HTML customization                                               */
;*---------------------------------------------------------------------*/
(when-engine-is-loaded 'html
  (lambda ()
    (let* ((he   (find-engine 'html))
           (oldd (markup-writer-get 'document he))
           (oldh (markup-writer-get '&html-header-style he)))
       (set! &html-navtabs-old-generic-title
             (markup-writer-get '&html-document-title he))
       ;; re-bindings
       (markup-writer 'chapter he
          :options '(:html-tabs-bar :title :number :file :toc :html-title :env)
          :predicate (lambda (n e)
                        (or (markup-option n :file)
                            (engine-custom e 'chapter-file)))
          :action (lambda (n e)
                     (output n e oldd)))
       (markup-writer 'section he
          :options '(:html-tabs-bar :title :number :file :toc :html-title :env)
          :predicate (lambda (n e)
                        (or (markup-option n :file)
                            (engine-custom e 'section-file)))
          :action (lambda (n e)
                     (output n e oldd)))
       (markup-writer 'subsection he
          :options '(:html-tabs-bar :title :number :file :toc :html-title :env)
          :predicate (lambda (n e)
                        (or (markup-option n :file)
                            (engine-custom e 'subsection-file)))
          :action (lambda (n e)
                     (output n e oldd)))
       (markup-writer 'subsubsection he
          :options '(:html-tabs-bar :title :number :file :toc :html-title :env)
          :predicate (lambda (n e)
                        (or (markup-option n :file)
                            (engine-custom e 'subsubsection-file)))
          :action (lambda (n e)
                     (output n e oldd)))
       (markup-writer '&html-header-style he
          :options 'all
          :before (writer-before oldh)
          :action (lambda (n e)
                     ((writer-action oldh) n e)
                     (let ((css? (engine-custom e 'html-navtabs-produce-css?)))
                       (if (or css? (unspecified? css?))
                           (for-each (lambda (m)
                                       (output (new markup
                                                    (markup m)
                                                    (parent n))
                                               e))
                                     '(&html-navtabs-css-title
                                       &html-navtabs-css-tabs
                                       &html-navtabs-css-bar
                                       &html-navtabs-css-active
                                       &html-navtabs-css-inactive
                                       &html-navtabs-css-margins)))))
          :after (writer-after oldh))
       (markup-writer '&html-document-title he :action &html-navtabs-generic-title)
       (markup-writer '&html-chapter-title he :action &html-navtabs-generic-title)
       (markup-writer '&html-section-title he :action &html-navtabs-generic-title)
       (markup-writer '&html-subsection-title he :action &html-navtabs-generic-title)
       (markup-writer '&html-subsubsection-title he :action &html-navtabs-generic-title)
       ;; html-divs
       (markup-writer '&html-navtabs-css-title :action &html-navtabs-css-title)
       (markup-writer '&html-navtabs-css-tabs :action &html-navtabs-css-tabs)
       (markup-writer '&html-navtabs-css-bar :action &html-navtabs-css-bar)
       (markup-writer '&html-navtabs-css-active :action &html-navtabs-css-active)
       (markup-writer '&html-navtabs-css-inactive :action &html-navtabs-css-inactive)
       ;;(markup-writer '&html-navtabs-css-margins :action &html-navtabs-css-margins)
       ;; html-tabs
       (markup-writer 'html-tabs he
          :options '(:unresolved :width handles)
          :before "<div class=\"navtabs-tabs\">\n"
          :after "</div>\n")
       ;; &html-tabs-bar
       (markup-writer '&html-tabs-bar he
          :options '()
          :before "<div class=\"navtabs-bar\">\n"
          :after "</div>\n")
       ;; &html-tabs-handle
       (markup-writer '&html-tabs-tab he
          :before (lambda (n e)
                     (let* ((c (handle-ast (markup-body n)))
                            (id (markup-ident c))
                            (f (html-file c e))
                            (l (let loop ((l (ast-parent n)))
                                  (if (markup-option l :no-tabs)
                                      (loop (ast-parent l))
                                      l))))
                        (format #t "<a href=\"~a#~a\" class=\"~a\""
                                (strip-ref-base (or f (*destination-file*) ""))
                                (string-canonicalize id)
                                (if (eq? c l) "active" "inactive"))
                        (html-class n)
                        (display ">")))
          :action (lambda (n e)
                     (let ((p (handle-ast (markup-body n))))
                        (if (document? p)
                            (let ((ht (markup-option p :html-title)))
                               (output (if ht ht (markup-option p :title)) e))
                            (output (markup-option p :title) e))))
          :after "</a>\n"))))


;;; arch-tag: 9538d2a2-14c9-4fa7-9320-7380404ad243
