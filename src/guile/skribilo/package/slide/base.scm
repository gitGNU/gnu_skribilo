;;; base.scm  --  Overhead transparencies, `base' engine.
;;;
;;; Copyright 2003, 2004  Manuel Serrano
;;; Copyright 2006  Ludovic Courtès <ludovic.courtes@laas.fr>
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

(define-module (skribilo package slide base)
  :use-module (skribilo utils syntax)

  :use-module (skribilo package slide)
  :use-module (skribilo writer)
  :use-module (skribilo engine)
  :use-module (skribilo ast)
  :autoload   (skribilo output)        (output)
  :autoload   (skribilo package base)  (symbol color itemize item)

  :use-module (srfi srfi-1)

  :export (%slide-outline-title %slide-outline-itemize-symbols))

(fluid-set! current-reader %skribilo-module-reader)



;;;
;;; Simple markups.
;;;
(let ((be (find-engine 'base)))

   ;; slide-pause
   (markup-writer 'slide-pause be
      :action #f)
   ;; slide-vspace
   (markup-writer 'slide-vspace be
      :options '()
      :action #f)
   ;; slide-embed
   (markup-writer 'slide-embed be
      :options '(:alt :geometry-opt)
      :action (lambda (n e)
		 (output (markup-option n :alt) e)))
   ;; slide-record
   (markup-writer 'slide-record be
      :options '(:tag :play)
      :action (lambda (n e)
		 (output (markup-body n) e)))
   ;; slide-play
   (markup-writer 'slide-play be
      :options '(:tag :color)
      :action (lambda (n e)
		 (output (markup-option n :alt) e)))
   ;; slide-play*
   (markup-writer 'slide-play* be
      :options '(:tag :color :scolor)
      :action (lambda (n e)
		 (output (markup-option n :alt) e))))


;;;
;;; Helper functions for the default topic/subtopic handling.
;;;

(define (make-subtopic-list node recurse?-proc make-entry-proc
			    itemize-symbols)
  ;; Make a list of the subtopic of `node'.  Go recursive if `recurse?-proc'
  ;; returns true.  `make-entry-proc' is passed a node and returns an entry
  ;; (a markup) for this node.  `itemize-symbols' is a (circular) list
  ;; containing the symbols to be passed to `itemize'.
  (let* ((subtopic? (lambda (n)
                      (or (is-markup? n 'slide-subtopic)
                          (is-markup? n 'slide))))
         (subtopic-types (if (is-markup? node 'slide-topic)
                             '(slide-subtopic slide)
                             '(slide-topic))))
    (if (subtopic? node)
        '()
        (apply itemize
               `(,@(if (is-markup? (car itemize-symbols) 'symbol)
                       `(:symbol ,(car itemize-symbols))
                       '())
                 ,@(map (lambda (t)
                          (item
                           (make-entry-proc t)
                           (if (recurse?-proc t)
                               (make-subtopic-list t recurse?-proc
                                                   make-entry-proc
                                                   (cdr itemize-symbols))
                               '())))
                        (filter (lambda (n)
                                  (and (markup? n)
                                       (member (markup-markup n)
                                               subtopic-types)))
                                (markup-body node))))))))

(define (make-topic-list current-topic recurse? make-entry-proc)
  ;; Make a full topic list of the document which contains
  ;; `current-topic'.  Here, `make-entry-proc' takes a topic node and
  ;; the current topic node as its arguments.
  (let ((doc (ast-document current-topic)))
    (make-subtopic-list doc
                        (lambda (t)
                          (and recurse? (eq? t current-topic)))
                        (lambda (t)
                          (make-entry-proc t current-topic))
                        %slide-outline-itemize-symbols)))

(define (make-topic-entry topic current-topic)
  ;; Produce an entry for `topic'.  Colorize it based on the fact
  ;; that the current topic is `current-topic' (it may need to be
  ;; hightlighted).
  (let ((current? (eq? topic current-topic)))
    (color :fg (if current? "#000000" "#666666")
           (apply (if current? bold (lambda (x) x))
                  (list (markup-option topic :title))))))


;;;
;;; Default topic/subtopic handling.
;;;

;; Title for the automatically-generated outline slide.
(define %slide-outline-title "")

;; Circular list of symbols to be passed to `itemize' in outlines.
(define %slide-outline-itemize-symbols
  (let loop ((names '(#t "-" "bullet" "->" "middot")))
    (if (null? names)
	'()
	(cons (if (string? (car names))
		  (symbol (car names))
		  (car names))
	      (loop (cdr names))))))


(define (make-outline-slide topic engine)
  (let* ((parent-topic (if (is-markup? topic 'slide-topic)
                           topic
                           (find1-up (lambda (n)
                                       (is-markup? n 'slide-topic))
                                     topic)))
         (unfold?      (markup-option topic :unfold?)))
    (output (slide :title %slide-outline-title :toc #f
                   :class (markup-class topic)
                   ;; The mark below is needed for cross-referencing by PDF
                   ;; bookmarks.
                   (if (markup-ident topic) (mark (markup-ident topic)) "")
                   (p (make-topic-list parent-topic unfold?
                                       make-topic-entry)))
            engine)))


(markup-writer 'slide-topic (find-engine 'base)
   :options '(:title :outline? :class :ident :unfold?)
   :action (lambda (n e)
	      (if (markup-option n :outline?)
		  (make-outline-slide n e))

	      (output (markup-body n) e)))

(markup-writer 'slide-subtopic (find-engine 'base)
   ;; FIXME: Largely untested.
   :options '(:title :outline? :class :ident :unfold?)
   :action (lambda (n e)
	      (if (markup-option n :outline?)
		  (make-outline-slide n e))

	      (output (markup-body n) e)))


;;; arch-tag: 1187ce0c-3ffc-4248-b68b-a7c77d6598b9
