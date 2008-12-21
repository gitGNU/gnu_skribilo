;;; rss-2.scm  --  A reader for RSS 2.0 files.
;;;
;;; Copyright 2008  Ludovic Courtès <ludo@gnu.org>
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

(define-module (skribilo reader rss-2)

  ;; We depend on `guile-library' (available in Debian).
  :use-module (sxml simple)
  :use-module (htmlprag)

  :use-module (ice-9 match)
  :use-module (srfi srfi-1)
  :use-module (srfi srfi-13)
  :use-module (srfi srfi-14)
  :use-module (srfi srfi-19)
  :use-module (srfi srfi-34)
  :use-module (srfi srfi-35)

  :use-module (skribilo reader)
  :use-module (skribilo utils syntax)

  :export (reader-specification))

(fluid-set! current-reader %skribilo-module-reader)

;;; Author:  Ludovic Courtès
;;;
;;; Commentary:
;;;
;;; A reader for RSS 2.0 feeds that turn a feed into a Skribilo document.
;;; See http://en.wikipedia.org/wiki/RSS for more information on RSS.
;;;
;;; Code:



;;;
;;; RSS 2.0 utilities.
;;;

(define (rss-channels feed)
  ;; Return the list of RSS 2.0 channels contained in FEED, an SXML tree.
  (match feed
    (('*TOP* ('*PI* _ ...)
             ('rss ('@ ('version "2.0"))
                   body ...))
     (filter (lambda (tag)
               (and (pair? tag)
                    (eq? (car tag) 'channel)))
             body))
    (_
     (raise (condition
             (&message
              (message (_ "input is not a valid RSS 2.0 feed"))))))))

(define (channel-items channel)
  ;; Return the list of items of CHANNEL, an SXML tree.
  (filter (lambda (tag)
            (and (pair? tag)
                 (eq? (car tag) 'item)))
          channel))

(define (channel-title channel)
  ;; Return the title of CHANNEL, the SXML tree of an RSS 2.0 channel.
  (let ((title (find-tag channel 'title)))
    (and (pair? title)
         (cadr title))))


;;;
;;; Skribilo output.
;;;

(define (warn* fmt . args)
  (apply format
         (current-error-port)
         (string-append "rss-2: " fmt "~%")
         args))

(define (html-string->parse-tree str)
  ;; Remove the leading `*TOP*'.
  (cdr (html->shtml str)))

(define (generic-tag->skribe tag)
  (let loop ((tag tag))
    (match tag
      ((? string? tag)
       tag)

      (()
       ''())

      ((and ((? string?) ...) strings)
       (string-concatenate strings))

      (('p body ...)
       (let ((body (map loop body)))
         (if (or (null? body)
                 (not (symbol? (car body))))
             `(p ,@body)
             `(p ,body))))

      (('font ('@ params ...) body ...)
       (let ((color (assq 'color params))
             (body  (map loop body)))
         (if (pair? color)
             `(color :fg ,(cadr color) ,@body)
             `(list ,@body))))

      (('img ('@ params ...) body ...)
       (let ((src (assq 'src params))
             (alt (assq 'alt params)))
         (if (pair? src)
             `(image :url ,(cadr src) ,(and (pair? alt) (cadr alt)))
             `(list ,@body))))

      (('a ('@ ('href ref) _ ...) text ...)
       (if (null? text)
           `(ref :url ,ref)
           `(ref :url ,ref :text ,@(map loop text))))

      (((or 'em 'strong) text ...)
       `(emph ,@(map loop text)))

      (((or 'i 'sl) text ...)
       `(it ,@text))

      (('b text ...)
       `(bold ,@(map loop text)))

      (((and (or 'code 'tt 'q) simple-tag) text ...)
       `(,simple-tag ,@(map loop text)))

      (('textarea ('@ params ...) body ...)
       `(pre ,@(map loop body)))

      (('pre body ...)
       `(pre ,@(map loop body)))

      (('acronym ('@ _ ...) body ...)
       ;; XXX: We don't (yet?) have an `acronym' markup.
       `(list ,@body))

      (('input ('@ params ...) body ...)
       (let ((value (assq 'value params)))
         (and (pair? value)
              `(pre (code ,(cadr value))))))

      ((or ('blockquote ('@ _ ...) body ...)
           ('blockquote body ...))
       `(blockquote ,@(map loop body)))

      (((or 'br 'hr) _ ...)
       `(linebreak))

      (((or 'ul 'ol) (and (or ('li _ ...) _) items) ...)
       (let ((items (filter pair? items)))
         (cons (if (eq? (car tag) 'ul) 'itemize 'enumerate)
               (map (lambda (item)
                      (let ((body (map loop (cdr item))))
                        (cons 'item body)))
                    items))))

      ((or ('dl ('@ _ ...) body ...)
           ('dl body ...))
       (let ((items (filter pair? body)))
         `(itemize
           ,@(let liip ((items  items)
                        (keys   '())
                        (result '()))
               (if (null? items)
                   (begin
                     (format (current-error-port) "ITEMS = ~s~%" result)
                     (reverse result))
                   (match (car items)
                     ((or ('dt ('@ _ ...) text ...)
                          ('dt text ...))
                      (liip (cdr items)
                            (cons text keys)
                            result))
                     ((or ('dd ('@ _ ...) text ...)
                          ('dd text ...))
                      (liip (cdr items)
                            '()
                            (cons `(item :key (list ,@(map loop (reverse keys)))
                                         ,@(map loop text))
                                  result)))
                     (_
                      (liip (cdr items) keys result))))))))

      ((or ('table ('@ _ ...) body ...)
           ('table body ...))
       `(table ,@(filter-map (lambda (x)
                               (and (pair? x) (loop x)))
                             body)))

      ((or ((and (or 'tr 'th 'td 'tc) tag) ('@ _ ...) body ...)
           ((and (or 'tr 'th 'td 'tc) tag) body ...))
       `(,tag ,@(map loop body)))

      (('*ENTITY* "additional" "nbsp")
       `(~))

      (('*ENTITY* "additional-char" (? string? char))
       (let ((char (string->number char)))
         (and (< char 256)
              (integer->char char))))

      ((and (((not (? symbol?)) _ ...) ...) lst)
       ;; Flatten non-markup lists.
       (loop (concatenate lst)))

      ((or ((or 'span 'div) ('@ _ ...) body ...)
           ((or 'span 'div) body ...))
       (let ((body (map loop body)))
         (if (or (null? body)
                 (not (symbol? (car body))))
             `(list ,@body)
             `(list ,body))))

      (('*PI* 'xml (? string? body))
       ;; Seen on MS-generated code: an <xml> tag in the middle of the
       ;; <description>!
       `(list ,@(loop (html-string->parse-tree body))))

      (((? symbol? unsupported-tag) rest ...)
       (warn* (_ "tag `~s' ignored") tag)
       #f)

      ((lst ...)
       (map loop lst))

      (_
       (warn* (_ "skipping tag `~a'~%") tag)
       #f))))

(define (english-date->date str)
  (let ((locale (setlocale LC_ALL)))
    (dynamic-wind
      (lambda ()
        (setlocale LC_ALL "C"))
      (lambda ()
        ;; The hack below allows us to process some of the feeds that use a
        ;; non-conforming time zone field in `pubDate' (e.g., feeds from
        ;; `livejournal.com').
        (let ((str (if (string-suffix? "GMT" str)
                       (string-append (substring str 0
                                                 (- (string-length str) 3))
                                      "+0000")
                       str)))
          (string->date str "~a, ~d ~b ~Y ~H:~M:~S ~z")))
      (lambda ()
        (setlocale LC_ALL locale)))))

(define (find-tag sxml tag)
  ;; Return the first sub-tree of SXML with tag TAG.
  (find (lambda (x)
          (and (pair? x)
               (eq? (car x) tag)))
        sxml))

(define (item->section item markup)
  ;; Turn ITEM, the SXML tree of an RSS item, into a section of type MARKUP
  ;; (e.g., `chapter' or `section').
  (let ((title (find-tag item 'title))
	(date  (find-tag item 'pubDate))
	(desc  (find-tag item 'description)))
    `(,markup :title ',(cadr title)

              (p (bold ,(string-trim-both
                         (date->string (english-date->date (cadr date))
                                       "~e ~B ~Y")))
                 ".  ")

	      ,@(generic-tag->skribe
		 (html-string->parse-tree (cadr desc))))))

(define (feed->document feed)
  ;; Return a Skribilo `(document ...)' S-exp from FEED, the SXML tree of an
  ;; RSS 2.0 feed.
  (let ((channels (rss-channels feed)))
    (if (null? channels)
        (raise (condition
                (&message
                 (message (_ "no RSS 2.0 channels found in feed")))))
        (let ((title   (channel-title (car channels)))
              (single? (null? (cdr channels))))
          ;; When there's only one channel, promote items as chapters.
          `(document :title ,title
             ,@(if single?
                   (map (lambda (item)
                          (item->section item 'chapter))
                        (channel-items (car channels)))
                   (map (lambda (channel)
                          `(chapter :title ,(channel-title channel)
                              ,@(map (lambda (item)
                                       (item->section item 'section))
                                     (channel-items channel))))
                        channels)))))))


;;;
;;; The reader.
;;;

(define (skip-white-space port)
  ;; Skip white space from PORT.  Return the last character read or the EOF
  ;; object.
  (let loop ((c (peek-char port)))
    (if (eof-object? c)
        c
        (if (char-set-contains? char-set:whitespace c)
            (begin
              (read-char port)
              (loop (peek-char port)))
            c))))

(define (make-rss-reader)
  (lambda (port)
    ;; XXX: Hack to avoid "Unexpected EOF" errors in `xml->sxml'.
    (let ((c (skip-white-space port)))
      (if (eof-object? c)
          c
          (feed->document (xml->sxml port))))))


;;;
;;; The reader specification.
;;;

(define-reader rss-2 "0.1" make-rss-reader)

;;; rss-2.scm ends here
