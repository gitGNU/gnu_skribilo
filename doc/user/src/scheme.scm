(use-modules (skribilo engine)       ;; provides `find-engine'
	     (skribilo evaluator)    ;; provides `evaluate-document'
	     (skribilo package base) ;; provides `chapter', etc.
             (srfi srfi-1))

(let (;; Select an engine, i.e., an output format.
      (e (find-engine 'html))

      ;; Create a document.
      (d (document #:title "Some Title"
	    (chapter #:title "The Chapter"
                (p "The paragraph...  "
                   "Text consists of "
                   "a list of strings.")
                (apply itemize
                       (map number->string
                            (iota 10)))))))

  ;; "Evaluate" the document to an HTML file.
  (with-output-to-file "foo.html"
    (lambda ()
      (evaluate-document d e))))
