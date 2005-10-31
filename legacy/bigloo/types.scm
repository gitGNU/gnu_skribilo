;*=====================================================================*/
;*    serrano/prgm/project/skribe/src/bigloo/types.scm                 */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Jul 22 16:40:42 2003                          */
;*    Last change :  Thu Oct 21 13:23:17 2004 (serrano)                */
;*    Copyright   :  2003-04 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    The definition of the Skribe classes                             */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module skribe_types

   (export (abstract-class %ast
	      (parent (default #unspecified))
	      (loc (default (evmeaning-location))))

	   (class %command::%ast
	      (fmt::bstring read-only)
	      (body (default #f)))
	   
	   (class %unresolved::%ast
	      (proc::procedure read-only))
	   
	   (class %handle::%ast
	      (ast (default #f)))

	   (abstract-class %node::%ast
	      (required-options::pair-nil read-only (default '()))
	      (options::pair-nil (default '()))
	      (body (default #f)))
	   
	   (class %processor::%node
	      (combinator (default (lambda (e1 e2) e1)))
	      (procedure::procedure (default (lambda (n e) n)))
	      engine)
	   
	   (class %markup::%node
	      (markup-init)
	      (ident (default #f))
	      (class (default #f))
	      (markup::symbol read-only))

	   (class %container::%markup
	      (env::pair-nil (default '())))
	   
	   (class %document::%container)
	   
	   (class %engine
	      (ident::symbol read-only)
	      (format::bstring (default "raw"))
	      (info::pair-nil (default '()))
	      (version::obj read-only (default #unspecified))
	      (delegate read-only (default #f))
	      (writers::pair-nil (default '()))
	      (filter::obj (default #f))
	      (customs::pair-nil (default '()))
	      (symbol-table::pair-nil (default '())))

	   (class %writer
	      (ident::symbol read-only)
	      (class read-only)
	      (pred::procedure read-only)
	      (upred read-only)
	      (options::obj read-only)
	      (verified?::bool (default #f))
	      (validate (default #f))
	      (before read-only)
	      (action read-only)
	      (after read-only))

	   (class %language
	      (name::bstring read-only)
	      (fontifier read-only (default #f))
	      (extractor read-only (default #f)))
	      
	   (markup-init ::%markup)
	   (find-markups ::bstring)
	   
	   (inline ast?::bool ::obj)
	   (inline ast-parent::obj ::%ast)
	   (inline ast-loc::obj ::%ast)
	   (inline ast-loc-set!::obj ::%ast ::obj)
	   (ast-location::bstring ::%ast)

	   (new-command . inits)
	   (inline command?::bool ::obj)
	   (inline command-fmt::bstring ::%command)
	   (inline command-body::obj ::%command)
	   
	   (new-unresolved . inits)
	   (inline unresolved?::bool ::obj)
	   (inline unresolved-proc::procedure ::%unresolved)
	   
	   (new-handle . inits)
	   (inline handle?::bool ::obj)
	   (inline handle-ast::obj ::%handle)

	   (inline node?::bool ::obj)
	   (inline node-body::obj ::%node)
	   (inline node-options::pair-nil ::%node)
	   (inline node-loc::obj ::%node)

	   (new-processor . inits)
	   (inline processor?::bool ::obj)
	   (inline processor-combinator::obj ::%processor)
	   (inline processor-engine::obj ::%processor)
	   
	   (new-markup . inits)
	   (inline markup?::bool ::obj)
	   (inline is-markup?::bool ::obj ::symbol)
	   (inline markup-markup::obj ::%markup)
	   (inline markup-ident::obj ::%markup)
	   (inline markup-body::obj ::%markup)
	   (inline markup-options::pair-nil ::%markup)

	   (new-container . inits)
	   (inline container?::bool ::obj)
	   (inline container-ident::obj ::%container)
	   (inline container-body::obj ::%container)
	   (inline container-options::pair-nil ::%container)

	   (new-document . inits)
	   (inline document?::bool ::obj)
	   (inline document-ident::bool ::%document)
	   (inline document-body::bool ::%document)
	   (inline document-options::pair-nil ::%document)
	   (inline document-env::pair-nil ::%document)
	   
	   (inline engine?::bool ::obj)
	   (inline engine-ident::obj ::obj)
	   (inline engine-format::obj ::obj)
	   (inline engine-customs::pair-nil ::obj)
	   (inline engine-filter::obj ::obj)
	   (inline engine-symbol-table::pair-nil ::%engine)

	   (inline writer?::bool ::obj)
	   (inline writer-before::obj ::%writer)
	   (inline writer-action::obj ::%writer)
	   (inline writer-after::obj ::%writer)
	   (inline writer-options::obj ::%writer)

	   (inline language?::bool ::obj)
	   (inline language-name::obj ::obj)
	   (inline language-fontifier::obj ::obj)
	   (inline language-extractor::obj ::obj)
			  
	   (new-language . inits)
	   
	   (location?::bool ::obj)
	   (location-file::bstring ::pair)
	   (location-pos::int ::pair)))

;*---------------------------------------------------------------------*/
;*    skribe-instantiate ...                                           */
;*---------------------------------------------------------------------*/
(define-macro (skribe-instantiate type values . slots)
   `(begin
       (skribe-instantiate-check-values ',type ,values ',slots)
       (,(symbol-append 'instantiate::% type)
	,@(map (lambda (slot)
		  (let ((id (if (pair? slot) (car slot) slot))
			(def (if (pair? slot) (cadr slot) #f)))
		     `(,id (new-get-value ',id ,values ,def))))
	       slots))))

;*---------------------------------------------------------------------*/
;*    skribe-instantiate-check-values ...                              */
;*---------------------------------------------------------------------*/
(define (skribe-instantiate-check-values id values slots)
   (let ((bs (every (lambda (v) (not (memq (car v) slots))) values)))
      (when (pair? bs)
	 (for-each (lambda (b)
		      (error (symbol-append '|new | id)
			     "Illegal field"
			     b))
		   bs))))

;*---------------------------------------------------------------------*/
;*    object-print ...                                                 */
;*---------------------------------------------------------------------*/
(define-method (object-print obj::%ast port print-slot::procedure)
   (let* ((class      (object-class obj))
	  (class-name (class-name class)))
      (display "#|" port)
      (display class-name port)
      (display #\| port)))
      
;*---------------------------------------------------------------------*/
;*    object-display ::%ast ...                                        */
;*---------------------------------------------------------------------*/
(define-method (object-display n::%ast . port)
   (fprintf (if (pair? port) (car port) (current-output-port))
	    "<#~a>"
	    (find-runtime-type n)))

;*---------------------------------------------------------------------*/
;*    object-display ::%markup ...                                     */
;*---------------------------------------------------------------------*/
(define-method (object-display n::%markup . port)
   (fprintf (if (pair? port) (car port) (current-output-port))
	    "<#~a:~a>"
	    (find-runtime-type n)
	    (markup-markup n)))

;*---------------------------------------------------------------------*/
;*    object-write ::%markup ...                                       */
;*---------------------------------------------------------------------*/
(define-method (object-write n::%markup . port)
   (fprintf (if (pair? port) (car port) (current-output-port))
	    "<#~a:~a:~a>"
	    (find-runtime-type n)
	    (markup-markup n)
	    (find-runtime-type (markup-body n))))

;*---------------------------------------------------------------------*/
;*    *node-table*                                                     */
;*    -------------------------------------------------------------    */
;*    A private hashtable that stores all the nodes of an ast. It      */
;*    is used for retreiving a node from its identifier.               */
;*---------------------------------------------------------------------*/
(define *node-table* (make-hashtable))

;*---------------------------------------------------------------------*/
;*    ast? ...                                                         */
;*---------------------------------------------------------------------*/
(define-inline (ast? obj)
   (%ast? obj))

;*---------------------------------------------------------------------*/
;*    ast-parent ...                                                   */
;*---------------------------------------------------------------------*/
(define-inline (ast-parent obj)
   (%ast-parent obj))

;*---------------------------------------------------------------------*/
;*    ast-loc ...                                                      */
;*---------------------------------------------------------------------*/
(define-inline (ast-loc obj)
   (%ast-loc obj))

;*---------------------------------------------------------------------*/
;*    ast-loc-set! ...                                                 */
;*---------------------------------------------------------------------*/
(define-inline (ast-loc-set! obj loc)
   (%ast-loc-set! obj loc))

;*---------------------------------------------------------------------*/
;*    ast-location ...                                                 */
;*---------------------------------------------------------------------*/
(define (ast-location obj)
   (with-access::%ast obj (loc)
      (if (location? loc)
	  (let* ((fname (location-file loc))
		 (char (location-pos loc))
		 (pwd (pwd))
		 (len (string-length pwd))
		 (lenf (string-length fname))
		 (file (if (and (substring=? pwd fname len)
				(and (>fx lenf len)))
			   (substring fname len (+fx 1 (string-length fname)))
			   fname)))
	     (format "~a, char ~a" file char))
	  "no source location")))

;*---------------------------------------------------------------------*/
;*    new-command ...                                                  */
;*---------------------------------------------------------------------*/
(define (new-command . init)
   (skribe-instantiate command init
		       (parent #unspecified)
		       (loc #f)
		       fmt
		       (body #f)))

;*---------------------------------------------------------------------*/
;*    command? ...                                                     */
;*---------------------------------------------------------------------*/
(define-inline (command? obj)
   (%command? obj))

;*---------------------------------------------------------------------*/
;*    command-fmt ...                                                  */
;*---------------------------------------------------------------------*/
(define-inline (command-fmt cmd)
   (%command-fmt cmd))

;*---------------------------------------------------------------------*/
;*    command-body ...                                                 */
;*---------------------------------------------------------------------*/
(define-inline (command-body cmd)
   (%command-body cmd))

;*---------------------------------------------------------------------*/
;*    new-unresolved ...                                               */
;*---------------------------------------------------------------------*/
(define (new-unresolved . init)
   (skribe-instantiate unresolved init
		       (parent #unspecified)
		       loc
		       proc))

;*---------------------------------------------------------------------*/
;*    unresolved? ...                                                  */
;*---------------------------------------------------------------------*/
(define-inline (unresolved? obj)
   (%unresolved? obj))

;*---------------------------------------------------------------------*/
;*    unresolved-proc ...                                              */
;*---------------------------------------------------------------------*/
(define-inline (unresolved-proc unr)
   (%unresolved-proc unr))

;*---------------------------------------------------------------------*/
;*    new-handle ...                                                   */
;*---------------------------------------------------------------------*/
(define (new-handle . init)
   (skribe-instantiate handle init
		       (parent #unspecified)
		       loc
		       (ast #f)))

;*---------------------------------------------------------------------*/
;*    handle? ...                                                      */
;*---------------------------------------------------------------------*/
(define-inline (handle? obj)
   (%handle? obj))

;*---------------------------------------------------------------------*/
;*    handle-ast ...                                                   */
;*---------------------------------------------------------------------*/
(define-inline (handle-ast obj)
   (%handle-ast obj))

;*---------------------------------------------------------------------*/
;*    node? ...                                                        */
;*---------------------------------------------------------------------*/
(define-inline (node? obj)
   (%node? obj))

;*---------------------------------------------------------------------*/
;*    node-body ...                                                    */
;*---------------------------------------------------------------------*/
(define-inline (node-body obj)
   (%node-body obj))

;*---------------------------------------------------------------------*/
;*    node-options ...                                                 */
;*---------------------------------------------------------------------*/
(define-inline (node-options obj)
   (%node-options obj))

;*---------------------------------------------------------------------*/
;*    node-loc ...                                                     */
;*---------------------------------------------------------------------*/
(define-inline (node-loc obj)
   (%node-loc obj))

;*---------------------------------------------------------------------*/
;*    new-processor ...                                                */
;*---------------------------------------------------------------------*/
(define (new-processor . init)
   (skribe-instantiate processor init
		       (parent #unspecified)
		       loc
		       (combinator (lambda (e1 e2) e1))
		       engine
		       (body #f)))

;*---------------------------------------------------------------------*/
;*    processor? ...                                                   */
;*---------------------------------------------------------------------*/
(define-inline (processor? obj)
   (%processor? obj))

;*---------------------------------------------------------------------*/
;*    processor-combinator ...                                         */
;*---------------------------------------------------------------------*/
(define-inline (processor-combinator proc)
   (%processor-combinator proc))
   
;*---------------------------------------------------------------------*/
;*    processor-engine ...                                             */
;*---------------------------------------------------------------------*/
(define-inline (processor-engine proc)
   (%processor-engine proc))
   
;*---------------------------------------------------------------------*/
;*    new-markup ...                                                   */
;*---------------------------------------------------------------------*/
(define (new-markup . init)
   (skribe-instantiate markup init
		       (parent #unspecified)
		       (loc #f)
		       markup
		       ident
		       (class #f)
		       (body #f)
		       (options '())
		       (required-options '())))

;*---------------------------------------------------------------------*/
;*    markup? ...                                                      */
;*---------------------------------------------------------------------*/
(define-inline (markup? obj)
   (%markup? obj))

;*---------------------------------------------------------------------*/
;*    is-markup? ...                                                   */
;*---------------------------------------------------------------------*/
(define-inline (is-markup? obj markup)
   (and (markup? obj) (eq? (markup-markup obj) markup)))

;*---------------------------------------------------------------------*/
;*    markup-init ...                                                  */
;*    -------------------------------------------------------------    */
;*    The markup constructor simply stores in the markup table the     */
;*    news markups.                                                    */
;*---------------------------------------------------------------------*/
(define (markup-init markup)
   (bind-markup! markup))

;*---------------------------------------------------------------------*/
;*    bind-markup! ...                                                 */
;*---------------------------------------------------------------------*/
(define (bind-markup! node)
   (hashtable-update! *node-table*
		      (markup-ident node)
		      (lambda (cur) (cons node cur))
		      (list node)))

;*---------------------------------------------------------------------*/
;*    find-markups ...                                                 */
;*---------------------------------------------------------------------*/
(define (find-markups ident)
   (hashtable-get *node-table* ident))

;*---------------------------------------------------------------------*/
;*    markup-markup ...                                                */
;*---------------------------------------------------------------------*/
(define-inline (markup-markup obj)
   (%markup-markup obj))

;*---------------------------------------------------------------------*/
;*    markup-ident ...                                                 */
;*---------------------------------------------------------------------*/
(define-inline (markup-ident obj)
   (%markup-ident obj))

;*---------------------------------------------------------------------*/
;*    markup-body ...                                                  */
;*---------------------------------------------------------------------*/
(define-inline (markup-body obj)
   (%markup-body obj))

;*---------------------------------------------------------------------*/
;*    markup-options ...                                               */
;*---------------------------------------------------------------------*/
(define-inline (markup-options obj)
   (%markup-options obj))

;*---------------------------------------------------------------------*/
;*    new-container ...                                                */
;*---------------------------------------------------------------------*/
(define (new-container . init)
   (skribe-instantiate container init
		       (parent #unspecified)
		       loc
		       markup
		       ident
		       (class #f)
		       (body #f)
		       (options '())
		       (required-options '())
		       (env '())))

;*---------------------------------------------------------------------*/
;*    container? ...                                                   */
;*---------------------------------------------------------------------*/
(define-inline (container? obj)
   (%container? obj))

;*---------------------------------------------------------------------*/
;*    container-ident ...                                              */
;*---------------------------------------------------------------------*/
(define-inline (container-ident obj)
   (%container-ident obj))

;*---------------------------------------------------------------------*/
;*    container-body ...                                               */
;*---------------------------------------------------------------------*/
(define-inline (container-body obj)
   (%container-body obj))

;*---------------------------------------------------------------------*/
;*    container-options ...                                            */
;*---------------------------------------------------------------------*/
(define-inline (container-options obj)
   (%container-options obj))

;*---------------------------------------------------------------------*/
;*    new-document ...                                                 */
;*---------------------------------------------------------------------*/
(define (new-document . init)
   (skribe-instantiate document init
		       (parent #unspecified)
		       loc
		       markup
		       ident
		       (class #f)
		       (body #f)
		       (options '())
		       (required-options '())
		       (env '())))

;*---------------------------------------------------------------------*/
;*    document? ...                                                    */
;*---------------------------------------------------------------------*/
(define-inline (document? obj)
   (%document? obj))

;*---------------------------------------------------------------------*/
;*    document-options ...                                             */
;*---------------------------------------------------------------------*/
(define-inline (document-options doc)
   (%document-options doc))

;*---------------------------------------------------------------------*/
;*    document-env ...                                                 */
;*---------------------------------------------------------------------*/
(define-inline (document-env doc)
   (%document-env doc))

;*---------------------------------------------------------------------*/
;*    document-ident ...                                               */
;*---------------------------------------------------------------------*/
(define-inline (document-ident doc)
   (%document-ident doc))

;*---------------------------------------------------------------------*/
;*    document-body ...                                                */
;*---------------------------------------------------------------------*/
(define-inline (document-body doc)
   (%document-body doc))

;*---------------------------------------------------------------------*/
;*    engine? ...                                                      */
;*---------------------------------------------------------------------*/
(define-inline (engine? obj)
   (%engine? obj))

;*---------------------------------------------------------------------*/
;*    engine-ident ...                                                 */
;*---------------------------------------------------------------------*/
(define-inline (engine-ident obj)
   (%engine-ident obj))

;*---------------------------------------------------------------------*/
;*    engine-format ...                                                */
;*---------------------------------------------------------------------*/
(define-inline (engine-format obj)
   (%engine-format obj))

;*---------------------------------------------------------------------*/
;*    engine-customs ...                                               */
;*---------------------------------------------------------------------*/
(define-inline (engine-customs obj)
   (%engine-customs obj))

;*---------------------------------------------------------------------*/
;*    engine-filter ...                                                */
;*---------------------------------------------------------------------*/
(define-inline (engine-filter obj)
   (%engine-filter obj))

;*---------------------------------------------------------------------*/
;*    engine-symbol-table ...                                          */
;*---------------------------------------------------------------------*/
(define-inline (engine-symbol-table obj)
   (%engine-symbol-table obj))

;*---------------------------------------------------------------------*/
;*    writer? ...                                                      */
;*---------------------------------------------------------------------*/
(define-inline (writer? obj)
   (%writer? obj))

;*---------------------------------------------------------------------*/
;*    writer-before ...                                                */
;*---------------------------------------------------------------------*/
(define-inline (writer-before obj)
   (%writer-before obj))

;*---------------------------------------------------------------------*/
;*    writer-action ...                                                */
;*---------------------------------------------------------------------*/
(define-inline (writer-action obj)
   (%writer-action obj))

;*---------------------------------------------------------------------*/
;*    writer-after ...                                                 */
;*---------------------------------------------------------------------*/
(define-inline (writer-after obj)
   (%writer-after obj))

;*---------------------------------------------------------------------*/
;*    writer-options ...                                               */
;*---------------------------------------------------------------------*/
(define-inline (writer-options obj)
   (%writer-options obj))

;*---------------------------------------------------------------------*/
;*    language? ...                                                    */
;*---------------------------------------------------------------------*/
(define-inline (language? obj)
   (%language? obj))

;*---------------------------------------------------------------------*/
;*    language-name ...                                                */
;*---------------------------------------------------------------------*/
(define-inline (language-name lg)
   (%language-name lg))

;*---------------------------------------------------------------------*/
;*    language-fontifier ...                                           */
;*---------------------------------------------------------------------*/
(define-inline (language-fontifier lg)
   (%language-fontifier lg))

;*---------------------------------------------------------------------*/
;*    language-extractor ...                                           */
;*---------------------------------------------------------------------*/
(define-inline (language-extractor lg)
   (%language-extractor lg))

;*---------------------------------------------------------------------*/
;*    new-get-value ...                                                */
;*---------------------------------------------------------------------*/
(define (new-get-value key init def)
   (let ((c (assq key init)))
      (match-case c
	 ((?- ?v)
	  v)
	 (else
	  def))))

;*---------------------------------------------------------------------*/
;*    new-language ...                                                 */
;*---------------------------------------------------------------------*/
(define (new-language . init)
   (skribe-instantiate language init name fontifier extractor))

;*---------------------------------------------------------------------*/
;*    location? ...                                                    */
;*---------------------------------------------------------------------*/
(define (location? o)
   (match-case o
      ((at ?- ?-)
       #t)
      (else
       #f)))

;*---------------------------------------------------------------------*/
;*    location-file ...                                                */
;*---------------------------------------------------------------------*/
(define (location-file o)
   (match-case o
      ((at ?fname ?-)
       fname)
      (else
       (error 'location-file "Illegal location" o))))

;*---------------------------------------------------------------------*/
;*    location-pos ...                                                 */
;*---------------------------------------------------------------------*/
(define (location-pos o)
   (match-case o
      ((at ?- ?loc)
       loc)
      (else
       (error 'location-pos "Illegal location" o))))
