moduledir = $(guilemoduledir)/skribilo/source
dist_module_DATA =      parameters.scm c.scm lisp.scm xml.scm	\
			lisp-lex.scm xml-lex.scm c-lex.scm


EXTRA_DIST = lisp-lex.l xml-lex.l c-lex.l

# Building the lexers with SILex.  You must have SILex installed in
# `$(top_srcdir)/src/guile/silex'.  The Git repository already contains
# it.  Otherwise, you can fetch it from
# http://www.iro.umontreal.ca/~dube/silex-src.tar.gz .
#
# Note: Those files should normally be part of the distribution, making
# this rule useless to the user.
.l.scm:
	$(GUILE) -L $(top_srcdir)/src/guile/silex			\
	         -c '(load-from-path "lex.scm") (lex "$^" "$@")' &&	\
	mv "$@" "$@.tmp" &&						\
	echo '(define-module (skribilo source $(^:%.l=%))' > "$@" &&	\
	echo '  :use-module (skribilo lib)' >> "$@" &&			\
	echo '  :use-module (skribilo source parameters)'		\
		>> "$@" &&						\
	echo '  :use-module (srfi srfi-1)' >> "$@" &&			\
	echo '  :export (lexer-init lexer' >> "$@" &&			\
	echo '           lexer-get-func-column' >> "$@" &&		\
	echo '           lexer-get-func-offset' >> "$@" &&		\
	echo '           lexer-get-line lexer-getc' >> "$@" &&		\
	echo '           lexer-ungetc))' >> "$@" &&			\
	cat "$@.tmp" >> "$@" &&						\
	rm "$@.tmp"


include $(top_srcdir)/guile-lint.am
