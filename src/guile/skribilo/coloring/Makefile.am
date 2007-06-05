guilemoduledir = $(GUILE_SITE)/skribilo/coloring
dist_guilemodule_DATA = c.scm lisp.scm xml.scm				\
			lisp-lex.l.scm xml-lex.l.scm c-lex.l.scm


EXTRA_DIST = lisp-lex.l xml-lex.l c-lex.l

# Building the lexers with SILex.  You must previously run
# `tla build-config ./arch-config' for this to run.
#
# Note: Those files should normally be part of the distribution, making
# this rule useless to the user.
%.l.scm: %.l
	$(GUILE) -L $(top_srcdir)/src/guile/silex			\
	         -c '(load-from-path "lex.scm") (lex "$^" "$@")'


include $(top_srcdir)/guile-lint.am
