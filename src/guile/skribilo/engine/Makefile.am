guilemoduledir = $(GUILE_SITE)/skribilo/engine
dist_guilemodule_DATA = base.scm context.scm html.scm html4.scm	\
			latex-simple.scm latex.scm		\
			lout.scm				\
			xml.scm

# FIXME: Guile-Lint can't be used here because of `define-skribe-module'
# and because of the custom reader syntax that's used.

#include $(top_srcdir)/guile-lint.am
