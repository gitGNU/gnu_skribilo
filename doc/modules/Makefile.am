# FIXME: We shouldn't install these.
moduledir = $(guilemoduledir)

SOURCES =					\
  skribilo/documentation/api.scm		\
  skribilo/documentation/env.scm		\
  skribilo/documentation/extension.scm		\
  skribilo/documentation/manual.scm

EXTRA_DIST = $(SOURCES)

CLEANFILES =

SUFFIXES =

lintable = $(EXTRA_DIST)
include $(top_srcdir)/guile-lint.am
include $(top_srcdir)/guile-vm.am
