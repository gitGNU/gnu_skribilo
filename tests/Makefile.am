TESTS_ENVIRONMENT =									\
  GUILE_AUTO_COMPILE=0									\
  GUILE_LOAD_COMPILED_PATH="$(top_builddir)/src/guile:$$GUILE_LOAD_COMPILED_PATH"	\
  $(GUILE) -L $(top_srcdir)/src/guile -L $(top_builddir)/src/guile

TESTS = ast.test readers/rss-2.test resolve.test

EXTRA_DIST = $(TESTS)

CLEANFILES = ast.log resolve.log rss-2.log
