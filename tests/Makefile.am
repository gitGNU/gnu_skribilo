TESTS_ENVIRONMENT =									\
  GUILE_AUTO_COMPILE=0									\
  GUILE_LOAD_COMPILED_PATH="$(top_builddir)/src/guile:$$GUILE_LOAD_COMPILED_PATH"	\
  $(GUILE) -L $(top_srcdir)/src/guile -L $(top_builddir)/src/guile

TESTS =						\
  ast.test					\
  resolve.test					\
  engines/info.test				\
  location.test

if BUILD_RSS2_READER

TESTS += readers/rss-2.test
EXTRA_DIST = $(TESTS)

else !BUILD_RSS2_READER

EXTRA_DIST = $(TESTS) readers/rss-2.test

endif !BUILD_RSS2_READER


CLEANFILES = ast.log resolve.log rss-2.log location.log info.log
