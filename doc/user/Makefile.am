BUILT_SOURCES = user.html

skribilo = $(top_srcdir)/src/guile/skribilo.scm
load_path = $(top_srcdir)/src/guile:$(top_srcdir)/src/guile/skribilo/package

%.html: %.skb
	GUILE_LOAD_PATH=$(load_path):$$GUILE_LOAD_PATH \
	$(skribilo) --target html -I ../ -o $@ $<

if HAVE_LOUT

BUILT_SOURCES += user.ps

%.lout: %.skb
	GUILE_LOAD_PATH=$(load_path):$$GUILE_LOAD_PATH \
	$(skribilo) --target lout -I ../ -o $@ $<

%.ps: %.lout
	$(LOUT) -c $(<:%.lout=%) -o $@ $<

endif

