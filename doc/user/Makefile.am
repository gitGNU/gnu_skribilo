SUBDIRS = src

EXTRA_DIST = bib.skb char.skb colframe.skb document.skb emacs.skb	\
  engine.skb enumeration.skb eq.skb examples.skb figure.skb		\
  font.skb footnote.skb htmle.skb image.skb index.skb			\
  justify.skb latexe.skb lib.skb line.skb links.skb			\
  markup.skb ornament.skb package.skb pie.skb prgm.skb sectioning.skb	\
  skribe-config.skb skribec.skb skribeinfo.skb slide.skb start.skb	\
  syntax.skb table.skb toc.skb user.skb xmle.skb			\
  lout/book-style.lout

BUILT_SOURCES = doc-config.scm
html_DATA = user.html

skribilo = $(top_builddir)/src/pre-inst-skribilo
skrflags = -I ../ -P ../img --compat=skribe
load_path = $(top_srcdir)/src/guile:$(top_srcdir)/src/guile/skribilo/package:$(top_builddir)/src/guile:$(top_srcdir)/doc/modules


.skb.html:
	GUILE_LOAD_PATH=$(load_path):$$GUILE_LOAD_PATH \
	$(skribilo) $(skrflags) --target html -o $@ $<

if HAVE_LOUT

ps_DATA = user.ps

.skb.lout:
	GUILE_LOAD_PATH=$(load_path):$$GUILE_LOAD_PATH \
	$(skribilo) $(skrflags) --target lout -o $@ $<

.lout.ps:
	$(LOUT) -r4 -I$(srcdir)/lout -c $(@:%.ps=%) -o $@ $<

endif

if HAVE_PLOTICUS
doc-config.scm:
	-echo "(define %have-ploticus? #t)" > $@
	-echo "(define %ploticus-path \"$(PLOTICUS)\")" >> $@
else
doc-config.scm:
	-echo "(define %have-ploticus? #f) (define %ploticus-path #f)" > $@
endif
