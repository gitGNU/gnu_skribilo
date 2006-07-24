SUBDIRS = src

EXTRA_DIST = bib.skb char.skb colframe.skb document.skb emacs.skb	\
  engine.skb enumeration.skb eq.skb examples.skb figure.skb		\
  font.skb footnote.skb htmle.skb image.skb index.skb			\
  justify.skb latexe.skb lib.skb line.skb links.skb			\
  markup.skb ornament.skb package.skb pie.skb prgm.skb sectioning.skb	\
  skribe-config.skb skribec.skb skribeinfo.skb slide.skb start.skb	\
  syntax.skb table.skb toc.skb user.skb xmle.skb

BUILT_SOURCES = user.html

skribilo = $(top_srcdir)/src/skribilo
load_path = $(top_srcdir)/src/guile:$(top_srcdir)/src/guile/skribilo/package

%.html: %.skb
	GUILE_LOAD_PATH=$(load_path):$$GUILE_LOAD_PATH \
	$(skribilo) --target html -I ../ -P ../img -o $@ $<

if HAVE_LOUT

BUILT_SOURCES += user.ps

%.lout: %.skb
	GUILE_LOAD_PATH=$(load_path):$$GUILE_LOAD_PATH \
	$(skribilo) --target lout -I ../ -P ../img -o $@ $<

%.ps: %.lout
	$(LOUT) -c $(<:%.lout=%) -o $@ $<

endif

