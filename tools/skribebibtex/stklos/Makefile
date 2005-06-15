#
# Makefile for STklos skribebibtex
#
#           Author: Erick Gallesio [eg@essi.fr]
#    Creation date: 26-Oct-2004 18:40 (eg)
# Last file update:  8-Nov-2004 15:25 (eg)

include ../../../etc/stklos/Makefile.skb
include ../../../etc/Makefile.config

POPULATION = Makefile bibtex-lex.l bibtex-parser.y skribebibtex.stk main.stk
BINDIR     = ../../../bin
TARGET	   = skribebibtex
EXE        = $(BINDIR)/$(TARGET).stklos

all: $(EXE)

$(EXE): main.stk bibtex-lex.stk bibtex-parser.stk
	stklos-compile -l -o $(EXE) main.stk

bibtex-lex.stk: bibtex-lex.l
	stklos-genlex bibtex-lex.l bibtex-lex.stk bibtex-lex

bibtex-parser.stk: bibtex-parser.y
	stklos -f bibtex-parser.y

bibtex:	bibtex-lex.stk


#======================================================================
#	install ...
#======================================================================
install: $(INSTALL_BINDIR)
	cp $(EXE) $(INSTALL_BINDIR)/$(TARGET).stklos \
           && chmod $(BMASK) $(INSTALL_BINDIR)/$(TARGET).stklos
	rm -f $(INSTALL_BINDIR)/$(TARGET)
	ln -s $(TARGET).stklos $(INSTALL_BINDIR)/$(TARGET)

$(INSTALL_BINDIR):
	mkdir -p $(INSTALL_BINDIR) && chmod a+rx $(INSTALL_BINDIR)


#======================================================================
#	uninstall ...
#======================================================================
uninstall: 
	rm $(INSTALL_BINDIR)/$(TARGET)
	rm $(INSTALL_BINDIR)/$(TARGET).stklos


#======================================================================
#	pop ...
#======================================================================
pop:
	@echo $(POPULATION:%=tools/skribebibtex/stklos/%)

#======================================================================
#	clean ...
#======================================================================

clean:
	rm -f $(EXE) bibtex-lex.stk bibtex-parser.stk *~ 
