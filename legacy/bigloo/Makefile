#*=====================================================================*/
#*    serrano/prgm/project/skribe/src/bigloo/Makefile                  */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Mon Jul 21 18:21:11 2003                          */
#*    Last change :  Fri Jun  4 10:10:50 2004 (serrano)                */
#*    Copyright   :  2003-04 Manuel Serrano                            */
#*    -------------------------------------------------------------    */
#*    The Makefile to build the Bigloo API                             */
#*=====================================================================*/

#*---------------------------------------------------------------------*/
#*    General inclusion                                                */
#*---------------------------------------------------------------------*/
include ../../etc/bigloo/Makefile.skb

#*---------------------------------------------------------------------*/
#*    Compilers and tools                                              */
#*---------------------------------------------------------------------*/
BSKBFLAGS	= -I $(SRCDIR)/bigloo

#*---------------------------------------------------------------------*/
#*    Targets ...                                                      */
#*---------------------------------------------------------------------*/
PROJECT		= skribe
CTARGET		= $(SKRIBEBINDIR)/skribe.bigloo
JVMTARGET	= $(SKRIBEBINDIR)/skribe.zip

PBASE		= bigloo.$(PROJECT)
ODIR		= o
CLASSDIR	= class_s/bigloo/$(PROJECT)
OBJDIR		= obj/bigloo/$(PROJECT)

#*---------------------------------------------------------------------*/
#*    Objects                                                          */
#*---------------------------------------------------------------------*/
SRCDIR		= ..
SKRIBECOMMON	= param api bib index lib sui
SKRIBEBGL	= types parseargs main eval evapi \
                  output resolve verify debug read prog source \
                  lisp xml c asm engine writer color
SKRIBEINCLUDE	= api new debug

MODULES		= $(SKRIBEBGL:%=%.scm) \
                  $(SKRIBECOMMON:%=%.bgl) \
                  configure.bgl
INCLUDES	= $(SKRIBEINCLUDE:%=%.sch)
SOURCES		= $(MODULES) \
                  $(SKRIBECOMMON:%=$(SRCDIR)/common/%.scm) \
                  $(SRCDIR)/common/configure.scm \
		  $(INCLUDES)
OBJECTS		= $(SKRIBECOMMON) $(SKRIBEBGL) configure
COBJECTS	= $(OBJECTS:%=$(ODIR)/%.o)
JVMCLASSES	= $(OBJECTS:%=$(ODIR)/class_s/bigloo/$(PROJECT)/%.class)

#*---------------------------------------------------------------------*/
#*    Population                                                       */
#*---------------------------------------------------------------------*/
POPULATIONBGL	= $(MODULES) $(INCLUDES) Makefile
POPULATIONSCM	= $(SKRIBECOMMON:%=%.scm) configure.scm.in

#*---------------------------------------------------------------------*/
#*    Suffixes                                                         */
#*---------------------------------------------------------------------*/
.SUFFIXES:
.SUFFIXES: .scm .bgl .class .o .obj

#*---------------------------------------------------------------------*/
#*    All                                                              */
#*---------------------------------------------------------------------*/
.PHONY: c jvm dotnet

all: $(TARGET)

c: $(CTARGET)
jvm: $(JVMTARGET)
dotnet: 
	echo "Not implemented yet"

#*--- c ---------------------------------------------------------------*/
$(CTARGET): $(SKRIBEBINDIR) .afile $(ODIR) $(COBJECTS)
	$(BIGLOO) $(BLINKFLAGS) -o $@ $(COBJECTS)

#*--- jvm -------------------------------------------------------------*/
$(JVMTARGET): $(SKRIBEBINDIR) .afile .jfile $(ODIR) $(JVMCLASSES)
	$(RM) -f $(JVMTARGET)
	(cd $(ODIR)/class_s && \
         $(ZIP) -q $(ZFLAGS) $(JVMTARGET) -r .)

$(SKRIBEBINDIR):
	mkdir -p $(SKRIBEBINDIR)

#*---------------------------------------------------------------------*/
#*    pop                                                              */
#*---------------------------------------------------------------------*/
.PHONY: pop

pop:
	@ echo $(POPULATIONSCM:%=src/common/%)
	@ echo $(POPULATIONBGL:%=src/bigloo/%)

#*---------------------------------------------------------------------*/
#*    ude                                                              */
#*---------------------------------------------------------------------*/
.PHONY: ude .etags .afile

ude:
	@ $(MAKE) -f Makefile .afile .etags dep

.afile:
	@ $(AFILE) -o .afile $(MODULES) 

.jfile:
	@ $(JFILE) -I src -o .jfile -pbase $(PBASE) $(MODULES) 

.etags: 
	@ $(BTAGS) -o .etags $(SOURCES)

dep: 
	@(num=`grep -n '^#bdepend start' Makefile | awk -F: '{ print $$1}' -`;\
          head -`expr $$num - 1` Makefile > /tmp/Makefile.aux)
	@ $(BDEPEND) -search-path ../common \
                     -search-path ../bigloo \
                     -strict-obj-dir $(ODIR) \
                     -strict-class-dir $(CLASSDIR) \
                     -fno-mco $(SOURCES) >> /tmp/Makefile.aux
	@ mv /tmp/Makefile.aux Makefile

getbinary:
	@ echo $(PROJECT)

getsources:
	@ echo $(SOURCES)

#*---------------------------------------------------------------------*/
#*    The implicit rules                                               */
#*---------------------------------------------------------------------*/
$(ODIR)/%.o: $(SRCDIR)/bigloo/%.bgl $(SRCDIR)/common/%.scm
	$(BIGLOO) $(BCFLAGS) $(BSKBFLAGS) $(BCOMMONFLAGS) -c \
                  $(SRCDIR)/bigloo/$*.bgl $(SRCDIR)/common/$*.scm -o $@

$(ODIR)/%.o: $(SRCDIR)/bigloo/%.scm
	$(BIGLOO) $(BCFLAGS) $(BSKBFLAGS) $(BCOMMONFLAGS) -c \
                  $(SRCDIR)/bigloo/$*.scm -o $@

$(ODIR)/class_s/bigloo/$(PROJECT)/%.class: \
	$(SRCDIR)/bigloo/%.bgl $(SRCDIR)/common/%.scm
	$(BIGLOO) $(BJVMFLAGS) $(BSKBFLAGS) $(BCOMMONFLAGS) -c \
                  $(SRCDIR)/bigloo/$*.bgl $(SRCDIR)/common/$*.scm -o $@

$(ODIR)/class_s/bigloo/$(PROJECT)/%.class: $(SRCDIR)/bigloo/%.scm
	$(BIGLOO) $(BJVMFLAGS) $(BSKBFLAGS) $(BCOMMONFLAGS) -c \
                  $(SRCDIR)/bigloo/$*.scm -o $@

$(OBJDIR)/%.obj: src/%.scm
	$(BIGLOO) $(BDNFLAGS) $(BCOMMONFLAGS) -c $< -o $@

#*---------------------------------------------------------------------*/
#*    Ad hoc rules                                                     */
#*---------------------------------------------------------------------*/
$(ODIR):
	mkdir -p $(ODIR)

$(CLASSDIR):
	mkdir -p $(CLASSDIR)

$(OBJDIR):
	mkdir -p $(OBJDIR)


#*---------------------------------------------------------------------*/
#*    install/uninstall                                                */
#*---------------------------------------------------------------------*/
.PHONY: install uninstall install-c uninstall-c install-jvm uninstall-jvm

install: 
	$(MAKE) install-$(TARGET)

uninstall: 
	$(MAKE) uninstall-$(TARGET)

install-c: $(DESTDIR)$(INSTALL_BINDIR)
	cp $(CTARGET) $(DESTDIR)$(INSTALL_BINDIR)/skribe.bigloo \
           && chmod $(BMASK) $(DESTDIR)$(INSTALL_BINDIR)/skribe.bigloo
	$(RM) -f $(DESTDIR)$(INSTALL_BINDIR)/skribe
	ln -s skribe.bigloo $(DESTDIR)$(INSTALL_BINDIR)/skribe

uninstall-c: 
	$(RM) -f $(DESTDIR)$(INSTALL_BINDIR)/skribe.bigloo
	$(RM) -f $(DESTDIR)$(INSTALL_BINDIR)/skribe

install-jvm: $(DESTDIR)$(INSTALL_FILDIR)
	cp $(JVMTARGET) $(DESTDIR)$(INSTALL_FILDIR)/skribe.zip
	cp $(FILDIR)/bigloo_s.zip $(DESTDIR)$(INSTALL_FILDIR)

uninstall-jvm: 
	$(RM) -f $(DESTDIR)$(INSTALL_FILDIR)/skribe.zip
	$(RM) -f $(DESTDIR)$(INSTALL_FILDIR)/bigloo_s.zip

$(DESTDIR)$(INSTALL_BINDIR):
	mkdir -p $(DESTDIR)$(INSTALL_BINDIR) && chmod $(BMASK) $(DESTDIR)$(INSTALL_BINDIR)

$(DESTDIR)$(INSTALL_FILDIR):
	mkdir -p $(DESTDIR)$(INSTALL_FILDIR) && chmod $(BMASK) $(DESTDIR)$(INSTALL_FILDIR)

#*---------------------------------------------------------------------*/
#*    Clean                                                            */
#*---------------------------------------------------------------------*/
clean:
	$(RM) -f .afile 
	$(RM) -f .jfile
	$(RM) -rf $(ODIR)
	$(RM) -f $(CTARGET) 
	$(RM) -f $(JVMTARGET) 

#*---------------------------------------------------------------------*/
#*    Cleanall                                                         */
#*---------------------------------------------------------------------*/
cleanall: clean

#*---------------------------------------------------------------------*/
#*    Manual dependency                                                */
#*---------------------------------------------------------------------*/
o/eval.o o/class/bigloo/skribe/eval.class: \
      $(SRCDIR)/bigloo/api.bgl $(SRCDIR)/common/api.scm

#bdepend start (don't edit)
#*---------------------------------------------------------------------*/
#*    Dependencies ...                                                 */
#*---------------------------------------------------------------------*/
o/index.o class_s/bigloo/skribe/index.class: ../bigloo/new.sch 
o/bib.o class_s/bigloo/skribe/bib.class: ../bigloo/new.sch 
o/writer.o class_s/bigloo/skribe/writer.class: ../bigloo/debug.sch 
o/lisp.o class_s/bigloo/skribe/lisp.class: ../bigloo/new.sch 
o/lib.o class_s/bigloo/skribe/lib.class: ../bigloo/debug.sch 
o/resolve.o class_s/bigloo/skribe/resolve.class: ../bigloo/debug.sch 
o/api.o class_s/bigloo/skribe/api.class: ../bigloo/new.sch               \
      ../bigloo/api.sch 
o/eval.o class_s/bigloo/skribe/eval.class: ../bigloo/debug.sch 
o/xml.o class_s/bigloo/skribe/xml.class: ../bigloo/new.sch 
o/parseargs.o class_s/bigloo/skribe/parseargs.class: ../bigloo/debug.sch 
o/prog.o class_s/bigloo/skribe/prog.class: ../bigloo/new.sch 
o/verify.o class_s/bigloo/skribe/verify.class: ../bigloo/debug.sch 
o/sui.o class_s/bigloo/skribe/sui.class: ../bigloo/debug.sch 
o/verify.o class_s/bigloo/skribe/verify.class: ../bigloo/debug.sch 
o/source.o class_s/bigloo/skribe/source.class: ../bigloo/new.sch 
o/bib.o class_s/bigloo/skribe/bib.class: ../bigloo/new.sch 
o/asm.o class_s/bigloo/skribe/asm.class: ../bigloo/new.sch 
o/source.o class_s/bigloo/skribe/source.class: ../bigloo/new.sch 
o/engine.o class_s/bigloo/skribe/engine.class: ../bigloo/debug.sch 
o/engine.o class_s/bigloo/skribe/engine.class: ../bigloo/debug.sch 
o/lib.o class_s/bigloo/skribe/lib.class: ../bigloo/debug.sch 
o/c.o class_s/bigloo/skribe/c.class: ../bigloo/new.sch 
o/writer.o class_s/bigloo/skribe/writer.class: ../bigloo/debug.sch 
o/xml.o class_s/bigloo/skribe/xml.class: ../bigloo/new.sch 
o/main.o class_s/bigloo/skribe/main.class: ../bigloo/debug.sch 
o/output.o class_s/bigloo/skribe/output.class: ../bigloo/debug.sch 
o/prog.o class_s/bigloo/skribe/prog.class: ../bigloo/new.sch 
o/output.o class_s/bigloo/skribe/output.class: ../bigloo/debug.sch 
o/resolve.o class_s/bigloo/skribe/resolve.class: ../bigloo/debug.sch 
o/sui.o class_s/bigloo/skribe/sui.class: ../bigloo/debug.sch 
o/asm.o class_s/bigloo/skribe/asm.class: ../bigloo/new.sch 
o/eval.o class_s/bigloo/skribe/eval.class: ../bigloo/debug.sch 
o/c.o class_s/bigloo/skribe/c.class: ../bigloo/new.sch 
o/index.o class_s/bigloo/skribe/index.class: ../bigloo/new.sch 
o/lisp.o class_s/bigloo/skribe/lisp.class: ../bigloo/new.sch 
o/api.o class_s/bigloo/skribe/api.class: ../bigloo/new.sch               \
      ../bigloo/api.sch 
o/parseargs.o class_s/bigloo/skribe/parseargs.class: ../bigloo/debug.sch 

#bdepend stop
