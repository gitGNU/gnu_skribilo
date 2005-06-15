#*=====================================================================*/
#*    serrano/prgm/project/skribe/etc/bigloo/Makefile.tpl              */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Wed Nov  7 09:20:47 2001                          */
#*    Last change :  Wed Feb 18 11:23:12 2004 (serrano)                */
#*    Copyright   :  2001-04 Manuel Serrano                            */
#*    -------------------------------------------------------------    */
#*    Standard Skribe makefile to build various libraries.             */
#*=====================================================================*/

#*---------------------------------------------------------------------*/
#*    Compilers, Tools and Destinations                                */
#*---------------------------------------------------------------------*/
# The heap file
HEAP_FILE	= $(LIB)/$(TARGETNAME).heap
HEAPJVM_FILE	= $(LIB)/$(TARGETNAME).jheap
# Where to store the library class files
PBASE		= bigloo.skribe.$(TARGETNAME)
CLASS_DIR	= o/class_s/bigloo/skribe/$(TARGETNAME)
O_DIR		= o

BUNSAFEFLAGS	= -unsafe

#*---------------------------------------------------------------------*/
#*    Suffixes                                                         */
#*---------------------------------------------------------------------*/
.SUFFIXES:
.SUFFIXES: .scm .class .o

#*---------------------------------------------------------------------*/
#*    The implicit rules                                               */
#*---------------------------------------------------------------------*/
$(O_DIR)/%.o: %.scm
	$(BIGLOO) $(BUNSAFEFLAGS) $(BCFLAGS) $(BCOMMONFLAGS) -c $< -o $@

$(CLASS_DIR)/%.class: %.scm
	$(BIGLOO) $(BUNSAFEFLAGS) $(BJVMFLAGS) $(BCOMMONFLAGS) -c $< -o $@

#*---------------------------------------------------------------------*/
#*    bin                                                              */
#*---------------------------------------------------------------------*/
.PHONY: bin-c bin-jvm

#*--- bin-c -----------------------------------------------------------*/
bin-c: $(TAGS) .afile .etags $(O_DIR) $(SKRIBEBINDIR)/$(TARGETNAME).bigloo

$(SKRIBEBINDIR)/$(TARGETNAME).bigloo: $(OBJECTS)
	$(BIGLOO) $(BUNSAFEFLAGS) $(BLINKFLAGS) $(BCOMMONFLAGS) $(OBJECTS) -o $(SKRIBEBINDIR)/$(TARGETNAME).bigloo
	@ echo "$(SKRIBEBINDIR)/$(TARGETNAME).bigloo done..."
	@ echo "-------------------------------"

#*--- bin-jvm ---------------------------------------------------------*/
bin-jvm: $(TAGS) .afile .etags .jfile $(CLASS_DIR) $(SKRIBEBINDIR)/$(TARGETNAME).zip

$(SKRIBEBINDIR)/$(TARGETNAME).zip: $(CLASSES)
	@ /bin/rm -f $(SKRIBEBINDIR)/$(TARGETNAME).zip
	@ (cd $(O_DIR)/class_s; \
           $(ZIP) -q $(ZFLAGS) $(SKRIBEBINDIR)/$(TARGETNAME).zip -r .)
	@ echo "$(SKRIBEBINDIR)/$(TARGETNAME).zip done..."
	@ echo "-------------------------------"

#*---------------------------------------------------------------------*/
#*    Directories                                                      */
#*---------------------------------------------------------------------*/
$(O_DIR): 
	mkdir -p $(O_DIR)

$(CLASS_DIR): 
	mkdir -p $(CLASS_DIR)

#*---------------------------------------------------------------------*/
#*    The heap construction                                            */
#*---------------------------------------------------------------------*/
.PHONY: heap heap-c heap-jvm

heap-c: $(HEAP_FILE)
heap-jvm: $(HEAPJVM_FILE)

$(HEAP_FILE): .afile make-lib.scm
	@ \rm -f $(HEAP_FILE)
	@ $(BIGLOO) $(BHEAPFLAGS) make-lib.scm -addheap $(HEAP_FILE)
	@ echo "Heap Done..."
	@ echo "-------------------------------"

$(HEAPJVM_FILE): .jfile .afile make-lib.scm
	@ \rm -f $(HEAPJVM_FILE)
	@ $(BIGLOO) -jvm $(BHEAPFLAGS) make-lib.scm -addheap $(HEAPJVM_FILE)
	@ echo "Heap JVM Done..."
	@ echo "-------------------------------"

#*---------------------------------------------------------------------*/
#*    lib                                                              */
#*---------------------------------------------------------------------*/
.PHONY: lib-c lib-jvm

#*--- lib-c -----------------------------------------------------------*/
lib-c: $(TAGS) .afile lib.$(SHAREDSUFFIX) lib.a

lib.$(SHAREDSUFFIX): $(LIB)/lib$(TARGETNAME)_s.$(SHAREDSUFFIX) $(LIB)/lib$(TARGETNAME)_u.$(SHAREDSUFFIX)
lib.a: $(LIB)/lib$(TARGETNAME)_s.a $(LIB)/lib$(TARGETNAME)_u.a

$(LIB)/lib$(TARGETNAME)_u.$(SHAREDSUFFIX): $(LIB)/lib$(TARGETNAME)_s.$(SHAREDSUFFIX)
	cd $(LIB); \
        /bin/rm -f lib$(TARGETNAME)_u.$(SHAREDSUFFIX); \
        ln -s lib$(TARGETNAME)_s.$(SHAREDSUFFIX) lib$(TARGETNAME)_u.$(SHAREDSUFFIX)

$(LIB)/lib$(TARGETNAME)_s.$(SHAREDSUFFIX): .afile $(OBJECTS)
	@ /bin/rm -f $(LIB)/lib$(TARGETNAME)_s.$(SHAREDSUFFIX)
	@ $(LD) -o $(LIB)/lib$(TARGETNAME)_s.$(SHAREDSUFFIX) $(OBJECTS) -lm -lc
	@ echo "lib$(TARGETNAME)_s.$(SHAREDSUFFIX) Done..."
	@ echo "-------------------------------"

$(LIB)/lib$(TARGETNAME)_u.a: $(LIB)/lib$(TARGETNAME)_s.a
	cd $(LIB); \
        /bin/rm -f lib$(TARGETNAME)_u.a; \
        ln -s lib$(TARGETNAME)_s.a lib$(TARGETNAME)_u.a

$(LIB)/lib$(TARGETNAME)_s.a: .afile $(OBJECTS)
	@ /bin/rm -f $(LIB)/lib$(TARGETNAME)_s.a
	@ $(AR) $(ARFLAGS) $(LIB)/lib$(TARGETNAME)_s.a $(OBJECTS)
	@ $(RANLIB) $(LIB)/lib$(TARGETNAME)_s.a
	@ echo "lib$(TARGETNAME)_s.a Done..."
	@ echo "-------------------------------"

#*--- lib-jvm ---------------------------------------------------------*/
lib-jvm: $(TAGS) $(CLASS_DIR) lib.zip

lib.zip: .afile .jfile $(CLASSES)
	@ /bin/rm -f $(LIB)/$(TARGETNAME).zip
	@ (cd $(O_DIR)/class_s; \
	  $(ZIP) -q $(ZFLAGS) \
                 $(LIB)/$(TARGETNAME)_s.zip \
                 $(CLASS_DIR:$(O_DIR)/class_s/%=%)/*.class)
	@ echo "lib$(TARGETNAME)_s.zip done..."
	@ echo "-------------------------------"

#*---------------------------------------------------------------------*/
#*    ude                                                              */
#*---------------------------------------------------------------------*/
.PHONY: ude
ude:
	@ $(MAKE) -f Makefile .afile .etags

.afile: $(SOURCES)
	@ $(AFILE) -o .afile $(_BGL_SOURCES) 

.jfile: $(SOURCES)
	@ $(JFILE) -o .jfile -pbase $(PBASE) $(SOURCES) 

.etags: $(SOURCES)
	@ $(BTAGS) -o .etags $(_BGL_SOURCES)

#*---------------------------------------------------------------------*/
#*    stdclean                                                         */
#*---------------------------------------------------------------------*/
stdclean:
	/bin/rm -f $(OBJECTS) $(_BGL_OBJECTS:%=%.c)
	/bin/rm -f $(SKRIBEBINDIR)/$(TARGETNAME).bigloo
	/bin/rm -f $(SKRIBEBINDIR)/$(TARGETNAME).zip
	/bin/rm -f $(LIB)/lib$(TARGETNAME)_s.$(SHAREDSUFFIX)
	/bin/rm -f $(LIB)/lib$(TARGETNAME)_u.$(SHAREDSUFFIX)
	/bin/rm -f .afile .etags .jfile
	/bin/rm -rf $(O_DIR)
	/bin/rm -f *~
	/bin/rm -f *.mco

#*---------------------------------------------------------------------*/
#*    install/uninstall                                                */
#*---------------------------------------------------------------------*/
install: 
	$(MAKE) install-$(TARGET)

uninstall: 
	$(MAKE) uninstall-$(TARGET)

install-c: $(DESTDIR)$(INSTALL_BINDIR)
	cp $(SKRIBEBINDIR)/$(TARGETNAME).bigloo $(DESTDIR)$(INSTALL_BINDIR)/$(TARGETNAME).bigloo \
           && chmod $(BMASK) $(DESTDIR)$(INSTALL_BINDIR)/$(TARGETNAME).bigloo
	/bin/rm -f $(DESTDIR)$(INSTALL_BINDIR)/$(TARGETNAME)
	ln -s $(TARGETNAME).bigloo $(DESTDIR)$(INSTALL_BINDIR)/$(TARGETNAME)

uninstall-c: 
	/bin/rm $(DESTDIR)$(INSTALL_BINDIR)/$(TARGETNAME).bigloo
	/bin/rm $(DESTDIR)$(INSTALL_BINDIR)/$(TARGETNAME)

install-jvm: $(DESTDIR)$(INSTALL_FILDIR)
	cp $(SKRIBEBINDIR)/$(TARGETNAME).zip $(DESTDIR)$(INSTALL_FILDIR)/$(TARGETNAME).zip
	cp $(FILDIR)/bigloo_s.zip $(DESTDIR)$(INSTALL_FILDIR)

uninstall-jvm: 
	/bin/rm $(DESTDIR)$(INSTALL_FILDIR)/$(TARGETNAME).zip
	/bin/rm -f $(DESTDIR)$(INSTALL_FILDIR)/bigloo_s.zip

$(DESTDIR)$(INSTALL_BINDIR):
	mkdir -p $(DESTDIR)$(INSTALL_BINDIR) && chmod $(BMASK) $(DESTDIR)$(INSTALL_BINDIR)

$(FILDIR):
	mkdir -p $(FILDIR) && chmod $(BMASK) $(DESTDIR)$(INSTALL_BINDIR)

