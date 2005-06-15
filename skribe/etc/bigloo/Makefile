#*=====================================================================*/
#*    serrano/prgm/project/skribe/etc/bigloo/Makefile                  */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Thu Oct 23 08:58:55 2003                          */
#*    Last change :  Wed Nov 17 10:51:50 2004 (serrano)                */
#*    Copyright   :  2003-04 Manuel Serrano                            */
#*    -------------------------------------------------------------    */
#*    The Bigloo etc Makefile                                          */
#*=====================================================================*/
include Makefile.skb
include ../Makefile.config

#*---------------------------------------------------------------------*/
#*    TMPDIR                                                           */
#*---------------------------------------------------------------------*/
DISTRIBTMPDIR	= /tmp
DISTRIBDIR	= $$HOME/prgm/distrib

#*---------------------------------------------------------------------*/
#*    POPULATION                                                       */
#*---------------------------------------------------------------------*/
POPULATION	= configure Makefile Makefile.tpl

#*---------------------------------------------------------------------*/
#*    distrib                                                          */
#*    -------------------------------------------------------------    */
#*    This rule must be executed in the main SKribe directory          */
#*    (i.e. ../..). They must be run with a command such as:           */
#*    "cd skribe; make -f etc/bigloo/Makefile distrib".                */
#*---------------------------------------------------------------------*/
.PHONY: distrib distrib-src distrib-jvm

distrib: distrib-src # distrib-jvm

#*--- distrib-src -----------------------------------------------------*/
distrib-src:
	@ echo "[0m[1;31m>>> distrib-src[0m"; \
	(skribedir=`pwd` \
         && /bin/rm -rf $(DISTRIBTMPDIR)/skribe \
         && mkdir -p $(DISTRIBTMPDIR)/skribe \
         && cd $(DISTRIBTMPDIR)/skribe \
         && $(MAKE) -f $$skribedir/Makefile -I $$skribedir checkout \
	 && /bin/rm -rf contribs \
         && $(MAKE) -f $$skribedir/etc/bigloo/Makefile -I $$skribedir/etc/bigloo do-distrib-src \
         && $(RM) -rf $(DISTRIBTMPDIR)/skribe$(SKRIBERELEASE))

.PHONY: do-distrib-src
do-distrib-src:
	(cd .. && \
         mv skribe skribe$(SKRIBERELEASE) && \
         tar cvfz $(DISTRIBDIR)/skribe$(SKRIBERELEASE).tar.gz skribe$(SKRIBERELEASE))

#*--- distrib-jvm -----------------------------------------------------*/
distrib-jvm:
	@ echo "[0m[1;32m>>> distrib-jvm[0m"; \
	(skribedir=`pwd` \
         && /bin/rm -rf $(DISTRIBTMPDIR)/skribe \
         && mkdir -p $(DISTRIBTMPDIR)/skribe \
	 && cd $(DISTRIBTMPDIR)/skribe \
         && $(MAKE) -f $$skribedir/Makefile -I $$skribedir checkout \
	 && /bin/rm -rf contribs \
         && $(MAKE) -f $$skribedir/etc/bigloo/Makefile -I $$skribedir/etc/bigloo do-distrib-jvm \
         && $(RM) -rf $(DISTRIBTMPDIR)/skribe)

.PHONY: do-distrib-jvm
do-distrib-jvm: lib bin lib/bigloo_s.zip
	$(RM) -f $(DISTRIBDIR)/skribe$(SKRIBERELEASE).zip
	(./configure --with-bigloo --jvm \
         && $(MAKE) \
         && cd .. \
	 && zip -qr $(ZFLAGS) $(DISTRIBDIR)/skribe$(SKRIBERELEASE).zip \
                skribe \
                -x "*~" \
                -x "*/bin/*-bigloo" \
                -x "*.class" \
                -x "*.o")

#*--- bigloo_s.zip ----------------------------------------------------*/
lib/bigloo_s.zip: lib
	cp $(FILDIR)/bigloo_s.zip $@

#*--- lib -------------------------------------------------------------*/
lib:
	mkdir -p lib

#*--- bin -------------------------------------------------------------*/
bin:
	mkdir -p bin

#*---------------------------------------------------------------------*/
#*    pop                                                              */
#*---------------------------------------------------------------------*/
.PHONY: pop

pop:
	@ echo $(POPULATION:%=etc/bigloo/%)
	@ (cd autoconf && $(MAKE) -s pop)

#*---------------------------------------------------------------------*/
#*    clean                                                            */
#*---------------------------------------------------------------------*/
.PHONY: clean distclean

clean:
	/bin/rm -f ../../lib/bigloo_s.zip

#*--- distclean -------------------------------------------------------*/
distclean:
	/bin/rm -f Makefile.skb
	/bin/rm -f ../../src/common/configure.scm



