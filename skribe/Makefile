#*=====================================================================*/
#*    serrano/prgm/project/skribe/Makefile                             */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Wed Jul 30 16:23:07 2003                          */
#*    Last change :  Fri May 21 16:37:53 2004 (serrano)                */
#*    Copyright   :  2003-04 Manuel Serrano                            */
#*    -------------------------------------------------------------    */
#*    The general Skribe makefile                                      */
#*=====================================================================*/
include etc/Makefile.config

#*---------------------------------------------------------------------*/
#*    DIRECTORIES                                                      */
#*---------------------------------------------------------------------*/
DIRECTORIES	= skr \
		  doc \
		  examples \
		  src \
		  emacs \
		  etc \
		  tools

POPULATIONDIRS	= $(DIRECTORIES) \
                  contribs

#*---------------------------------------------------------------------*/
#*    all                                                              */
#*---------------------------------------------------------------------*/
.PHONY: all

all:
	(cd src/$(SYSTEM) && $(MAKE))
	(cd tools && $(MAKE))
	(cd doc && $(MAKE))

#*---------------------------------------------------------------------*/
#*    install                                                          */
#*---------------------------------------------------------------------*/
.PHONY: install uninstall

install:
	for d in $(DIRECTORIES); do \
           (cd $$d && $(MAKE) install) || exit -1; \
        done

uninstall:
	for d in $(DIRECTORIES); do \
           (cd $$d && $(MAKE) uninstall) || exit -1; \
        done

#*---------------------------------------------------------------------*/
#*    revision                                                         */
#*---------------------------------------------------------------------*/
.PHONY: revision populate skribe.prj

revision: populate checkin

populate: skribe.prj
	prcs populate skribe `$(MAKE) pop`

checkin:
	prcs checkin -r$(SKRIBERELEASE).@ skribe

checkout:
	@ prcs checkout -r$(SKRIBERELEASE).@ skribe

skribe.prj:
	@ cat skribe.prj | sed -e s,"(Populate-Ignore ())","(Populate-Ignore (\"\\\\\\\\\\.o\\$$\" \"\\\\\\\\\\~$$\" \"\\\\\\\\\\.log\\$$\" \"\\\\\\\\\\.ps\\$$\" \"\\\\\\\\\\.aux\\$$\" \"\\\\\\\\\\.date_of_backup\\$$\" \"\\\\\\\\\\.so\\$$\" \"\\\\\\\\\\.a\\$$\" \"if_not_there\\$$\" \"if_mach\\$$\" \"threadlibs\\$$\"))", > skribe.dprj; $(RM) -f skribe.prj; mv skribe.dprj skribe.prj

#*---------------------------------------------------------------------*/
#*    population                                                       */
#*    -------------------------------------------------------------    */
#*    The list of all files that have to be placed inside the          */
#*    repository for revision.                                         */
#*---------------------------------------------------------------------*/
.PHONY: subpop popfilelist

subpop:
	@ for d in $(POPULATIONDIRS); do \
             (cd $$d && $(MAKE) -s pop); \
          done

pop:
	@ echo Makefile INSTALL LICENSE README README.java
	@ echo configure
	@ (for p in `$(MAKE) -s subpop`; do \
            echo $$p; \
           done) | sort

#*---------------------------------------------------------------------*/
#*    distrib                                                          */
#*---------------------------------------------------------------------*/
.PHONY: distrib distrib-jvm distrib-src

distrib:
	$(MAKE) distrib -f etc/$(SYSTEM)/Makefile -I etc/$(SYSTEM)
	(cd www && $(MAKE))

distrib-jvm:
	$(MAKE) distrib-jvm -f etc/$(SYSTEM)/Makefile -I etc/$(SYSTEM)

distrib-src:
	$(MAKE) distrib-src -f etc/$(SYSTEM)/Makefile -I etc/$(SYSTEM)

#*---------------------------------------------------------------------*/
#*    clean/distclean                                                  */
#*---------------------------------------------------------------------*/
.PHONY: clean distclean
	$(RM) -f etc/Makefile.config

clean:
	(cd src && $(MAKE) clean)
	(cd doc && $(MAKE) clean)
	(cd tools && $(MAKE) clean)
	(cd etc && $(MAKE) clean)

distclean: clean
	(cd emacs && $(MAKE) distclean)
	(cd etc && $(MAKE) distclean)

#*---------------------------------------------------------------------*/
#*    devclean/devdistclean                                            */
#*---------------------------------------------------------------------*/
.PHONY: devclean devdistclean

devclean: clean
	(cd www && $(MAKE) clean)

devdistclean: devclean distclean

