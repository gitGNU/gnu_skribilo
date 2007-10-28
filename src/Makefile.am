SUBDIRS = guile

EXTRA_DIST = skribilo.in

bin_SCRIPTS = skribilo
CLEANFILES  = $(bin_SCRIPTS)

substitute = sed -e 's,[@]guilemoduledir[@],$(guilemoduledir),g'	\
		 -e 's,[@]GUILE[@],$(GUILE),g'				\
		 -e 's,[@]PACKAGE_STRING[@],$(PACKAGE_STRING),g'	\
		 -e 's,[@]PACKAGE_NAME[@],$(PACKAGE_NAME),g'		\
		 -e 's,[@]PACKAGE_VERSION[@],$(PACKAGE_VERSION),g'

SUFFIXES = .in

.in:
	$(substitute) "$^" > "$@.tmp" &&	\
	chmod +x "$@.tmp" &&			\
	mv "$@.tmp" "$@"
