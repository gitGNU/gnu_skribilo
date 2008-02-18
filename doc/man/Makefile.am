EXTRA_DIST = skribilo.1.in
man1_MANS  = skribilo.1

CLEANFILES = $(man1_MANS)


include $(top_srcdir)/substitute.am

.in:
	$(substitute) "$^" > "$@.tmp" &&	\
	mv "$@.tmp" "$@"
