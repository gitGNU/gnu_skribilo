EXTRA_DIST     = skribe.el.in
lisp_DATA      = skribe.el

CLEANFILES     = $(lisp_DATA)

include $(top_srcdir)/substitute.am

.in:
	$(substitute) "$^" > "$@.tmp" &&	\
	mv "$@.tmp" "$@"
