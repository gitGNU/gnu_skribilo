SUBDIRS = po src doc emacs tests

ACLOCAL_AMFLAGS = -I m4

EXTRA_DIST = TODO THANKS README.Skribe ChangeLog.Skribe

gen-ChangeLog:
	if ! test -d "$(top_srcdir)/.git"; then				\
	  echo "\`.git' not found; \`ChangeLog' not generated" >&2;	\
	elif ! git --version > /dev/null 2>&1; then			\
	  echo "git not found; \`ChangeLog' not generated" >&2;		\
	else								\
	  "$(top_srcdir)/build-aux/gitlog-to-changelog"			\
	     > "$(distdir)/cl-t";					\
	  rm -f "$(distdir)/ChangeLog";					\
	  mv "$(distdir)/cl-t" "$(distdir)/ChangeLog";			\
	fi

EXTRA_DIST += $(top_srcdir)/.version
$(top_srcdir)/.version:
	echo $(VERSION) > "$@-t" && mv "$@-t" "$@"
gen-tarball-version:
	echo "$(VERSION)" > "$(distdir)/.tarball-version"

dist-hook: gen-ChangeLog gen-tarball-version

.PHONY: gen-ChangeLog gen-tarball-version
