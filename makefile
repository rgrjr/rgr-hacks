# Installation of public stuff.
#
#    Modification history:
#
# created.  -- rgr, 9-Jan-99.
# added rgr-html-tags.el.  -- rgr, 13-Feb-99.
# added matlab-hacks.el.  -- rgr, 24-Sep-99.
# bootcamp-install & related stuff.  -- rgr, 3-Oct-99.
#

install-dir = /usr/local/share/emacs/site-lisp

# this needs to track the contents of the rgr-hacks-public-files emacs-lisp
# variable.  -- rgr, 13-Feb-99.
public-source = matlab-hacks.el bmerc-hacks.el \
		rgr-html-hacks.el rgr-html-head.el rgr-html-nest.el \
		rgr-html-boilerplate.el rgr-html-tags.el
public-bins = ${public-source:%.el=%.elc}
# Install source, autoloads, and compiled files.
install-files = ${public-source} ${public-bins}

all:	elc-files rgr-hacks-autoloads.el
elc-files:
	emacs --batch --load compile-rgr-hacks.el -f rgr-hacks-compile-self

rgr-hacks-autoloads.el:		.
	@if [ ! -r $@ ]; then \
	    echo Creating new $@ file.; \
	    echo ';;; Autoloads for the rgr-hacks library.' > $@; \
	    echo >> $@; \
	    echo '' >> $@; \
	fi
	emacs --batch --load rgr-random-hacks.el \
		-f rgr-batch-update-autoloads $^ rgr-hacks-autoloads.el

clean:
	rm -f *.elc

install:
	install.pl -show -m 444 ${install-files} ${install-dir}
	cd ${install-dir}; emacs -batch -f batch-update-autoloads .

### Bootcamp
rgr-hacks-version = rgr-hacks-1.2
bootcamp-dist-dir = bootcamp
rgr-hacks-subdir = emacs
bootcamp-install-dir = ${bootcamp-dist-dir}/${rgr-hacks-subdir}
bootcamp-pub-dir = /usr/local/etc/httpd/htdocs/needle-doc/emacs

bootcamp-discus-files = discus/discus-date.el discus/discus-diff.el \
	discus/discus-inlines.el discus/discus-unix-date.el discus/discus.el \
	discus/psa-defstruct.el
bootcamp-compile-files = ange-ftp-hacks.el c-hacks.el html-helper-mode.el \
	pls-mode.el possibilities.el regexp-opt.el rgr-abbrev-completion.el \
	rgr-compile-hacks.el rgr-dired.el rgr-elisp-hacks.el \
	rgr-fill-comment.el rgr-hacks.el rgr-html-boilerplate.el \
	rgr-html-hacks.el rgr-html-head.el rgr-html-nest.el \
	rgr-html-tags.el rgr-lisp-hacks.el rgr-mouse-19.el rgr-mouse-20.el \
	rgr-mouse.el rgr-perl-hacks.el \
	rgr-shell-hacks.el rgr-tcl.el rgr-term-setup.el sql.el tcl.el
bootcamp-source-files = ${bootcamp-compile-files} dot-emacs.el emacs-init.el \
	from-site-start.el rgr-mouse-18.el \
	discus/discus-compile.el
# Note that we have to trust that everything is compiled correctly, since not
# everything has the right eval-when-compile forms.  -- rgr, 3-Oct-99.
bootcamp-install:
	install.pl -show -m 644 ${bootcamp-source-files} \
		${bootcamp-compile-files:%.el=%.elc} \
		${bootcamp-discus-files} ${bootcamp-discus-files:%.el=%.elc} \
		${bootcamp-pub-dir}/rgr-hacks.html \
		${bootcamp-install-dir}
	cd ${bootcamp-install-dir}; \
		emacs -batch -f batch-update-autoloads .; \
		rm -f loaddefs.el[.~]*; \
		chmod 644 loaddefs.el *.elc

${bootcamp-dist-dir}/${rgr-hacks-version}.tar.gz:
	cd ${bootcamp-dist-dir}; rm -f ${rgr-hacks-version}.tar; \
		tar cf ${rgr-hacks-version}.tar ${rgr-hacks-subdir}; \
		gzip ${rgr-hacks-version}.tar

install-random-files:
	install.pl -show -m 444 ${bootcamp-install-dir}/dot-emacs.el \
		${bootcamp-pub-dir}

install-rgr-hacks:	${bootcamp-dist-dir}/${rgr-hacks-version}.tar.gz install-random-files
	install.pl -show -m 444 \
		${bootcamp-dist-dir}/${rgr-hacks-version}.tar.gz \
		${bootcamp-pub-dir}
