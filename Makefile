ELPA_DEPENDENCIES=package-lint let-alist libmpdel navigel tablist

ELPA_ARCHIVES=melpa gnu

LINT_CHECKDOC_FILES = mpdel-browser.el mpdel-core.el mpdel.el mpdel-playlist.el mpdel-song.el mpdel-tablist.el mpdel-transient.el
LINT_PACKAGE_LINT_FILES = ${LINT_CHECKDOC_FILES}
LINT_COMPILE_FILES = ${LINT_CHECKDOC_FILES}

makel.mk:
	# Download makel
	@if [ -f ../makel/makel.mk ]; then \
		ln -s ../makel/makel.mk .; \
	else \
		curl \
		--fail --silent --show-error --insecure --location \
		--retry 9 --retry-delay 9 \
		-O https://github.com/DamienCassou/makel/raw/v1.0.0/makel.mk; \
	fi

# Include makel.mk if present
-include makel.mk
