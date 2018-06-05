PACKAGE_BASENAME = mpdel

export CI=false

emake.mk:
	curl --fail --silent --show-error --insecure --location --retry 9 --retry-delay 9 -O \
		https://raw.githubusercontent.com/vermiculus/emake.el/master/emake.mk

# Include emake.mk if present
-include emake.mk

.PHONY: check lint

check: lint

lint: PACKAGE_LISP += $(PACKAGE_TESTS)
lint: PACKAGE_ARCHIVES += melpa-stable
lint: lint-checkdoc lint-package-lint compile
