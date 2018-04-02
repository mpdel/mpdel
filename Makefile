SRCS = mpdel-core.el mpdel-playlist.el mpdel-song.el mpdel-nav.el mpdel.el
TESTS =

LOAD_PATH = -L . -L ../libmpdel -L ../package-lint

EMACSBIN ?= emacs
BATCH     = $(EMACSBIN) -Q --batch $(LOAD_PATH) \
		--eval "(setq load-prefer-newer t)" \
		--eval "(require 'package)" \
		--eval "(add-to-list 'package-archives '(\"melpa-stable\" . \"http://stable.melpa.org/packages/\"))" \
		--eval "(setq enable-dir-local-variables nil)" \
		--funcall package-initialize

CURL = curl -fsSkL --retry 9 --retry-delay 9
GITLAB=https://gitlab.petton.fr

.PHONY: all ci-dependencies check test lint

all: check

ci-dependencies:
	# Install dependencies in ~/.emacs.d/elpa
	$(BATCH) \
	--funcall package-refresh-contents \
	--eval "(package-install 'package-lint)"

	# Install libmpdel separately as it is not in melpa yet
	$(CURL) -O ${GITLAB}/mpdel/libmpdel/raw/master/libmpdel.el

check: lint

lint :
	# Byte compile all and stop on any warning or error
	$(BATCH) \
	--eval "(setq byte-compile-error-on-warn t)" \
	-f batch-byte-compile ${SRCS} ${TESTS}

	# Load all source files in the same Emacs to find conflicts
	$(BATCH) $(addprefix -l , ${SRCS})

	# Run package-lint to check for packaging mistakes
	$(BATCH) \
	--eval "(require 'package-lint)" \
	--eval "(setq enable-dir-local-variables nil)" \
	-f package-lint-batch-and-exit ${SRCS}
