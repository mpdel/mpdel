SRCS = mpdel.el mpdel-core.el mpdel-playlist.el mpdel-song.el mpdel-nav.el
TESTS =

export PACKAGE_FILE=$(word 1,$(SRCS))
export PACKAGE_LISP=$(SRCS) $(TESTS)
export PACKAGE_ARCHIVES=melpa

export PACKAGE_TEST_DEPS=libmpdel
export PACKAGE_TEST_ARCHIVES=melpa

EMAKE_PATH = ../emake

LOAD_PATH = -L . -L $(EMAKE_PATH)  -L ../package-lint -L ../libmpdel

BATCH = emacs -Q --batch $(LOAD_PATH) \
		--eval "(setq enable-dir-local-variables nil)"

EMAKE = $(BATCH) -l emake.el --eval "(emake (pop argv))"

CURL = curl -fsSkL --retry 9 --retry-delay 9
GITLAB=https://gitlab.petton.fr

.PHONY: all install check lint

all: check

install: emake.el
	@$(EMAKE) install

	# Install libmpdel separately as it is not in melpa yet
	$(CURL) -O ${GITLAB}/mpdel/libmpdel/raw/master/libmpdel.el

emake.el:
	@test -f $(EMAKE_PATH)/emake.el || \
	  curl -fsSkL --retry 9 --retry-delay 9 -O \
	    'https://raw.githubusercontent.com/vermiculus/emake.el/master/emake.el'

check: lint

lint :
	# Byte compile all and stop on any warning or error
	@$(EMAKE) compile ~error-on-warn
	# Run checkdoc to check Emacs Lisp conventions
	@$(EMAKE) test checkdoc

	# Run package-lint to check for packaging mistakes
	@$(EMAKE) test package-lint

	# Load all source files in the same Emacs to find conflicts
	@$(BATCH) $(addprefix -l , ${SRCS})
