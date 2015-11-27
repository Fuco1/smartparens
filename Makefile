EMACS ?= emacs
CASK ?= cask

test: unit-tests

unit-tests: elpa
	${CASK} exec ert-runner

elpa:
	mkdir -p elpa
	${CASK} install 2> elpa/install.log

clean-elpa:
	rm -rf elpa

clean-elc:
	rm -f *.elc test/*.elc

clean: clean-elpa clean-elc

print-deps:
	${EMACS} --version
	@echo CASK=${CASK}

travis-ci: print-deps test
