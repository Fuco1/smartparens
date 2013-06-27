EMACS ?= emacs
CARTON ?= carton
ECUKES ?= $(shell find elpa/ecukes-*/ecukes | tail -1)

test: unit-tests ecukes-features

unit-tests: elpa
	${CARTON} exec ${EMACS} -Q -batch -L . -L tests \
		-l tests/smartparens-test.el -f ert-run-tests-batch-and-exit

ecukes-features: elpa
	${CARTON} exec ${ECUKES} features

elpa:
	mkdir -p elpa
	${CARTON} install 2> elpa/install.log

clean-elpa:
	rm -rf elpa

clean-elc:
	rm -f *.elc

clean: clean-elpa clean-elc

print-deps:
	${EMACS} --version
	@echo CARTON=${CARTON}
	@echo ECUKES=${ECUKES}

travis-ci: print-deps test
