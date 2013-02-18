EMACS ?= emacs
CARTON ?= carton
ECUKES ?= $(shell find elpa/ecukes-*/ecukes | tail -1)

test: unit-tests ecukes-features

unit-tests: elpa
	@echo "Stub for unit tests using ERT"

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
