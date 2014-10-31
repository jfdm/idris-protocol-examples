##  Makefile

IDRIS = idris
PKG   = greeter
OPTS  =
EXE   = greeter.app

build:
	${IDRIS} ${OPTS} --build ${PKG}.ipkg

build_clean: clean build

clean:
	${IDRIS} --clean ${PKG}.ipkg
	rm -rf ${EXE}

check: clean
	${IDRIS} --checkpkg ${PKG}.ipkg

doc:
	${IDRIS} --mkdoc ${PKG}.ipkg

clobber: clean
	find . -name "*~" -delete
	rm -rf greeter.app

.PHONY: clean
