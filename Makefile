##  Makefile

IDRIS = idris
PKG   = example
OPTS  =
EXE   = a.out

install: clean
	${IDRIS} ${OPTS} --install ${PKG}.ipkg

build: clean
	${IDRIS} ${OPTS} --build ${PKG}.ipkg

clean:
	${IDRIS} --clean ${PKG}.ipkg
	rm -rf ${EXE}

check: clean
	${IDRIS} --checkpkg ${PKG}.ipkg

doc:
	${IDRIS} --mkdoc ${PKG}.ipkg

clobber: clean
	find . -name "*~" -delete

.PHONY: clean install build clean check doc clobber

