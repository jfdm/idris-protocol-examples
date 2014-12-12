##  Makefile

IDRIS = idris
PKG   = rfc
OPTS  =

install: clean
	${IDRIS} ${OPTS} --install ${PKG}.ipkg

build:
	${IDRIS} ${OPTS} --build ${PKG}.ipkg

build_clean: clean build

clean:
	${IDRIS} --clean ${PKG}.ipkg
	find . -name "*~" -delete

check: clean
	${IDRIS} --checkpkg ${PKG}.ipkg

doc:
	${IDRIS} --mkdoc ${PKG}.ipkg

clobber: clean
	rm -rf ${EXE}

.PHONY: clean install build clean check doc clobber
