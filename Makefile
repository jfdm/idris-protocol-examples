##  Makefile

PKG=example.ipkg
OPTS=
EXE=a.out

install: clean
	idris ${OPTS} --install ${PKG}

build: clean
	idris ${OPTS} --build ${PKG}

clean:
	idris --clean ${PKG}
	find . -name "*~" -delete
	rm -rf ${EXE}

check: clean
	idris --checkpkg ${PKG}


.PHONY: clean 
