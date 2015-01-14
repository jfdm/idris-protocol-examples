##  Makefile

IDRIS = idris
PKGS = -p effects -p protocols -p rfc
OPTS =

all : time-all echo-all knock-all tcp

knock-all : knock knock-args

knock :
	${IDRIS} ${PKGS} ${OPTS} KnockKnock.idr -o knockknock

knock-args :
	${IDRIS} ${PKGS} ${OPTS} KnockKnockArgs.idr -o knockknock-args

tcp :
	${IDRIS} ${PKGS} ${OPTS} TCPHandshake.idr -o tcpshake

tcpfail :
	${IDRIS} ${PKGS} ${OPTS} TCPHandshakeFail.idr -o tcpshake

time-all : daytime time time-alt

daytime :
	${IDRIS} ${PKGS} ${OPTS} DayTime.idr -o daytime

time :
	${IDRIS} ${PKGS} ${OPTS} Time.idr -o time

time-alt :
	${IDRIS} ${PKGS} ${OPTS} TimeAlt.idr -o time-alt

# chargen :
# 	${IDRIS} ${PKGS} ${OPTS} CharGenProcess.idr -o chargen

echo-all : echo-proto echo-naive echo

echo :
	${IDRIS} ${PKGS} ${OPTS} EchoProcess.idr -o echo

echo-proto :
	${IDRIS} ${PKGS} ${OPTS} EchoProtocol.idr -o echo-proto

echo-naive :
	${IDRIS} ${PKGS} ${OPTS} EchoNaive.idr -o echo-naive

clean :
	find . -name "*.ibc" -delete

clobber: clean
	find . -name "*~" -delete
	rm -rf echo-naive echo-proto echo daytime time time-alt knockknock knockknock-args tcpshake

.PHONY: clean
