
GHC=/usr/local/bin/ghc-7.0.3
#GHC=/usr/local/bin/ghc-7.0.4
#GHC=/usr/local/bin/ghc-7.1.20110617
RUNGHC=/usr/local/bin/runghc-7.0.3
OPT=-package ghc-7.0.3 -o main
#OPT=-package ghc-7.0.4 -o main
#OPT=-package ghc-7.1.20110617 -o main

all: main

main: Main.hs
	${GHC} ${OPT}  Main.hs

run: Main.hs
	${RUNGHC} ${OPT} Main.hs

test2: test2.hs
	${GHC} -o test2 test2.hs

clean:
	rm -f main test2
