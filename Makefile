
MAIN= Main.hs
EXEC_NAME= ML

GHC_OPTS= -Wall
GHCI_OPTS= -Wall

HUGS_OPTS= -98

HADDOCK_OPTS=

configure:
	cabal configure

all:
	cabal build

docs:
	cabal haddock \
	--executables --html-location='http://hackage.haskell.org/packages/archive/$$pkg/latest/doc/html'

load:
	ghci $(MAIN)

loadhugs:
	hugs $(HUGS_OPTS) $(MAIN)

run:
	runghc $(GHCI_OPTS) Main.hs

runhugs:
	runhugs $(HUGS_OPTS) Main.hs

clean:
	rm -f $(EXEC_NAME)
	rm -f *.hi *.o
	rm -rf $(DOCS_DIR)
