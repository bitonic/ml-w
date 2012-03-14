
MAIN= Main.hs
EXEC_NAME= ML

GHC_OPTS= -Wall
GHCI_OPTS= -Wall

HUGS_OPTS= -98

DOCS_DIR= docs
PROLOGUE= README
HADDOCK_OPTS= -o $(DOCS_DIR) -p $(PROLOGUE) -h

all:
	ghc -o $(EXEC_NAME) $(GHC_OPTS) $(MAIN)

docs:
	mkdir -p $(DOCS_DIR)
	haddock $(HADDOCK_OPTS) $(MAIN)

load:
	ghci $(MAIN)

loadhugs:
	hugs $(HUGS_OPTS) $(MAIN)

run:
	runhaskell $(GHCI_OPTS) Main.hs

runhugs:
	runhugs $(HUGS_OPTS) Main.hs

clean:
	rm -f $(EXEC_NAME)
	rm -f *.hi *.o
	rm -rf $(DOCS_DIR)
