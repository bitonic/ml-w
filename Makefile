
MAIN= Main.hs
EXEC_NAME= ML

GHC_OPTS= -Wall

DOCS_DIR= docs
PROLOGUE= README
HADDOCK_OPTS= -o $(DOCS_DIR) -p $(PROLOGUE) -h

all: $(TARGETS)
	ghc -o $(EXEC_NAME) $(GHC_OPTS) $(MAIN)

docs:
	mkdir -p $(DOCS_DIR)
	haddock $(HADDOCK_OPTS) $(MAIN)

clean:
	rm -f $(EXEC_NAME)
	rm -f *.hi *.o
	rm -rf $(DOCS_DIR)
