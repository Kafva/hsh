EXEC=sha
CFLAGS?=-Wall -Wcompat -Widentities -Wincomplete-record-updates -Wincomplete-uni-patterns -Wmissing-export-lists -Wmissing-home-modules -Wpartial-fields -Wredundant-constraints 
GHCOPTS=-threaded
HC?=ghc


$(EXEC): src/*.hs
	$(HC) $(CFLAGS) $(GHCOPTS) $^ -o $@

clean:
	rm -f $(EXEC) **/*.o **/*.hi *.hi *.o

.PHONY: test
test: Test.hs
	rm -f sha
	$(HC) $(CFLAGS) $(GHCOPTS) $< -o sha

