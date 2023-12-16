SHELL = /bin/bash -o pipefail
EXEC = hsh
HC ?= ghc
CFLAGS?=-Wall -Wcompat -Widentities -Wincomplete-record-updates \
	-Wincomplete-uni-patterns -Wmissing-export-lists \
	-Wmissing-home-modules -Wpartial-fields -Wredundant-constraints
GHCOPTS=-threaded -odir objs -hidir objs

$(EXEC): src/*.hs
	mkdir -p objs
	$(HC) $(CFLAGS) $(GHCOPTS) $^ -o $@

clean:
	rm -rf $(EXEC) objs
