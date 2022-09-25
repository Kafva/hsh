SHELL=/bin/bash
EXEC=sha
CFLAGS?=-Wall -Wcompat -Widentities -Wincomplete-record-updates -Wincomplete-uni-patterns -Wmissing-export-lists -Wmissing-home-modules -Wpartial-fields -Wredundant-constraints 
GHCOPTS=-threaded
HC?=ghc
.PHONY: test dbg ghci run clean

$(EXEC): src/*.hs
	$(HC) $(CFLAGS) $(GHCOPTS) $^ -o $@

clean:
	rm -f $(EXEC) **/*.o **/*.hi *.hi *.o gen_md5_table



#==============================================================================#
test: Test.hs
	rm -f sha
	$(HC) $(CFLAGS) $(GHCOPTS) $< -o sha
dbg:
	cd src && ghci -ghci-script dbg.ghci < <(echo ":quit")
ghci:
	cd src && ghci -ghci-script dbg.ghci
run:
	make sha && printf 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'|./sha -d

gen_md5_table: util/gen_md5_table.hs
	$(HC) $(CFLAGS) $(GHCOPTS) $^ -o gen_md5_table && ./gen_md5_table
