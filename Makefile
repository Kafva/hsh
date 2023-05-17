.PHONY: test dbg ghci run clean gen_md5_table

SHELL=/bin/bash
EXEC=hsh
HC?=ghc
CFLAGS?=-Wall -Wcompat -Widentities -Wincomplete-record-updates \
	-Wincomplete-uni-patterns -Wmissing-export-lists \
	-Wmissing-home-modules -Wpartial-fields -Wredundant-constraints
GHCOPTS=-threaded -odir objs -hidir objs

#==============================================================================#

$(EXEC): src/*.hs
	@mkdir -p objs
	$(HC) $(CFLAGS) $(GHCOPTS) $^ -o $@

clean:
	rm -f $(EXEC) gen_md5_table
	rm -rf objs

#==============================================================================#
test: Test.hs
	rm -f $(EXEC)
	$(HC) $(CFLAGS) $(GHCOPTS) $< -o $(EXEC)
dbg:
	cd src && ghci -ghci-script dbg.ghci < <(echo ":quit")
ghci:
	cd src && ghci -ghci-script dbg.ghci
run:
	make $(EXEC) && printf 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'|./$(EXEC) -d

gen_md5_table: util/gen_md5_table.hs
	$(HC) $(CFLAGS) $(GHCOPTS) $^ -o gen_md5_table && ./gen_md5_table
	python3 -c 'from math import sin, floor; print(floor(2**32 * abs(sin(0+1))))'
