.PHONY: verify build

HSH := $(shell find dist-newstyle -type f -name hsh)
HMAC_KEY ?= abc
INPUTFILE ?= .testenv/input.dat

export TIMEFORMAT = "%Rs"

################################################################################

build: tests/rfc/RFC-6234
	cabal build -v0
	mkdir -p tests/bin
	clang -w tests/rfc/Sha1.c -o tests/bin/Sha1
	clang -w tests/rfc/Md5.c -o tests/bin/Md5
	$(MAKE) -C tests/rfc/RFC-6234
	go build -C tests -o bin/pbkdf main.go
	go build -C tests -o bin/hmac  main.go

tests/rfc/RFC-6234:
	git clone --depth 1 https://github.com/Madricas/RFC-6234.git $@

install: src/*.hs
	cabal install --overwrite-policy=always

clean:
	rm -rf dist-newstyle tests/bin .testenv

################################################################################

# $1: Algorithm
# $2: Expected output
define verify_ok
	@if [ $(shell $(HSH) -a ${1} -k $(HMAC_KEY) < $(INPUTFILE)) = ${2} ]; then \
		printf "[ \033[92m OK \033[0m ] %-7s %s\n" ${1} ${2}; \
	else \
		printf "[ \033[91mFAIL\033[0m ] %-7s %s\n" ${1}; \
	fi
endef

$(INPUTFILE):
	rm -rf .testenv
	mkdir -p $(dir $@)
	dd if=/dev/urandom of=$@ bs=1K count=32

verify: build $(INPUTFILE)
	mkdir -p .testenv
	$(call verify_ok,md5,$(shell md5sum < $(INPUTFILE) | awk '{print $$1}'))
	$(call verify_ok,sha1,$(shell sha1sum < $(INPUTFILE) | awk '{print $$1}'))
	$(call verify_ok,sha224,$(shell sha224sum < $(INPUTFILE) | awk '{print $$1}'))
	$(call verify_ok,sha256,$(shell sha256sum < $(INPUTFILE) | awk '{print $$1}'))

