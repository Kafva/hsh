.PHONY: test build clean

HSH_ARGS += -k $(HMAC_KEYFILE)
HMAC_KEYFILE ?= .testenv/key.dat
INPUTFILE ?= .testenv/input.dat

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

define msg
	@printf "\033[32m>>>\033[0m ${1}\n"
endef

# $1: Algorithm
# $2: Expected output
define verify_ok
	@if [ $(shell cabal run hsh -- -a ${1} -k $(HMAC_KEYFILE) < $(INPUTFILE)) = ${2} ]; then \
		printf "[ \033[92m OK \033[0m ] %-7s %s\n" ${1} ${2}; \
	else \
		printf "[ \033[91mFAIL\033[0m ] %-7s %s\n" ${1}; \
	fi
endef

# $1: Algorithm
# $2: Builtin reference command (optional)
# $3: Reference command label
# $4: Reference command
define verify_alg
	@if [ -n "${2}" ]; then \
		$(call msg,${2}); \
		${2} < $(INPUTFILE); \
	fi
	$(call msg,${3})
	${4}
	$(call msg,hsh)
	cabal run hsh -- -a ${1} $(HSH_ARGS) < $(INPUTFILE)
endef

$(INPUTFILE):
	rm -rf .testenv
	mkdir -p $(dir $@)
	dd if=/dev/urandom of=$@ bs=1K count=32

$(HMAC_KEYFILE):
	mkdir -p $(dir $@)
	@# echo -n abc > $@
	dd if=/dev/urandom of=$@ bs=32 count=1

test-md5: $(INPUTFILE)
	$(call verify_alg,md5,md5sum,Md5-RFC,\
		./tests/bin/Md5 < $(INPUTFILE))

test-sha1: $(INPUTFILE)
	$(call verify_alg,sha1,sha1sum,Sha1-RFC,\
		./tests/bin/Sha1 < $(INPUTFILE))

test-sha224: $(INPUTFILE)
	$(call verify_alg,sha224,sha224sum,Sha224-RFC,\
		tests/rfc/RFC-6234/shatest -s $(shell cat $(INPUTFILE)) -h1)

test-sha256: $(INPUTFILE)
	$(call verify_alg,sha256,sha256sum,Sha256-RFC,\
		tests/rfc/RFC-6234/shatest -s $(shell cat $(INPUTFILE)) -h2)

test-hmac: build $(INPUTFILE) $(HMAC_KEYFILE)
	$(call verify_alg,hmac,,golang/x/crypto/hmac,\
		tests/bin/hmac -d $(INPUTFILE) $(HMAC_KEYFILE))

test: build $(INPUTFILE) $(HMAC_KEYFILE)
	mkdir -p .testenv
	$(call verify_ok,md5,$(shell md5sum < $(INPUTFILE) | awk '{print $$1}'))
	$(call verify_ok,sha1,$(shell sha1sum < $(INPUTFILE) | awk '{print $$1}'))
	$(call verify_ok,sha224,$(shell sha224sum < $(INPUTFILE) | awk '{print $$1}'))
	$(call verify_ok,sha256,$(shell sha256sum < $(INPUTFILE) | awk '{print $$1}'))
	$(call verify_ok,hmac,$(shell tests/bin/hmac $(INPUTFILE) $(HMAC_KEYFILE)))

