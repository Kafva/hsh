.PHONY: test build clean

# Inputfile:
#  * Acts as the input stream for hash algorithms
#  * Acts as the input message for HMAC
#  * Acts as the salt for PBKDF2
INPUTFILE ?= .testenv/input.dat

# Keyfile:
# * Unused by hash algorithms
# * Acts as the input key HMAC
# * Acts as the password for PBKDF2
KEYFILE ?= .testenv/key.dat

# Maximum key length: (2^32 - 1) * hLen 
# For sha1: hLen=20
PBKDF2_DERIVED_KEY_LENGTH ?= 60
PBKDF2_ITERATIONS ?= 1

HSH_ARGS += -k $(KEYFILE)
HSH_ARGS += -i $(PBKDF2_ITERATIONS)
HSH_ARGS += -l $(PBKDF2_DERIVED_KEY_LENGTH)

################################################################################

build: tests/rfc/RFC-6234
	cabal build -v0
	mkdir -p tests/bin
	clang -w tests/rfc/Sha1.c -o tests/bin/Sha1
	clang -w tests/rfc/Md5.c -o tests/bin/Md5
	$(MAKE) -C tests/rfc/RFC-6234
	go build -C tests -o bin/hmac  main.go
	go build -C tests -o bin/pbkdf2 main.go

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
	@if [ $(shell cabal run hsh -- -a ${1} -k $(KEYFILE) < $(INPUTFILE)) = ${2} ]; then \
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

$(KEYFILE):
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

test-hmac: build $(INPUTFILE) $(KEYFILE)
	$(call verify_alg,hmac,,golang/x/crypto/hmac,\
		tests/bin/hmac -d $(INPUTFILE) $(KEYFILE))

test-pbkdf2: build $(INPUTFILE) $(KEYFILE)
	$(call verify_alg,pbkdf2,,golang/x/crypto/pbkdf2,\
		tests/bin/pbkdf2 -d $(KEYFILE) $(INPUTFILE) \
			$(PBKDF2_ITERATIONS) \
			$(PBKDF2_DERIVED_KEY_LENGTH))

test: build $(INPUTFILE) $(KEYFILE)
	mkdir -p .testenv
	$(call verify_ok,md5,$(shell md5sum < $(INPUTFILE) | awk '{print $$1}'))
	$(call verify_ok,sha1,$(shell sha1sum < $(INPUTFILE) | awk '{print $$1}'))
	$(call verify_ok,sha224,$(shell sha224sum < $(INPUTFILE) | awk '{print $$1}'))
	$(call verify_ok,sha256,$(shell sha256sum < $(INPUTFILE) | awk '{print $$1}'))
	$(call verify_ok,hmac,$(shell tests/bin/hmac $(INPUTFILE) $(KEYFILE)))
	$(call verify_ok,pbkdf2,$(shell tests/bin/pbkdf2 $(KEYFILE) $(INPUTFILE) \
		$(PBKDF2_ITERATIONS) $(PBKDF2_DERIVED_KEY_LENGTH)))

