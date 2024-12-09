.PHONY: test profile reset-profile build clean

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
PBKDF2_DERIVED_KEY_LENGTH ?= 64
INNER_HASH_ALGORITHM ?= sha256
PBKDF2_ITERATIONS ?= 512

# Memory cost parameter for scrypt (N)
SCRYPT_MEMORY_COST ?= 8 # 32768
# Parallelisation parameter for scrypt (p)
SCRYPT_PARALLELISATION ?= 1
# Block size for scrypt (r)
SCRYPT_BLOCK_SIZE ?= 8
SCRYPT_DERIVED_KEY_LENGTH = 64
SCRYPT_PBKDF2_ITERATIONS = 1

HSH_ARGS += -k $(KEYFILE)
HSH_ARGS += -l $(PBKDF2_DERIVED_KEY_LENGTH)
HSH_ARGS += -H $(INNER_HASH_ALGORITHM)
HSH_ARGS += -N $(SCRYPT_MEMORY_COST)

################################################################################

build: reset-profile tests/rfc/RFC-6234
	cabal build -v0
	mkdir -p tests/bin
	clang -w tests/rfc/Sha1.c -o tests/bin/Sha1
	clang -w tests/rfc/Md5.c -o tests/bin/Md5
	$(MAKE) -C tests/rfc/RFC-6234
	go build -C tests -o bin/hash  main.go
	ln -fns ./hash tests/bin/hmac
	ln -fns ./hash tests/bin/pbkdf2
	ln -fns ./hash tests/bin/scrypt

tests/rfc/RFC-6234:
	git clone --depth 1 https://github.com/Madricas/RFC-6234.git $@

install: reset-profile src/*.hs
	cabal install --overwrite-policy=always

clean: reset-profile
	rm -rf dist-newstyle tests/bin .testenv

################################################################################

define msg
	@printf "\033[32m>>>\033[0m ${1}\n"
endef

# $1: Algorithm
# $2: Additional hsh arguments
# $3: Expected output
define verify_ok
	@if [ $(shell cabal run hsh -- -a ${1} $(HSH_ARGS) ${2} < $(INPUTFILE)) = ${3} ]; then \
		printf "[ \033[92m OK \033[0m ] %-7s %s\n" ${1} ${3}; \
	else \
		printf "[ \033[91mFAIL\033[0m ] %-7s %s\n" ${1} ${3}; \
	fi
endef

# $1: Algorithm
# $2: Additional hsh arguments
# $3: Builtin reference command (optional)
# $4: Reference command label
# $5: Reference command
define verify_alg
	@if [ -n "${3}" ]; then \
		$(call msg,${3}); \
		${3} < $(INPUTFILE); \
	fi
	$(call msg,${4})
	${5}
	$(call msg,hsh)
	cabal run hsh -- -a ${1} ${2} $(HSH_ARGS) < $(INPUTFILE)
endef

$(INPUTFILE):
	rm -rf .testenv
	mkdir -p $(dir $@)
	dd if=/dev/urandom of=$@ bs=1K count=32

$(KEYFILE):
	mkdir -p $(dir $@)
	@# echo -n abc > $@
	dd if=/dev/urandom of=$@ bs=1 count=32

test-md5: $(INPUTFILE)
	$(call verify_alg,md5,,md5sum,Md5-RFC,\
		./tests/bin/Md5 < $(INPUTFILE))

test-sha1: $(INPUTFILE)
	$(call verify_alg,sha1,,sha1sum,Sha1-RFC,\
		./tests/bin/Sha1 < $(INPUTFILE))

test-sha224: $(INPUTFILE)
	$(call verify_alg,sha224,,sha224sum,Sha224-RFC,\
		tests/rfc/RFC-6234/shatest -s $(shell cat $(INPUTFILE)) -h1)

test-sha256: $(INPUTFILE)
	$(call verify_alg,sha256,,sha256sum,Sha256-RFC,\
		tests/rfc/RFC-6234/shatest -s $(shell cat $(INPUTFILE)) -h2)

test-hmac: build $(INPUTFILE) $(KEYFILE)
	$(call verify_alg,hmac,,,golang/x/crypto/hmac,\
		tests/bin/hmac -H $(INNER_HASH_ALGORITHM) -d $(INPUTFILE) $(KEYFILE))

test-pbkdf2: build $(INPUTFILE) $(KEYFILE)
	$(call verify_alg,pbkdf2,-i $(PBKDF2_ITERATIONS),,golang/x/crypto/pbkdf2,\
		tests/bin/pbkdf2 -H $(INNER_HASH_ALGORITHM) -d $(KEYFILE) $(INPUTFILE) \
			$(PBKDF2_ITERATIONS) \
			$(PBKDF2_DERIVED_KEY_LENGTH))

test-scrypt: build $(INPUTFILE) $(KEYFILE)
	$(call verify_alg,scrypt,-i $(SCRYPT_PBKDF2_ITERATIONS),,golang/x/crypto/scrypt,\
		tests/bin/scrypt -d $(KEYFILE) $(INPUTFILE) \
			$(SCRYPT_MEMORY_COST) \
			$(SCRYPT_BLOCK_SIZE) \
			$(SCRYPT_PARALLELISATION) \
			$(SCRYPT_DERIVED_KEY_LENGTH))

test: build $(INPUTFILE) $(KEYFILE)
	mkdir -p .testenv
	$(call verify_ok,md5,,$(shell md5sum < $(INPUTFILE) | awk '{print $$1}'))
	$(call verify_ok,sha1,,$(shell sha1sum < $(INPUTFILE) | awk '{print $$1}'))
	$(call verify_ok,sha224,,$(shell sha224sum < $(INPUTFILE) | awk '{print $$1}'))
	$(call verify_ok,sha256,,$(shell sha256sum < $(INPUTFILE) | awk '{print $$1}'))
	$(call verify_ok,hmac,,$(shell tests/bin/hmac -H $(INNER_HASH_ALGORITHM) $(INPUTFILE) $(KEYFILE)))
	$(call verify_ok,pbkdf2,-i $(PBKDF2_ITERATIONS),$(shell tests/bin/pbkdf2 -H $(INNER_HASH_ALGORITHM) $(KEYFILE) $(INPUTFILE) \
		$(PBKDF2_ITERATIONS) $(PBKDF2_DERIVED_KEY_LENGTH)))
	$(call verify_ok,scrypt,-i $(SCRYPT_PBKDF2_ITERATIONS),$(shell tests/bin/scrypt $(KEYFILE) $(INPUTFILE) \
		$(SCRYPT_MEMORY_COST) \
		$(SCRYPT_BLOCK_SIZE) \
		$(SCRYPT_PARALLELISATION) \
		$(SCRYPT_DERIVED_KEY_LENGTH)))

reset-profile:
	rm -f *.prof 2> /dev/null
	@# Compile time template functions do not build with profiling enabled
	rm -f cabal.project.local

profile: $(INPUTFILE) reset-profile
	cabal build -v0
	cabal v2-configure --enable-profiling
	@# The program will accept runtime options after +RTS when compiled with `-rtsopts`,
	@# A .prof report is generated with `-p`.
	cabal run hsh -- +RTS -p -N -RTS $(HSH_ARGS) -a pbkdf2 < $(INPUTFILE)
	rm -f cabal.project.local
	cat *.prof

