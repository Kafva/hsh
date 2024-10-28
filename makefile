.PHONY: test build

build: tests/rfc/RFC-6234
	cabal build -v0
	mkdir -p tests/bin
	clang -w tests/rfc/Sha1.c -o tests/bin/Sha1
	clang -w tests/rfc/Md5.c -o tests/bin/Md5
	$(MAKE) -C tests/rfc/RFC-6234
	go build -C tests -o bin/pbkdf main.go
	go build -C tests -o bin/hmac  main.go

test: build
	tests/check.sh verify

tests/rfc/RFC-6234:
	git clone --depth 1 https://github.com/Madricas/RFC-6234.git $@

install: src/*.hs
	cabal install --overwrite-policy=always

clean:
	rm -rf dist-newstyle tests/bin
