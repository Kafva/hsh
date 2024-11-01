# hsh
Basic implementations of various crypto algorithms such as MD5 (RFC 1321), SHA1
(RFC 3174) and SHA224-256 (RFC 6234).
The performance is very not good, debug logs for each step can be printed with `-d`.

```bash
# Run
cabal run hsh -- --help

# (Re)-install to ~/.local/bin
make install

# Verify against builtins
make test
```
