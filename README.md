# hsh
Basic implementation of md5 and sha1. The performance is very not good, debug
logs for each step can be printed with `-d`.

```bash
# Run
cabal run hsh -- --help

# (Re)-install to ~/.local/bin
cabal install --overwrite-policy=always

# RFC implementation of md5 (slightly modified to work on 64-bit systems)
clang -w rfc/Md5.c -o Md5
```
