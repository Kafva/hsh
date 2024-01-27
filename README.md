# hsh
Basic implementation of MD5 (RFC 1321) and SHA1 (RFC 3174).
The performance is very not good, debug logs for each step can be printed with `-d`.

```bash
# Run
cabal run hsh -- --help

# (Re)-install to ~/.local/bin
cabal install --overwrite-policy=always

#
# RFC implementations:
#
# Modified to work on 64-bit systems
clang -w rfc/Md5.c -o Md5

# Modified to use first block from stdin
clang -w rfc/Sha1.c -o Sha1
```
