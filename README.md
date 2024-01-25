# hsh
Hash algorithm implementations

```bash
cabal run hsh -- --help

# Debugging
cat << EOF > .run.ghci
:load src/Main.hs
:main
EOF

cabal repl
(ghci) :script .run.ghci

# RFC implementation (slightly modified to work on 64-bit systems)
clang -w rfc/Md5.c -o Md5
```
