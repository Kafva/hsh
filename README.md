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

# RFC implementation
clang -m32 reference/Md5.c -o Md5
```
