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

# Reference implementation
clang reference/Md5.c -o md5
```
