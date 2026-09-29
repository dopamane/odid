# Open Drone ID

[ASTM F3411-22a](https://store.astm.org/f3411-22a.html)

Usage

```
odid --help
odid w basic foo | odid r
```

Install

```
cabal install odid
```

[Development](https://www.haskell.org/ghcup/)

```
cabal build
cabal test
cabal haddock
cabal run odid -- w basic foo bar.bin
cabal install
```
