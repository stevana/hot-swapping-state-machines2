## Running the code

The easiest way to get the code running is probably using the [Nix package
manager](https://nixos.org/download).

```bash
git clone https://github.com/stevana/hot-swapping-state-machines2.git
cd hot-swapping-state-machines2
nix-shell
cabal repl
```

Although using [GHCup](https://www.haskell.org/ghcup/) should work too, if you
replace `nix-shell` with `ghcup install ghc 9.8.1`.
