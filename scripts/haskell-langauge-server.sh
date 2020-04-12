#!/usr/bin/env sh
nix-shell -A shells.ghc --run "haskell-language-server $@"
