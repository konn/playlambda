#!/usr/bin/env sh
nix-shell -A shells.ghc -I . --command "$HOME/.cabal/store/ghc-8.6.5/hskll-lngg-srvr-0.1.0.0-fc6a6bc5/bin/haskell-language-server $@"
