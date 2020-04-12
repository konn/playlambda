{ system ? builtins.currentSystem }:
(import ./reflex-platform { inherit system; }).project ({pkgs, ...}: {
  useWarp = true;

  packages = {
    playlambda-lib = ./lib;
    playlambda-backend = ./backend;
    playlambda-frontend = ./frontend;
  };

  shells = {
    ghc = ["playlambda-lib" "playlambda-backend" "playlambda-frontend"];
    ghcjs = ["playlambda-lib" "playlambda-frontend"];
  };
})