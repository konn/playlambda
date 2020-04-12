{ system ? builtins.currentSystem }:
(import ./reflex-platform { inherit system; }).project ({pkgs, ...}: {
  useWarp = true;

  packages = {
    playland-lib = ./lib;
    playland-backend = ./backend;
    playland-frontend = ./frontend;
  };

  shells = {
    ghc = ["playland-lib" "playland-backend" "playland-frontend"];
    ghcjs = ["playland-lib" "playland-frontend"];
  };
})