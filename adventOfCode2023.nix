{pkgs, ...}: let
  haskell = pkgs.callPackage ./haskell.nix {};
in
  haskell.callCabal2nix "adventOfCode2023" ./. {}
