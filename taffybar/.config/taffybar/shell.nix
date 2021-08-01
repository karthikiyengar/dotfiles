{ pkgs ? import <nixpkgs> { } }:
let
  ghc = pkgs.haskellPackages.ghcWithPackages (pkgs: [
    pkgs.taffybar
    pkgs.gtk
    pkgs.xmonad-contrib
  ]);
in
pkgs.mkShell {
  buildInputs = [ ghc ];
}

