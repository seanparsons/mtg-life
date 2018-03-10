{ compiler ? "ghcjsHEAD" }:

let
  release = (import ./release.nix {inherit compiler;});
in release.pkgs.stdenv.lib.overrideDerivation release.mtg-life.env (oldAttrs: rec {
  nativeBuildInputs = (if builtins.hasAttr "nativeBuildInputs" oldAttrs then oldAttrs.nativeBuildInputs else []) ++ [
    release.cabal
    release.pkgs.haskellPackages.cabal2nix
  ];
})