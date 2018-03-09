{ compiler ? "ghcjsHEAD" }:

let
  release = (import ./release.nix {inherit compiler;});
in release.mtg-life.env.overrideAttrs (oldAttrs: rec {
  buildInputs = oldAttrs.buildInputs ++ [release.cabal];
});