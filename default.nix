{ compiler ? "ghcjsHEAD" }:

(import ./release.nix {inherit compiler;}).mtg-life