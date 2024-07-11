{ pkgs ? import <nixpkgs> {} }:

let
  inherit (pkgs) stdenv;
  yq = import ./yq.nix { inherit pkgs; };
in
stdenv.mkDerivation {
  name = "diff-yaml";

  src = ../scripts/diff-yaml;

  buildInputs = [ pkgs.bash yq ];

  phases = [ "installPhase" ];

  installPhase = ''
    mkdir -p $out/bin
    cp $src $out/bin/diff-yaml
  '';
}
