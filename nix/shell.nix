{ pkgs ? import <nixpkgs> {} }:

let
  patched-yq = import ./yq.nix { inherit pkgs; };
in
pkgs.mkShell {
  buildInputs = with pkgs; [ pkg-config patched-yq zlib ];
}
