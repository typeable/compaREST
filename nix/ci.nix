{ sources ? import ./sources.nix
, pkgs ? import sources.nixpkgs { }
}:

let
  yq = import ./yq.nix { inherit pkgs; };
  diff-yaml = import ./diff-yaml.nix { inherit pkgs; };
in
pkgs.mkShell {
  packages = [ pkgs.haskellPackages.fourmolu diff-yaml yq ];
}
