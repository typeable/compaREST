{ sources ? import ./sources.nix
, pkgs ? import sources.nixpkgs {  }
}:

let
  patched-yq = import ./yq.nix { inherit pkgs; };
  diffYaml = import ./diff-yaml.nix { inherit pkgs; };
in
pkgs.mkShell {
  buildInputs = with pkgs; [ pkg-config diffYaml patched-yq zlib ];
}
