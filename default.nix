{ sources ? import ./nix/sources.nix
, haskellNix ? import sources.haskellNix { inherit system; }
, pkgs ? import haskellNix.sources.nixpkgs-2105 (haskellNix.nixpkgsArgs // {inherit system;})
, system ? builtins.currentSystem
}:
let
  hsPkgs = pkgs.haskell-nix.stackProject {
    src = ./.;

    modules = [
      {
        dontStrip = false;
        dontPatchELF = false;
        enableDeadCodeElimination = true;
      }
    ];
  };

  compaREST-static = pkgs.runCommand "compaREST-static" { } ''
    mkdir $out
    cp ${hsPkgs.projectCross.musl64.hsPkgs.openapi-diff.components.exes.openapi-diff + "/bin/openapi-diff"} $out/compaREST

    ${pkgs.nukeReferences}/bin/nuke-refs $out/compaREST
  '';
  compaREST = pkgs.dockerTools.buildImage {
    name = "compaREST";
    contents = [compaREST-static];
    config = {
      Entrypoint = [ "/compaREST" ];
    };
  };

in compaREST
