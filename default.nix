{ sources ? import ./nix/sources.nix
, haskellNix ? import sources.haskellNix { inherit system; }
, pkgs ? import haskellNix.sources.nixpkgs-2105 (haskellNix.nixpkgsArgs // { inherit system; })
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

  staticify = drv: pkgs.runCommand "compaREST-static" { } ''
    mkdir -p $out
    cp -R ${drv}/bin $out

    ${pkgs.nukeReferences}/bin/nuke-refs $out/bin/*
  '';

  compaREST = pkgs.dockerTools.buildImage {
    name = "compaREST";
    contents = [ (staticify hsPkgs.projectCross.musl64.hsPkgs.openapi-diff.components.exes.openapi-diff) ];
    config = {
      Entrypoint = [ "/bin/compaREST" ];
    };
  };

in
compaREST
