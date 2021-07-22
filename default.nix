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
    # contents = [];
    # contents = [
    #   hsPkgs.openapi-diff.components.exes.openapi-diff
    # ];

    # runAsRoot = ''
    #   mkdir /app
    #   cp -av ${compaREST-static} /app/compaREST
    # '';


    config = {
      Entrypoint = [ "${compaREST-static}/compaREST" ];
    };
  };

in compaREST
