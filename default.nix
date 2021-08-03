{ sources ? import ./nix/sources.nix
, haskellNix ? import sources.haskellNix { inherit system; }
, pkgs ? import haskellNix.sources.nixpkgs-2105 (haskellNix.nixpkgsArgs // { inherit system; })
, system ? builtins.currentSystem
, nix-filter ? import sources.nix-filter
}:
let
  hsPkgs = pkgs.haskell-nix.stackProject {
    src = nix-filter {
      root = ./.;
      name = "compaREST";
      include = [
        ./stack.yaml
        ./stack.yaml.lock
        ./openapi-diff.cabal
      ];
    };

    modules = [
      {
        dontStrip = false;
        dontPatchELF = false;
        enableDeadCodeElimination = true;
        packages.openapi-diff.src = nix-filter {
          root = ./.;
          name = "compaREST-src";
          include = with nix-filter; [
            (./openapi-diff.cabal)
            (inDirectory ./test)
            (inDirectory ./src)
            (inDirectory ./app)
            (inDirectory ./github-action)
            ./awsm-css/dist/awsm.min.css
            ./LICENSE
          ];
        };
      }
    ];
  };

  staticify = drv: pkgs.runCommand "compaREST-static" { } ''
    mkdir -p $out/bin
    cp ${drv + "/bin"}/* $out/bin

    ${pkgs.nukeReferences}/bin/nuke-refs $out/bin/*
  '';

  compaREST = pkgs.dockerTools.buildImage {
    name = "compaREST";
    contents = [ (staticify hsPkgs.projectCross.musl64.hsPkgs.openapi-diff.components.exes.openapi-diff) ];
    config = {
      Entrypoint = [ "/bin/openapi-diff" ];
    };
  };

  compaRESTGithubAction = pkgs.dockerTools.buildImage {
    name = "compaREST-github-action";
    contents = [ (staticify hsPkgs.projectCross.musl64.hsPkgs.openapi-diff.components.exes.comparest-github-action) ];
    config = {
      Entrypoint = [ "/bin/comparest-github-action" ];
    };
  };

in
{ inherit compaREST compaRESTGithubAction; }
