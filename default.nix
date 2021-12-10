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
        ./comparest.cabal
      ];
    };

    modules = [
      {
        dontStrip = false;
        dontPatchELF = false;
        enableDeadCodeElimination = true;
        ghcOptions = [ "-O2" ];
        packages.comparest.src = nix-filter {
          root = ./.;
          name = "compaREST-src";
          include = with nix-filter; [
            (./comparest.cabal)
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

  staticify = name: drv: pkgs.runCommand name { } ''
    mkdir -p $out/bin
    cp ${drv + "/bin"}/* $out/bin

    ${pkgs.nukeReferences}/bin/nuke-refs $out/bin/*
  '';

  compaRESTBin = hsPkgs.projectCross.musl64.hsPkgs.comparest.components.exes.comparest;

  compaREST = pkgs.dockerTools.buildImage {
    name = "compaREST";
    contents = [ (staticify "compaREST-static" compaRESTBin) ];
    config = {
      Entrypoint = [ "/bin/comparest" ];
    };
  };

  compaRESTGithubAction =
    let
      action = staticify "compaREST-github-action-static" hsPkgs.projectCross.musl64.hsPkgs.comparest.components.exes.comparest-github-action;
      wrapped = pkgs.runCommand "wrapped-compaREST-github-action" { buildInputs = [ pkgs.makeWrapper ]; } ''
        makeWrapper ${action}/bin/comparest-github-action $out/bin/pre --add-flags "pre"
        makeWrapper ${action}/bin/comparest-github-action $out/bin/run --add-flags "run"
      '';
    in
    pkgs.dockerTools.buildImage {
      name = "typeable/comparest-github-action";
      tag = "latest";
      contents = [ wrapped pkgs.cacert ];
    };

in
{ inherit compaREST compaRESTGithubAction hsPkgs haskellNix compaRESTBin; }
