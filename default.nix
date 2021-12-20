{ sources ? import ./nix/sources.nix
, haskellNix ? import sources.haskellNix { inherit system; }
, pkgs ? import haskellNix.sources.nixpkgs-2105 (haskellNix.nixpkgsArgs // { inherit system; })
, system ? builtins.currentSystem
, nix-filter ? import sources.nix-filter
}:
let
  masterPkgs = import sources.nixpkgs { inherit system; };

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
        ghcOptions = [
          "-O2"
          "-fexpose-all-unfoldings"
          "-fspecialise-aggressively"
        ];
        packages.pandoc.ghcOptions = [ "-O1" ];
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

  compaRESTBin = hsPkgs.comparest.components.exes.comparest;
  compaRESTStaticBin = (staticify "compaREST-static" hsPkgs.projectCross.musl64.hsPkgs.comparest.components.exes.comparest);


  # doesn't work
  armDarwinCompaREST = hsPkgs.projectCross.aarch64-darwin.hsPkgs.comparest.components.exes.comparest;

  compaRESTImage = pkgs.dockerTools.buildImage {
    name = "compaREST";
    contents = [ compaRESTStaticBin ];
    config = {
      Entrypoint = [ "/bin/comparest" ];
    };
  };

  macOSCompaRESTBundle = pkgs.runCommand "compaREST-macos-bundled"
    {
      buildInputs = [ masterPkgs.macdylibbundler ];
    } ''
    mkdir -p $out/lib
    cp ${compaRESTBin}/bin/comparest $out/comparest
    chmod 755 $out/comparest
    dylibbundler -b \
      -x $out/comparest \
      -d $out/lib \
      -p '@executable_path/lib'
  '';


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

  WindowsCompaRESTBin = hsPkgs.projectCross.mingwW64.hsPkgs.comparest.components.exes.comparest;
in
{
  inherit compaRESTImage
    compaRESTGithubAction
    compaRESTStaticBin
    compaRESTBin
    hsPkgs
    macOSCompaRESTBundle
    WindowsCompaRESTBin
    ;
  test = hsPkgs.comparest.components.tests.comparest-tests;
}
