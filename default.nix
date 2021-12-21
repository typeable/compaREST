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
        ./compaREST.cabal
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
        packages.compaREST.src = nix-filter {
          root = ./.;
          name = "compaREST-src";
          include = with nix-filter; [
            (./compaREST.cabal)
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

  compaRESTBin = hsPkgs.compaREST.components.exes.compaREST;
  compaRESTStaticBin = (staticify "compaREST-static" hsPkgs.projectCross.musl64.hsPkgs.compaREST.components.exes.compaREST);


  # doesn't work
  armDarwinCompaREST = hsPkgs.projectCross.aarch64-darwin.hsPkgs.compaREST.components.exes.compaREST;

  compaRESTImage = pkgs.dockerTools.buildImage {
    name = "compaREST";
    contents = [ compaRESTStaticBin ];
    config = {
      Entrypoint = [ "/bin/compaREST" ];
    };
  };

  macOSCompaRESTBundle = pkgs.runCommand "compaREST-macOS-bundled"
    {
      buildInputs = [ masterPkgs.macdylibbundler ];
    } ''
    mkdir -p $out/lib
    cp ${compaRESTBin}/bin/compaREST $out/compaREST
    chmod 755 $out/compaREST
    dylibbundler -b \
      -x $out/compaREST \
      -d $out/lib \
      -p '@executable_path/lib'
  '';


  compaRESTGithubAction =
    let
      action = staticify "compaREST-GitHub-Action-static" hsPkgs.projectCross.musl64.hsPkgs.compaREST.components.exes.compaREST-GitHub-Action;
      wrapped = pkgs.runCommand "wrapped-compaREST-GitHub-Action" { buildInputs = [ pkgs.makeWrapper ]; } ''
        makeWrapper ${action}/bin/compaREST-GitHub-Action $out/bin/pre --add-flags "pre"
        makeWrapper ${action}/bin/compaREST-GitHub-Action $out/bin/run --add-flags "run"
      '';
    in
    pkgs.dockerTools.buildImage {
      name = "typeable/compaREST-GitHub-Action";
      tag = "latest";
      contents = [ wrapped pkgs.cacert ];
    };

  WindowsCompaRESTBin = hsPkgs.projectCross.mingwW64.hsPkgs.compaREST.components.exes.compaREST;
in
builtins.trace hsPkgs.compaREST.components.exes
{
  inherit compaRESTImage
    compaRESTGithubAction
    compaRESTStaticBin
    compaRESTBin
    hsPkgs
    macOSCompaRESTBundle
    WindowsCompaRESTBin
    ;
  test = hsPkgs.compaREST.components.tests.compaREST-tests;
}
