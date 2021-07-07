{ sources ? import ./nix/sources.nix
, haskellNix ? import sources.haskellNix { }
, pkgs ? import haskellNix.sources.nixpkgs-2105 haskellNix.nixpkgsArgs
, npmNix ? import (sources.npmNix + "/npmPackages") { inherit pkgs; }
, mavenix ? import (sources.mavenix) { inherit pkgs; }
}:
let
  typeable-openapi-diff = (pkgs.haskell-nix.stackProject {
    src = pkgs.haskell-nix.haskellLib.cleanGit {
      name = "openapi-diff";
      src = ./..;
    };
  }).openapi-diff.components.exes.openapi-diff;
  typeable-openapi-diff-exe = typeable-openapi-diff + "/bin/openapi-diff";
  typeable-openapi-diff-differ = pkgs.writeScript "typeable-openapi-diff-differ" ''
    #!${pkgs.stdenv.shell}
    set +e
    ${typeable-openapi-diff-exe} -s $1 -c $2 -o $3/typeable.md
    exit 0
  '';

  atlassian-openapi-diff = pkgs.mkYarnPackage {
    name = "openapi-diff";
    src = sources.atlassian-openapi-diff;
    yarnLock = ./atlassian/yarn.lock;
    patches = [ ./atlassian/addDist.patch ];
    postBuild = "yarn run gulp compileDist";
  };
  atlassian-openapi-diff-exe = atlassian-openapi-diff + "/bin/openapi-diff";
  atlassian-openapi-diff-differ = pkgs.writeScript "atlassian-openapi-diff-differ" ''
    #!${pkgs.stdenv.shell}
    set +e
    ${atlassian-openapi-diff-exe} $1 $2 >$3/atlassian.json  2>$3/atlassian.error.txt
    if [ ! -s $3/atlassian.error.txt ]; then
      rm $3/atlassian.error.txt
    fi
    exit 0
  '';

  openapitools-openapi-diff = mavenix.buildMaven {
    maven = pkgs.maven;
    src = sources.openapitools-openapi-diff;
    doCheck = false;
    infoFile = ./openapitools/mavenix.lock;
    MAVEN_OPTS = "-Dgithook.plugin.skip=true";
  };
  openapitools-openapi-diff-exe =
    pkgs.runCommand "openapitools-openapi-diff"
      { buildInputs = [ pkgs.makeWrapper ]; }
      ''makeWrapper ${pkgs.jdk}/bin/java $out --add-flags "-jar ${openapitools-openapi-diff}/share/java/openapi-diff-cli-2.0.0-SNAPSHOT-all.jar"'';
  openapitools-openapi-diff-differ = pkgs.writeScript "openapitools-openapi-diff-differ" ''
    #!${pkgs.stdenv.shell}
    set +e
    ${openapitools-openapi-diff-exe} $1 $2 --off --markdown $3/openapitools.md 2>$3/openapitools.error.txt
    if [ ! -s $3/openapitools.error.txt ]; then
      rm $3/openapitools.error.txt
    fi
    exit 0
  '';

  getInputs = with pkgs;
    dir: lib.flatten (lib.mapAttrsToList
      (name: type:
        if type == "directory" then
          let
            path = dir + "/${name}";
            contents = builtins.readDir path;
          in
          if contents."a.yaml" or null == "regular" && contents."b.yaml" or null == "regular"
          then path
          else getInputs path
        else [ ]
      )
      (builtins.readDir dir)
    );

  runBenchmark = root: pkgs.runCommand "openapi-diff-benchmarks"
    {
      dirs = map
        (path: builtins.toJSON {
          relativePath = pkgs.lib.removePrefix (builtins.toString root) (builtins.toString path);
          inherit path;
        })
        (getInputs root);
      differs = [
        typeable-openapi-diff-differ
        atlassian-openapi-diff-differ
        openapitools-openapi-diff-differ
      ];
    }
    ''
      echo "Running compatibility checks:"
      for dir in $dirs
      do
        path=$(echo $dir | ${pkgs.jq}/bin/jq -r .path)
        relativePath=$(echo $dir | ${pkgs.jq}/bin/jq -r .relativePath)
        echo "''\t$relativePath"
        output=$out$relativePath
        mkdir -p $output
        for differ in $differs
        do
          $differ $path/a.yaml $path/b.yaml $output
        done
      done
    '';


in
runBenchmark ../test/golden/common
