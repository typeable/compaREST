{ sources ? import ./nix/sources.nix
, haskellNix ? import sources.haskellNix { }
, gomod2nix ? sources.gomod2nix
, pkgs ? import haskellNix.sources.nixpkgs-2105 (haskellNix.nixpkgsArgs // {
    overlays = haskellNix.nixpkgsArgs.overlays ++ [
      (self: super: {
        buildGoApplication = super.callPackage (gomod2nix + "/builder") { };
      })
    ];
  })
, npmNix ? import (sources.npmNix + "/npmPackages") { inherit pkgs; }
, mavenix ? import (sources.mavenix) { inherit pkgs; }
, bumpToken ? null
, bumpDocumentation ? null
}:
let
  typeable-comparest = (pkgs.haskell-nix.stackProject {
    src = pkgs.haskell-nix.haskellLib.cleanGit {
      name = "comparest";
      src = ./..;
    };
  }).comparest.components.exes.comparest;
  typeable-comparest-exe = typeable-comparest + "/bin/comparest";
  typeable-comparest-differ = pkgs.writeScript "typeable-comparest-differ" ''
    #!${pkgs.stdenv.shell}
    set +e
    ${typeable-comparest-exe} -c $1 -s $2 -o $3/typeable.md
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

  oasdiff = pkgs.pkgs.buildGoApplication {
    pname = "oasdiff";
    version = "0.1";
    src = sources.oasdiff;
    modules = ./oasdiff/gomod2nix.toml;
  };
  oasdiff-differ = pkgs.writeScript "oasdiff-differ" ''
    #!${pkgs.stdenv.shell}
    set +e
    ${oasdiff}/bin/oasdiff -base $1 -revision $2 -format text >$3/oasdiff.txt 2>$3/oasdiff.error.txt
    if [ ! -s $3/oasdiff.error.txt ]; then
      rm $3/oasdiff.error.txt
    fi
    exit 0
  '';

  bump-differ = { bumpToken, bumpDocumentation }: pkgs.writeScript "bump-differ" ''
    #!${pkgs.stdenv.shell}

    ${pkgs.httpie}/bin/https --ignore-stdin POST bump.sh/api/v1/versions definition=@$1 documentation=${bumpDocumentation} -a ${bumpToken}: >/dev/null
    id=$(${pkgs.httpie}/bin/https --ignore-stdin POST bump.sh/api/v1/versions definition=@$2 documentation=${bumpDocumentation} -a ${bumpToken}: | ${pkgs.jq}/bin/jq -r .id)

    if [ -z "$id" ]
    then
      echo "Not changed" > $3/bump.txt
    else
      state=""

      while [ "$state" != "deployed" ]
      do
        state=$(${pkgs.httpie}/bin/https --ignore-stdin GET bump.sh/api/v1/versions/$id -a ${bumpToken}: | ${pkgs.jq}/bin/jq -r .state)
      done

      ${pkgs.httpie}/bin/https --ignore-stdin GET bump.sh/api/v1/versions/$id -a ${bumpToken}: | ${pkgs.jq}/bin/jq -r .diff_summary > $3/bump.txt
    fi
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
          a = path + "/a.yaml";
          b = path + "/b.yaml";
        })
        (getInputs root);
      differs = [
        typeable-comparest-differ
        atlassian-openapi-diff-differ
        openapitools-openapi-diff-differ
        oasdiff-differ
      ] ++ (
        if bumpToken == null || bumpDocumentation == null
        then builtins.trace "Warning: bumpToken or bumpDocumentation not provided. Skipping bump.sh benchmarks." [ ]
        else [ (bump-differ { inherit bumpToken bumpDocumentation; }) ]
      );
    }
    ''
      echo "Running compatibility checks:"
      for dir in $dirs
      do
        a=$(echo $dir | ${pkgs.jq}/bin/jq -r .a)
        b=$(echo $dir | ${pkgs.jq}/bin/jq -r .b)
        relativePath=$(echo $dir | ${pkgs.jq}/bin/jq -r .relativePath)
        echo "''\t$relativePath"
        output=$out$relativePath
        mkdir -p $output
        for differ in $differs
        do
          $differ $a $b $output
        done
        cp $a $output/a.yaml
        cp $b $output/b.yaml
      done
    '';

  benchmarkReportTemplate = root:
    let
      tools = [
        "Typeable"
        "Atlassian"
        "OpenAPI Tools"
        "oasdiff"
        "Bump.sh"
      ];
      foo = with pkgs.lib.strings;
        concatMapStrings (x: "| " + x) ([ "" ] ++ tools) + "| \n" + concatMapStrings (_: "|---") ([ "" ] ++ tools) + "| \n" +
        concatMapStrings
          (path:
            let
              relativePath = pkgs.lib.removePrefix (builtins.toString root + "/") (builtins.toString path);

            in
            "[${relativePath}](./${relativePath})${concatMapStrings (_: " | ✅❌") tools} \n")
          (getInputs root);
    in
    foo;

in
runBenchmark ../test/golden/common
