{
  description = "Voice activity detection powered by SileroVAD";

  inputs = {
    haskellNix.url = "github:input-output-hk/haskell.nix";
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs =
    { self
    , nixpkgs
    , haskellNix
    , flake-utils
    ,
    }:
    flake-utils.lib.eachSystem [ "x86_64-linux" "x86_64-darwin" ] (system:
    let
      projectName = "silero-vad-hs";
      compiler-nix-name = "ghc902";
      index-state = null;

      package = {
        defaultPackage = flake.packages."${projectName}:exe:${projectName}";
      };

      mkProject = haskell-nix:
        haskell-nix.cabalProject' {
          src = ./.;
          inherit index-state compiler-nix-name;

          # plan-sha256 = "";
          # materialized = ./materialized + "/${projectName}";
        };

      overlays = [
        haskellNix.overlay
        (self: super: { ${projectName} = mkProject self.haskell-nix; })
      ];

      pkgs = import nixpkgs {
        inherit system overlays;
        inherit (haskellNix) config;
      };
      project = pkgs.${projectName};
      flake = pkgs.${projectName}.flake { crossPlatforms = ps: with ps; [ ]; };

      tools = {
        cabal = {
          inherit index-state;
          # plan-sha256 = "";
          # materialized = ./materialized/cabal;
        };

        haskell-language-server = {
          inherit index-state;
          # plan-sha256 = "";
          # materialized = ./materialized/haskell-language-server;
        };

        hoogle = {
          inherit index-state;
          # plan-sha256 = "";
          # materialized = ./materialized/hoogle;
        };

        ghcid = {
          inherit index-state;
          # plan-sha256 = "";
          # materialized = ./materialized/ghcid;
        };
      };

      devShell = project.shellFor {
        inherit tools;
        packages = ps: [ ps.${projectName} ];
        inputsFrom = [{ buildInputs = with pkgs; [ alejandra ormolu hpack ]; }];
        exactDeps = true;
        shellHook = ''
          export TASTY_COLOR=always
        '';
      };

      gcroot =
        (import ./nix/gcroot.nix)
          pkgs
          overlays
          devShell
          flake
          projectName
          project
          self
          tools;
    in
    flake // gcroot // package);
}
