{
  inputs.haskellNix.url = "github:input-output-hk/haskell.nix";
  inputs.nixpkgs.follows = "haskellNix/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  outputs = { self, nixpkgs, flake-utils, haskellNix }:
    flake-utils.lib.eachSystem [ "x86_64-linux" ]
      (system:
        let
          packages = ps: builtins.attrValues (pkgs.haskell-nix.haskellLib.selectProjectPackages ps);
          enableIndexing = builtins.getEnv "ENABLE_INDEXING";
          overlays = [
            haskellNix.overlay
            (final: prev: { project = import ./project.nix { inherit final pkgs packages enableIndexing; }; })
          ];
          pkgs = import nixpkgs { inherit system overlays; inherit (haskellNix) config; };
          flake = pkgs.project.flake { };
        in
        flake // { legacyPackages = pkgs; });
  nixConfig = {
    extra-substituters = [
      "https://cache.iog.io"
      "https://miso-haskell.cachix.org"
    ];
    extra-trusted-public-keys = [
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
      "miso-haskell.cachix.org-1:6N2DooyFlZOHUfJtAx1Q09H0P5XXYzoxxQYiwn6W1e8="
    ];
    allow-import-from-derivation = true;
  };
}
