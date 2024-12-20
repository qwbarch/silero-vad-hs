{ final, packages, enableIndexing, pkgs, ... }:

let
  compiler-nix-name = "ghc8107";
  index-state = "2023-01-07T00:00:00Z";
  buildInputs = with builtins.getAttr compiler-nix-name (pkgs.haskell.packages); [
    hpack
    cabal-install
    ghc
    ghcid
    haskell-language-server
    markdown-unlit
  ];
in
final.haskell-nix.cabalProject' {
  inherit compiler-nix-name index-state;
  src = ./.;
  shell = {
    inherit packages;
    additional =
      if enableIndexing != "" && builtins.fromJSON enableIndexing
      then packages else _: [ ];
    withHoogle = true;
    exactDeps = false;
    inputsFrom = [{ inherit buildInputs; }];
    shellHook = ''
      export TASTY_COLOR=always
    '';
  };
}
