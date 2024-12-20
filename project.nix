{ final, packages, enableIndexing, pkgs, ... }:

let
  compiler-nix-name = "ghc928";
  index-state = null;
  buildInputs = with builtins.getAttr compiler-nix-name (pkgs.haskell.packages); [
    hpack
    ghc
  ];
  tools = {
    cabal = { };
    haskell-language-server = { };
    ghcid = { };
  };
in
final.haskell-nix.cabalProject' {
  inherit compiler-nix-name index-state;
  src = ./.;
  shell = {
    inherit packages tools;
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
