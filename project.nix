{ final, packages, enableIndexing, pkgs, ... }:

let
  compiler-nix-name = "ghc928";
  tools = {
    cabal = { };
    haskell-language-server = { };
    hpack = { };
    ghcid = { };
  };
  buildInputs = with pkgs; [
    stdenv.cc.cc
  ];
in
final.haskell-nix.cabalProject' {
  inherit compiler-nix-name;
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
      export LD_LIBRARY_PATH="lib:${pkgs.stdenv.cc.cc.lib}/lib"
    '';
  };
}
