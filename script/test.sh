#!/usr/bin/env bash

pushd ..
nix develop --command bash -c '
  export LD_LIBRARY_PATH=$(nix eval --raw nixpkgs.glibc.outPath)/lib:${LD_LIBRARY_PATH}
  ghcid --command "cabal repl test/Main.hs" --test "main" --restart hello-project.cabal -W
'
popd

