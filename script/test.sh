#!/usr/bin/env bash

pushd ..
nix develop --command bash -c '
  export LD_LIBRARY_PATH=lib:$(nix path-info .#stdenv.cc.cc.lib)/lib
  ghcid --command "cabal repl test/Main.hs" --test "main" --restart hello-project.cabal -W
'
popd
