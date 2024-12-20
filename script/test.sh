#!/usr/bin/env bash
pushd ..
nix develop --command ghcid --command "cabal repl test/Main.hs" --test "main" --restart hello-project.cabal -W
popd
