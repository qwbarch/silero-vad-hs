#!/usr/bin/env bash
cd ..
nix develop --command ghcid --command "cabal repl test/Main.hs" --test "main" --restart hello-project.cabal -W
