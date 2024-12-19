#!/usr/bin/env bash
cd ..
nix develop --command ghcid --command "cabal repl app/Main.hs" --test "main" --restart SileroVAD.cabal -W
