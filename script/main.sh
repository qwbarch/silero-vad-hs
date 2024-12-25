#!/usr/bin/env bash
pushd ..
nix develop --command ghcid --command "cabal repl app/Main.hs" --test "main" --restart silero-vad.cabal -W
popd
