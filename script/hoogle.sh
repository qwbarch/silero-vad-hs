#!/usr/bin/env bash
pushd ..
ENABLE_INDEXING=true && nix develop --impure --command hoogle server --local -p 3001
popd
