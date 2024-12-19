#!/usr/bin/env bash
cd ..
rm -rf gcroot
nix build '.#gcroot' -o gcroot
for f in gcroot/materializers/*; do echo "$(basename $f) - $($f/calculateSha)"; $f/generateMaterialized materialized/$(basename $f); done
