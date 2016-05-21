#!/usr/bin/env bash

# This is useful for seeing warnings on files that haven't changed since the last compilation
stack build --force-dirty --ghc-options="-Wall -fforce-recomp"
