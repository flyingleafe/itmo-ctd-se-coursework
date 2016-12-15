#!/bin/sh

EXE=coursework-exe
PATH_TO_EXE=.stack-work/install/x86_64-linux-nix/lts-7.13/8.0.1/bin/$EXE

stack --nix build
patchelf --remove-needed libgfortran.so.3 $PATH_TO_EXE
