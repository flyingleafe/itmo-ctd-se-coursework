with import <nixpkgs> { };
let
  hsPkgs = haskell.packages.ghc801;
in
  haskell.lib.buildStackProject {
     name = "lab6";
     ghc = hsPkgs.ghc;
     buildInputs =
       [ blas liblapack gfortran48 ];
  }
