with import <nixpkgs> { };
let
  hsPkgs = haskell.packages.ghc801;
in
  haskell.lib.buildStackProject {
     name = "coursework";
     ghc = hsPkgs.ghc;
     buildInputs =
       [ zlib glib blas liblapack gfortran48 ];
     LANG = "en_US.UTF-8";
  }
