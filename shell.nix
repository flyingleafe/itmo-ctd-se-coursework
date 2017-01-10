with import <nixpkgs> { };
let
  hsPkgs = haskell.packages.ghc801;
in
  haskell.lib.buildStackProject {
     name = "coursework";
     ghc = hsPkgs.ghc;
     buildInputs =
       [ zlib glib git cabal-install autoreconfHook stack ];
     LANG = "en_US.UTF-8";
  }
