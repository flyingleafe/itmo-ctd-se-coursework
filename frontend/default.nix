with import <nixpkgs> {}; {
  mlEnv = stdenv.mkDerivation {
    name = "courseworkFrontend";
    buildInputs = [
      nodejs
      electron
    ];
  };
}
