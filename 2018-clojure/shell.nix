with import <nixpkgs> {};

stdenv.mkDerivation {
  name = "advent-of-code-clojure";

  buildInputs = [
    pkgs.clojure
    pkgs.leiningen
  ];
}
