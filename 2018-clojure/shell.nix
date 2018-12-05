with import <nixpkgs> {};

stdenv.mkDerivation {
  name = "advent-of-code-clojure";

  buildInputs = [
    pkgs.clojure
    pkgs.leiningen
  ];

 shellHook = ''
  emacs --daemon && emacsclient -c -a emacs &> /dev/null &
  '';
}
