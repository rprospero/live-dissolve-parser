{ pkgs ? import <nixpkgs> { } }:
let
  easy-ps = import
    (pkgs.fetchFromGitHub {
      owner = "justinwoo";
      repo = "easy-purescript-nix";
      rev = "5716cd791c999b3246b4fe173276b42c50afdd8d";
      sha256 = "1r9lx4xhr42znmwb2x2pzah920klbjbjcivp2f0pnka7djvd2adq";
    }) {
    inherit pkgs;
  };
in
pkgs.mkShell {
  buildInputs = [
    pkgs.json2yaml
    pkgs.yj
    pkgs.xmlstarlet
    pkgs.nodejs
    easy-ps.purs-0_14_4
    easy-ps.purs-tidy
    easy-ps.purescript-language-server
    easy-ps.psc-package
    easy-ps.purty
    easy-ps.spago
  ];
}
