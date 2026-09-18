{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  packages = [
    pkgs.duckdb
    pkgs.jdk11
    pkgs.nodejs
    pkgs.jq
    pkgs.kahip
    pkgs.cabal-install
    (pkgs.haskell.packages.ghc914.ghcWithPackages (p: with p; [
      aeson
      hashable
      vector
      zip-archive
    ]))
    pkgs.pkg-config
  ];
  shellHook = ''
    export LD_LIBRARY_PATH="${pkgs.lib.makeLibraryPath [ pkgs.kahip ]}:$LD_LIBRARY_PATH"
  '';
}
