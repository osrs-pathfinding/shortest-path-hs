{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  packages = [
    pkgs.metis
    pkgs.kahip
    pkgs.cabal-install
    (pkgs.haskellPackages.ghcWithPackages (p: with p; [
      aeson
      hashable
      pqueue
      vector
      zip-archive
    ]))
    pkgs.pkg-config
  ];
  shellHook = ''
    export LD_LIBRARY_PATH="${pkgs.lib.makeLibraryPath [ pkgs.metis pkgs.kahip ]}:$LD_LIBRARY_PATH"
  '';
}
