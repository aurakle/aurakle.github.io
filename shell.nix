let
  nixpkgsVer = "7069932e560daa85506f65ec7f63e4bbc5e0d22a";
  pkgs = import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/${nixpkgsVer}.tar.gz") { config = {}; overlays = []; };
  libs = with pkgs; [
    zlib
  ];
in with pkgs; mkShell {
  name = "starwatch";

  buildInputs = with haskellPackages; [
    cabal-install
    hakyll
    (ghcWithPackages (p: with p; [
      Chart
      Chart-cairo
      hakyll
      hakyllbars
      pandoc
    ]))
  ] ++ libs;

  LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath libs;
}
