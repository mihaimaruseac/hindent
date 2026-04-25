let
  pkgs = import <nixpkgs> {};
in

pkgs.mkShell {
  packages = with pkgs; [
    cabal-install
    haskell.compiler.ghc912
    zlib.dev
  ];

  shellHook = ''
    export PS1="[\[\033[01;32m\]nix-shell\[\033[00m\]:\W] \[\033[01;32m\]λ\[\033[00m\] "
  '';
}
