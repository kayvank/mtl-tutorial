{ compiler ? "ghc8107" }:

let
  sources = import ./nix/sources.nix;
  pkgs = import sources.nixpkgs {};

  gitignore = pkgs.nix-gitignore.gitignoreSourcePure [ ./.gitignore ];

  myHaskellPackages = pkgs.haskell.packages.${compiler}.override {
    overrides = hself: hsuper: {
      "monad-transformers" =
        hself.callCabal2nix
          "monad-transformers"
          (gitignore ./.)
          {};
    };
  };

  shell = myHaskellPackages.shellFor {
    packages = p: [
      p."monad-transformers"
    ];
    buildInputs = [
      myHaskellPackages.haskell-language-server
      pkgs.haskellPackages.cabal-install
      pkgs.haskellPackages.ghcid
      pkgs.haskellPackages.ormolu
      pkgs.haskellPackages.hlint
      pkgs.haskellPackages.hpack
      pkgs.niv
      pkgs.nixpkgs-fmt
    ];
    withHoogle = true;

  ## this is a hack, untill I give up on emacs-doom or my nixos machine
  ## doom on my nix-os machine caches, but not load nix-shell env, which makes the haskell lsp useless

    shellHook = ''
        [ -d ~/.emacs.d/bin/doom ] && ~/.emacs.d/bin/doom env
      '';

  };

  exe = pkgs.haskell.lib.justStaticExecutables (myHaskellPackages."monad-transformers");

  docker = pkgs.dockerTools.buildImage {
    name = "monad-transformers";
    config.Cmd = [ "${exe}/bin/monad-transformers" ];
  };
in
{
  inherit shell;
  inherit exe;
  inherit docker;
  inherit myHaskellPackages;
  "monad-transformers" = myHaskellPackages."monad-transformers";
}
