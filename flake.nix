# I used chatgpt to generate this template and then just
# modified to how I normally use these things.
{
  description = "My Haskell project";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, flake-compat }:
    let
      pkgs = nixpkgs.legacyPackages.x86_64-linux;
      hpkgs = pkgs.haskellPackages.override {
        overrides = hnew: hold: {
          persistent-mysql-haskell = hnew.callCabal2nix "persistent-mysql-haskell" ./. { };
          # temporary workaround as nixpkgs hasn't absorbed this yet
          mysql-pure = (hold.callHackageDirect {
              pkg = "mysql-pure";
              ver = "1.0.1";
              sha256 = "sha256-Tf/TL0tg1gL3H4kgeFjVdzSielCwZWwRjAu2rrIhWhA=";
          } {});
        };
      };
    in
    {
      defaultPackage.x86_64-linux =  hpkgs.persistent-mysql-haskell;
      inherit pkgs;
      devShell.x86_64-linux = hpkgs.shellFor {
        packages = ps : [ ps."persistent-mysql-haskell" ];
        withHoogle = true;

        buildInputs = [
          hpkgs.haskell-language-server
          pkgs.ghcid
          pkgs.cabal-install
        ];
      };
    };
}