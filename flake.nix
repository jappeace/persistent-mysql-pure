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
          # TODO Figureo out how to run the database tests in CI
          persistent-mysql-haskell = pkgs.haskell.lib.dontCheck (hnew.callCabal2nix "persistent-mysql-haskell" ./. { });
          # temporary workaround as nixpkgs hasn't absorbed this yet
          mysql-haskell = (hold.callHackageDirect {
              pkg = "mysql-haskell";
              ver = "1.1.2";
              sha256 = "sha256-cTIXdzScFPXUyrFENv1wm+lM5/UEcrBczBtEaMFn5qw=";
          } {});
          crypton-x509 = pkgs.haskell.lib.markUnbroken (pkgs.haskell.lib.dontCheck hold.crypton-x509);
          # need to override untill nixpkgs merges in the new tls
          # do to the cryptonite/crypton clusterfuck
          tls = (hold.callHackageDirect {
              pkg = "tls";
              ver = "1.7.1";
              sha256 = "sha256-l8+Kgx7A8zg2tl64mC7t/S0gJNCS10fQ/2I65bTMpjY=";
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
