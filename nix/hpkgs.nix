{
  pkgs ? import ./pkgs.nix { },
}:
pkgs.haskellPackages.override {
  overrides = hnew: hold: {
    persistent-mysql-pure =
      pkgs.haskell.lib.dontCheck (hnew.callCabal2nix "persistent-mysql-pure" ../. { });
    mysql-haskell = pkgs.haskell.lib.dontCheck (hnew.callHackageDirect {
      pkg = "mysql-haskell";
      ver = "1.2.0";
      sha256 = "sha256-NFJTBP/prXGl2XLAj3SdIrE1gIJREO6lqupDuP7xLf4=";
    } {});
    ram = hnew.callHackageDirect {
      pkg = "ram";
      ver = "0.21.1";
      sha256 = "sha256-J+gP+rZft1xkxzxmvXcktnDIymRkjg5u5wmhEge3+GQ=";
    } {};
  };
}
