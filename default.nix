{ hpkgs ? import ./nix/hpkgs.nix {}
,
}:
hpkgs.persistent-mysql-pure
