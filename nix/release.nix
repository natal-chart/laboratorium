let
  pkgs = import ./packages.nix {};
in
  { laboratorium = pkgs.haskellPackages.laboratorium; }
