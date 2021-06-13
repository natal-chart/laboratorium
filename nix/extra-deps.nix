{system ? builtins.currentSystem}:
# NOTE(luis)
# All packages listed here will be built from source, as they're not
# 'blessed' in our pinned nix version. The sources themselves _are_
# obtained from cache.nixos.org, but need to be rebuilt regardless.
# Exercise restraint!
let
  dontCheck   = (import ./packages.nix{inherit system;}).haskell.lib.dontCheck;
  doJailbreak = (import ./packages.nix{inherit system;}).haskell.lib.doJailbreak;
in (super: {
  # don't run tests for these
  # they currently don't build in nix due to bad quickcheck versions
  swiss-ephemeris = dontCheck (super.callPackage ./extra-pkgs/swiss-ephemeris.nix {}) ;
  timezone-detect = dontCheck (super.callPackage ./extra-pkgs/timezone-detect.nix {}) ;
  # had to jailbreak due to incompatible versions of base and optparse-applicative
  #diagrams-rasterific = doJailbreak (super.diagrams-rasterific);
  # had to use a newer version from github due to 1.4.3 as published not being
  # compatible with the nix-provided version of optparse-applicative. 
  # Fortunately, someone fixed it: https://github.com/diagrams/diagrams-lib/pull/353/files
  #diagrams-lib   = dontCheck (super.callPackage ./extra-pkgs/diagrams-lib.nix {});
  # don't check: uses a version of `tasty` not available in our nix pin:
  #fused-effects  = dontCheck super.fused-effects;
  # Had to jailbreak due to dependency on old version of bytestring
  #postgresql-simple-migration = doJailbreak super.postgresql-simple-migration;
  # had to jailbreak due to tls/http-client bad deps
  network-api-support = doJailbreak super.network-api-support;
})
