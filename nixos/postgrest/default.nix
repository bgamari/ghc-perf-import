{ pkgs }: 

let
  haskellPackages = pkgs.haskell.packages.ghc865.override {
    overrides = self: super: with pkgs.haskell.lib; {
      # Disable tests to shrink the transitive closure
      mkDerivation = drv: super.mkDerivation (drv // { doCheck = false; });

      hasql = self.callPackage ./hasql.nix {};
      hasql-pool = self.callPackage ./hasql-pool.nix {};
      aeson = self.callPackage ./aeson.nix {};
      protolude = self.callPackage ./protolude.nix {};
      configurator-pg = self.callPackage ./configurator-pg.nix {};
      postgrest = self.callPackage ./postgrest.nix {};
    };
  };
in haskellPackages.postgrest
