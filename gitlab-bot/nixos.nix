{ accessToken }: { pkgs, config, ... }:

let
  service = pkgs.python3Packages.callPackage ./package.nix {};

  ghc-perf-import-service = {
    systemd.services.ghc-perf-import-service = {
      description = "GitLab perf import service";
      script = ''
        ${service}/bin/ghc-perf-import-service \
          --gitlab-root=https://gitlab.${config.ghc.rootDomain} \
          --access-token="${accessToken}" \
          --port=8090
      '';
      wantedBy = [ "multi-user.target" ];
    };
  };

in {
  imports = [ ghc-perf-import-service ];
}

