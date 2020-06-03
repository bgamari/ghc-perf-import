{ pkgs, lib, ... }:

let
  db_name = "ghc_perf";
  postgrestPort = 8889;
  accessToken = "TOKEN";

  ghc-perf-import = pkgs.haskellPackages.callCabal2nix "perf-import" ../import {};

  ghc-perf-web = pkgs.callPackage (import ../web.nix) {};

  gitlab-bot = pkgs.callPackage ../gitlab-bot {};

  postgrest = import ./postgrest { inherit pkgs; };
  postgrestConfig = builtins.toFile "postgrest.conf" ''
    db-uri = "postgres://ghc_perf_web@/ghc_perf"
    db-schema = "public"
    db-anon-role = "ghc_perf_web"
    db-pool = 10
  
    server-host = "*4"
    server-port = ${toString postgrestPort}
  '';

  pre-import-script = ''
    if ! ${pkgs.sudo}/bin/sudo -upostgres ${pkgs.postgresql}/bin/psql -lqtA | grep -q "^${db_name}|"; then
      echo "Initializing schema..."
      ${pkgs.sudo}/bin/sudo -upostgres ${pkgs.postgresql}/bin/psql \
        -c "CREATE ROLE ghc_perf WITH LOGIN;" \
        -c "CREATE DATABASE ${db_name} WITH OWNER ghc_perf;"
      ${pkgs.sudo}/bin/sudo -ughc_perf ${pkgs.postgresql}/bin/psql -U ghc_perf < ${../import/schema.sql}
      ${pkgs.sudo}/bin/sudo -upostgres ${pkgs.postgresql}/bin/psql \
        -c "GRANT SELECT ON ALL TABLES IN SCHEMA public TO ghc_perf_web;"
      echo "done."
    fi
  '';

  import-script = ''
    cd /var/cache/ghc-perf
    if [ ! -d ghc ]; then
      git clone https://gitlab.haskell.org/ghc/ghc
    fi
    cd ghc

    git pull
    git fetch https://gitlab.haskell.org/ghc/ghc-performance-notes.git refs/notes/perf:refs/notes/ci/perf

    perf-import-git -c postgresql:///${db_name} -d ghc
    perf-import-notes -c postgresql:///${db_name} -d ghc -r refs/notes/ci/perf
  '';

in {
  users.users.ghc_perf = {
    description = "User for ghc-perf import script";
  };

  systemd.services.ghc-perf-gitlab-import-bot = {
    description = "ghc-perf metric import bot";
    preStart = pre-import-script;
    script = ''
      ghc-perf-import-service \
        --gitlab-root=https://gitlab.haskell.org/ \
        --access-token=${accessToken} \
        --conn-string=postgresql:///ghc_perf \
        --port=7088
    '';
    path = [ pkgs.git gitlab-bot ghc-perf-import ];
    serviceConfig = {
      User = "ghc_perf";
      PermissionsStartOnly = true;
      CacheDirectory = "ghc-perf";
    };
  };

  systemd.services.ghc-note-perf-import = {
    description = "Update ghc-perf metrics from perf notes";
    preStart = pre-import-script;
    script = import-script;
    path = [ pkgs.git ghc-perf-import ];
    serviceConfig = {
      User = "ghc_perf";
      PermissionsStartOnly = true;
      CacheDirectory = "ghc-perf";
    };
  };

  systemd.timers.ghc-perf-import = {
    description = "Periodically update ghc-perf metrics";
    wants = [ "network.target" ];
    timerConfig = {
      OnCalendar = "hourly";
      Unit = "ghc-perf-import.service";
    };
  };

  users.users.ghc_perf_web = {
    isSystemUser = true;
    shell = "/bin/false";
  };

  systemd.services.postgrest-ghc-perf = {
    description = "Postgrest instance for ghc-perf";
    script = ''
      ${postgrest}/bin/postgrest ${postgrestConfig}
    '';
    serviceConfig.User = "ghc_perf_web";
  };

}

