# NixOS module for weapon-server
{
  config,
  lib,
  ...
}:

let
  cfg = config.services.weapon-server;
in
{
  options.services.weapon-server = {
    enable = lib.mkEnableOption "weapon-server";

    package = lib.mkOption {
      type = lib.types.package;
      description = "The weapon-server package to use";
    };

    port = lib.mkOption {
      type = lib.types.port;
      default = 4096;
      description = "Port to listen on";
    };

    workDir = lib.mkOption {
      type = lib.types.str;
      default = "/var/lib/weapon-server";
      description = "Working directory for weapon-server";
    };

    user = lib.mkOption {
      type = lib.types.str;
      default = "weapon";
      description = "User to run weapon-server as";
    };

    group = lib.mkOption {
      type = lib.types.str;
      default = "weapon";
      description = "Group to run weapon-server as";
    };

    openFirewall = lib.mkOption {
      type = lib.types.bool;
      default = false;
      description = "Open firewall for weapon-server port";
    };

    backend = lib.mkOption {
      type = lib.types.enum [
        "iouring"
        "warp"
      ];
      default = "iouring";
      description = "HTTP backend to use (iouring requires kernel 5.1+)";
    };

    cores = lib.mkOption {
      type = lib.types.nullOr lib.types.int;
      default = null;
      description = "Number of cores to use (null = all available)";
    };

    quiet = lib.mkOption {
      type = lib.types.bool;
      default = true;
      description = "Suppress stdout logging (log to file only)";
    };
  };

  config = lib.mkIf cfg.enable {
    users.users.${cfg.user} = {
      isSystemUser = true;
      inherit (cfg) group;
      home = cfg.workDir;
      createHome = true;
    };

    users.groups.${cfg.group} = { };

    systemd.services.weapon-server = {
      description = "Weapon AI Server (io_uring)";
      wantedBy = [ "multi-user.target" ];
      after = [ "network.target" ];

      environment = {
        HOME = cfg.workDir;
      };

      serviceConfig = {
        Type = "simple";
        User = cfg.user;
        Group = cfg.group;
        WorkingDirectory = cfg.workDir;
        ExecStart =
          let
            args = [
              "--port"
              (toString cfg.port)
              "--backend"
              cfg.backend
            ]
            ++ lib.optionals (cfg.cores != null) [
              "--cores"
              (toString cfg.cores)
            ]
            ++ lib.optionals cfg.quiet [ "--quiet" ];
          in
          "${cfg.package}/bin/weapon-server ${lib.escapeShellArgs args}";
        Restart = "on-failure";
        RestartSec = 5;

        # Hardening
        NoNewPrivileges = true;
        ProtectSystem = "strict";
        ProtectHome = true;
        PrivateTmp = true;
        ReadWritePaths = [ cfg.workDir ];

        # io_uring needs these
        LimitMEMLOCK = "infinity";
        LimitNOFILE = 65536;
      };
    };

    networking.firewall.allowedTCPPorts = lib.mkIf cfg.openFirewall [ cfg.port ];
  };
}
