# NixOS module for weapon-server
{
  config,
  lib,
  pkgs,
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
  };

  config = lib.mkIf cfg.enable {
    users.users.${cfg.user} = {
      isSystemUser = true;
      group = cfg.group;
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
        ExecStart = "${cfg.package}/bin/weapon-server";
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
