{ config, lib, pkgs, ... }:

with lib;
let cfg = config.services.pokedex-pfpa;

in
{
  options.services.pokedex-pfpa = {
    enable = mkEnableOption "pokedex-pfpa";

    package = mkOption {
      type = types.package;
      default = pkgs.pokedex-pfpa;
      defaultText = "pkgs.pokedex-pfpa";
      description = ''
        The package implementing pokedex-pfpa
      '';
    };
    port = mkOption {
      type = types.int;
      default = 8080;
      description = "Port number to bind to.";
    };
    sessionKeyFile = mkOption {
      type = types.path;
      description = "Path containing .aes session private key";
      default = null;
    };
    openFirewall = mkOption {
      type = types.bool;
      default = false;
      description = "Whether to open port in the firewall for the server.";
    };
  };

  config = mkIf cfg.enable {
    systemd.services.pokedex-pfpa = {
      description = "pokedex-pfpa";
      wantedBy = [ "multi-user.target" ];
      serviceConfig = {
        ExecStart = "${cfg.package}/bin/pokedex-pfpa";
        Restart = "on-failure";
        User = "pokedex";
      };
      environment = {
        YESOD_PORT = toString cfg.port;
        YESOD_PGUSER = "pokedex";
        YESOD_PGHOST = "/var/run/postgresql";
        YESOD_PGPORT = "5432";
        YESOD_PGDATABASE = "pokedex";
        YESOD_SESSION_KEY_PATH = toString cfg.sessionKeyFile;
      };
    };

    users = {
      users.pokedex = {
        description = "pokedex-pfpa service user";
        isSystemUser = true;
        group = "pokedex";
      };
      groups.pokedex = { };
    };

    networking.firewall =
      mkIf cfg.openFirewall { allowedTCPPorts = [ cfg.port ]; };
  };
}
