{
  config,
  inputs,
  lib,
  pkgs,
  ...
}:
let
  inherit (inputs)
    nix-index-database
    ;

  inherit (lib)
    mkDefault
    ;
in
{
  imports = [
    nix-index-database.nixosModules.nix-index
  ];

  environment = {
    systemPackages = with pkgs; [
      # file
      file

      # network
      curl
      dogdns

      # nix
      nix-alien
      nix-output-monitor
      nix-tree
      nvd

      # search
      fd
      findutils
      gnugrep
      ripgrep

      # serial port
      picocom

      # terminal multiplexer
      screen
    ];
  };

  programs = {
    command-not-found = {
      enable = false;
    };

    git = {
      enable = true;

      lfs = {
        enable = true;
      };
    };

    nix-index = {
      enable = true;
      enableBashIntegration = mkDefault true;
      enableFishIntegration = mkDefault config.programs.fish.enable;
      enableZshIntegration = mkDefault config.programs.zsh.enable;
    };

    nix-ld = {
      enable = true;
    };

    tmux = {
      enable = true;
      keyMode = "emacs";
      shortcut = "'";
    };
  };
}
