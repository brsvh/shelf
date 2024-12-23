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
    nix-index-database.hmModules.nix-index
  ];

  home = {
    packages = with pkgs; [
      # calculator
      eva

      # debug
      cntr

      # development

      # diff
      delta

      # file
      choose
      eza
      file
      hex
      rnr

      # monitor

      # network
      curl
      dogdns

      # nix
      nix-alien
      nix-output-monitor
      nix-tree
      nvd

      # nixpkgs
      nixpkgs-review

      # process
      procs

      # search
      agrep
      findutils
      gnugrep
      igrep

      # terminal
      screen

      # wayland
      wl-clipboard

      # x11
      xclip
    ];
  };

  programs = {
    bat = {
      enable = true;

      extraPackages = with pkgs.bat-extras; [
        batdiff
        batman
        batgrep
        batwatch
      ];
    };

    btop = {
      enable = true;
    };

    command-not-found = {
      enable = false;
    };

    fd = {
      enable = true;
    };

    fzf = {
      enable = true;
      enableBashIntegration = mkDefault true;
      enableFishIntegration = mkDefault config.programs.fish.enable;
      enableZshIntegration = mkDefault config.programs.zsh.enable;
    };

    jq = {
      enable = true;
    };

    nix-index = {
      enable = true;
      enableBashIntegration = mkDefault true;
      enableFishIntegration = mkDefault config.programs.fish.enable;
      enableZshIntegration = mkDefault config.programs.zsh.enable;
    };

    ripgrep = {
      enable = true;
    };
  };
}
