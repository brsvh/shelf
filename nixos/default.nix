{ config
, inputs
, lib
, self
, withSystem
, ...
}:
with builtins;
with lib;
let
  inherit (inputs.haumea.lib) load loaders;

  hosts = load {
    src = ../nixos;
    loader = loaders.path;
  };
in
{
  configurations = {
    default = {
      nixos = {
        inherit (inputs) nixpkgs;
        system = "x86_64-linux";
        stateVersion = "23.05";
      };
    };

    global = {
      nixos = {
        modules =
          [
            inputs.disko.nixosModules.disko
            inputs.home-manager.nixosModules.home-manager
            inputs.lanzaboote.nixosModules.lanzaboote
            {
              home-manager = {
                useGlobalPkgs = mkDefault true;
                useUserPackages = mkDefault true;
              };
              nixpkgs = {
                overlays =
                  [
                    inputs.emacs-overlay.overlays.default
                    inputs.rust-overlay.overlays.default
                  ];
              };
            }
          ];
        specialArgs = {
          inherit (inputs) hardware;
        };
      };
    };

    nixos = {
      "eustoma" = {
        domain = "brsvh.org";
        modules =
          [
            hosts.eustoma.configuration
            hosts.eustoma.disko
          ];
        nixpkgs = inputs.nixpkgs-unstable;
        stateVersion = "23.11";
      };
    };
  };

  flake = {
    nixosConfigurations =
      mapAttrs
        (_: cfg: cfg.finalNixOSConfiguration)
        config.configurations.nixos;
  };
}
