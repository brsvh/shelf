{
  config,
  flake-parts-lib,
  lib,
  ...
}:
with lib;
let
  inherit (flake-parts-lib) mkSubmoduleOptions;
in {
  options = {
    flake = mkSubmoduleOptions {
      homeConfigurations = mkOption {
        type = types.lazyAttrsOf types.raw;
        default = {};
        description = ''
          Instantiated home-manager configurations.

           Used by `home-manager`.

          `homeConfigurations` is for specific users.
        '';
        example = literalExpression ''
          {
            my-home = inputs.home-manager.homeManagerConfiguration {
              inherit pkgs;

              # Specify your home configuration modules here,
              # for example, the path to your home.nix.
              modules = [ ./home.nix ];

              # Optionally use extraSpecialArgs
              # to pass through arguments to home.nix
            };
          }
        '';
      };
    };
  };
}
