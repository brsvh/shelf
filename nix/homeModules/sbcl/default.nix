{
  config,
  lib,
  pkgs,
  ...
}:
let
  inherit (lib)
    mdDoc
    mkIf
    mkMerge
    mkOption
    types
    ;

  XDG = config.home.preferXdgDirectories;

  noXDG = !config.home.preferXdgDirectories;

  rcText = ''
    ;;;; sbclrc --- SBCL configuration -*- mode: lisp; -*-

    ${config.programs.sbcl.config}
  '';
in
{
  options = {
    programs = {
      sbcl = {
        config = mkOption {
          default = "";

          description = mdDoc ''
            Configuration for SBCl.
          '';

          type = types.lines;
        };

        enable = mkOption {
          default = false;

          description = mdDoc ''
            Whether to enable SBCL.
          '';

          type = types.bool;
        };

        package = mkOption {
          default = pkgs.sbcl;

          description = mdDoc ''
            SBCL package will be installed.
          '';

          type = types.package;
        };
      };
    };
  };

  config =
    let
      enabled = config.programs.sbcl.enable;
    in
    mkMerge [
      (mkIf enabled {
        home = {
          packages = [
            config.programs.sbcl.package
          ];
        };
      })
      (mkIf (enabled && XDG) {
        xdg = {
          configFile = {
            "sbcl/sbclrc" = {
              text = rcText;
            };
          };
        };
      })
      (mkIf (enabled && noXDG) {
        home = {
          file = {
            ".sbclrc" = {
              text = rcText;
            };
          };
        };
      })
    ];
}
