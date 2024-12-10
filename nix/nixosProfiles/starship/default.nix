{
  config,
  lib,
  pkgs,
  ...
}:
let
  inherit (lib)
    mkIf
    mkMerge
    ;
in
{
  config =
    let
      # REVIEW find a better way to check desktop/GUI for display
      #        graphical symbols.
      withgui = config.hardware.graphics.enable;
    in
    mkMerge [
      {
        programs = {
          starship = {
            enable = true;

            settings = {
              username = {
                show_always = true;
                disabled = false;
              };
            };
          };
        };
      }
      (mkIf withgui {
        environment = {
          systemPackages = with pkgs.nerd-fonts; [
            symbols-only
          ];
        };

        programs = {
          starship = {
            presets = [
              "nerd-font-symbols"
            ];
          };
        };
      })
      (mkIf (!withgui) {
        programs = {
          starship = {
            presets = [
              "plain-text-symbols"
            ];
          };
        };
      })
    ];
}
