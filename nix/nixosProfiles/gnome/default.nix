{
  lib,
  my,
  pkgs,
  ...
}:
let
  inherit (lib)
    mkForce
    ;
in
{
  imports = [
    my.nixosProfiles.dbus
    my.nixosProfiles.dconf
    my.nixosProfiles.english
    my.nixosProfiles.ibus
    my.nixosProfiles.wayland
    my.nixosProfiles.xdg
  ];

  environment = {
    systemPackages =
      with pkgs;
      (
        [ adwaita-icon-theme ]
        ++ (with gnomeExtensions; [
          appindicator
          kimpanel
        ])
      );
  };

  programs = {
    dconf = {
      profiles = {
        gdm = {
          databases = [
            {
              settings = {
                "org/gnome/mutter" = {
                  experimental-features = [
                    "scale-monitor-framebuffer"
                  ];
                };
              };
            }
          ];
        };

        user = {
          databases = [
            {
              settings = {
                "org/gnome/mutter" = {
                  dynamic-workspaces = true;
                  edge-tiling = true;

                  experimental-features = [
                    "scale-monitor-framebuffer"
                  ];
                };

                "org/gnome/shell" = {
                  enabled-extensions = [
                    "appindicatorsupport@rgcjonas.gmail.com"
                    "kimpanel@kde.org"
                    "user-theme@gnome-shell-extensions.gcampax.github.com"
                  ];
                };
              };
            }
          ];
        };
      };
    };

    gnupg = {
      agent = {
        pinentryPackage = mkForce pkgs.pinentry-gnome3;
      };
    };
  };

  qt = {
    enable = true;
    platformTheme = "gnome";
    style = "adwaita";
  };

  services = {
    dbus = {
      packages = with pkgs; [ gcr ];
    };

    gnome = {
      gnome-initial-setup = {
        enable = mkForce false;
      };
    };

    udev = {
      packages = with pkgs; [
        gnome-settings-daemon
      ];
    };

    xserver = {
      desktopManager = {
        gnome = {
          enable = true;
        };
      };

      displayManager = {
        gdm = {
          enable = true;
        };
      };

      enable = true;

      excludePackages = with pkgs; [
        xterm
      ];
    };
  };
}
