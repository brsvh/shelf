{
  config,
  home-manager-lib,
  lib,
  my,
  osConfig,
  pkgs,
  ...
}:
let
  inherit (builtins)
    map
    toString
    ;

  inherit (lib)
    mkForce
    mkIf
    mkMerge
    optionals
    ;

  inherit (home-manager-lib.gvariant)
    mkTuple
    ;

  withIbus = osConfig.i18n.inputMethod.enable && osConfig.i18n.inputMethod.type == "ibus";

  withGnome = osConfig.services.xserver.desktopManager.gnome.enable or false;

  withTouchPad = osConfig.services.libinput.enable or false;
in
{
  imports =
    [
      my.homeProfiles.xdg
    ]
    ++ optionals withGnome [
      my.homeProfiles.gtk
      my.homeProfiles.qt
      my.homeProfiles.wayland
    ]
    ++ optionals (withGnome && withIbus) [
      my.homeProfiles.ibus
    ];

  config = mkMerge [
    (mkIf withGnome {
      dconf = {
        settings = {
          "org/gnome/desktop/interface" =
            let
              cfg = config.fonts.fontconfig.languages.english;

              size = toString config.fonts.size;
            in
            {
              font-name = "${cfg.sansSerif} ${size}";
              document-font-name = "${cfg.sansSerif} ${size}";
              monospace-font-name = "${cfg.monospace} ${size}";
            };

          "org/gnome/mutter" = {
            dynamic-workspaces = true;
          };

          "org/gnome/settings-daemon/plugins/power" = {
            sleep-inactive-ac-type = "nothing";
          };

          "org/gnome/shell" = {
            disable-user-extensions = false;
            enabled-extensions =
              let
                exts = config.programs.gnome-shell.extensions;
              in
              map (ext: if ext ? id then ext.id else ext.extensionUuid) exts;
          };

          "org/gnome/shell/app-switcher" = {
            current-workspace-only = true;
          };
        };
      };

      gtk = {
        cursorTheme = {
          name = mkForce "Adwaita";
          size = 16;
          package = mkForce pkgs.adwaita-icon-theme;
        };

        iconTheme = {
          name = mkForce "Adwaita";
          package = mkForce pkgs.adwaita-icon-theme;
        };
      };

      qt = {
        platformTheme = {
          name = "adwaita";
        };

        style = {
          name = "adwaita";
        };
      };

      programs = {
        gnome-shell = {
          extensions = with pkgs.gnomeExtensions; [
            {
              id = "appindicatorsupport@rgcjonas.gmail.com";
              package = appindicator;
            }
            {
              id = "user-id-in-top-panel@fthx";
              package = user-id-in-top-panel;
            }
            {
              id = "user-theme@gnome-shell-extensions.gcampax.github.com";
              package = user-themes;
            }
          ];
        };
      };

      services = {
        gpg-agent = {
          pinentryPackage = mkForce pkgs.pinentry-gnome3;
        };
      };

      xdg = {
        mimeApps = {
          defaultApplications = {
            "image/avif" = "org.gnome.Loupe.desktop";
            "image/bmp" = "org.gnome.Loupe.desktop";
            "image/gif" = "org.gnome.Loupe.desktop";
            "image/heic" = "org.gnome.Loupe.desktop";
            "image/jpeg" = "org.gnome.Loupe.desktop";
            "image/jxl" = "org.gnome.Loupe.desktop";
            "image/png" = "org.gnome.Loupe.desktop";
            "image/svg+xml" = "org.gnome.Loupe.desktop";
            "image/svg+xml-compressed" = "org.gnome.Loupe.desktop";
            "image/tiff" = "org.gnome.Loupe.desktop";
            "image/vnd-ms.dds" = "org.gnome.Loupe.desktop";
            "image/vnd.microsoft.icon" = "org.gnome.Loupe.desktop";
            "image/vnd.radiance" = "org.gnome.Loupe.desktop";
            "image/webp" = "org.gnome.Loupe.desktop";
            "image/x-dds" = "org.gnome.Loupe.desktop";
            "image/x-exr" = "org.gnome.Loupe.desktop";
            "image/x-portable-anymap" = "org.gnome.Loupe.desktop";
            "image/x-portable-bitmap" = "org.gnome.Loupe.desktop";
            "image/x-portable-graymap" = "org.gnome.Loupe.desktop";
            "image/x-portable-pixmap" = "org.gnome.Loupe.desktop";
            "image/x-qoi" = "org.gnome.Loupe.desktop";
            "image/x-tga" = "org.gnome.Loupe.desktop";
          };
        };
      };
    })
    (mkIf (withGnome && withIbus) {
      dconf = {
        settings = {
          "org/gnome/desktop/input-sources" = {
            mru-sources = [
              (mkTuple [
                "xkb"
                "us"
              ])
            ];

            per-window = false;

            sources =
              let
                cfg = osConfig.i18n.inputMethod.ibus;
              in
              [
                (mkTuple [
                  "xkb"
                  "us"
                ])
              ]
              ++ (optionals cfg.chinese.enable [
                (mkTuple [
                  "ibus"
                  cfg.chinese.engineName
                ])
              ]);
          };

        };
      };
    })
    (mkIf (withGnome && withTouchPad) {
      dconf = {
        settings = {
          "org/gnome/desktop/peripherals/touchpad" = {
            two-finger-scrolling-enabled = true;
          };
        };
      };
    })
  ];
}
