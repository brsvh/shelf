{
  config,
  pkgs,
  ...
}:
{
  gtk = {
    cursorTheme = {
      name = "Vanilla-DMZ";
      package = pkgs.vanilla-dmz;
      size = 16;
    };

    enable = true;

    font = {
      name = config.fonts.fontconfig.languages.english.sansSerif;
      size = config.fonts.size;
    };

    gtk2 = {
      configLocation = "${config.xdg.configHome}/gtk-2.0/gtkrc";

      extraConfig = ''
        gtk-button-images = 1;
        gtk-enable-animations = 1;
        gtk-menu-images = 1;
        gtk-primary-button-warps-slider = 1;
        gtk-toolbar-style = 3;
      '';
    };

    gtk3 = {
      extraConfig = {
        gtk-application-prefer-dark-theme = false;
        gtk-button-images = true;
        gtk-enable-animations = true;
        gtk-menu-images = true;
        gtk-primary-button-warps-slider = true;
        gtk-toolbar-style = 3;
      };
    };

    gtk4 = {
      extraConfig = {
        gtk-application-prefer-dark-theme = false;
        gtk-enable-animations = true;
        gtk-primary-button-warps-slider = true;
      };
    };

    iconTheme = {
      name = "Adwaita";
      package = pkgs.adwaita-icon-theme;
    };
  };
}
