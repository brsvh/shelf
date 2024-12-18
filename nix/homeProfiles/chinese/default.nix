{
  my,
  pkgs,
  ...
}:
{
  imports = [
    my.homeModules.fonts
    my.homeProfiles.fonts
    my.homeProfiles.rime
  ];

  fonts = {
    fontconfig = {
      emoji = {
        enable = true;
      };

      languages = {
        chinese = {
          enable = true;

          fonts = with pkgs; [
            TH-fonts
            alibaba-fonts
            foundertype-fonts
            lxgw-neoxihei
            lxgw-wenkai
            trionestype-fonts
            tsangertype-fonts
            windows-fonts
          ];

          sansSerif = "IBM Plex Sans SC";
          serif = "Zhuque Fangsong (technical preview)";
          monospace = "LXGW WenKai Mono";
        };
      };

      symbol = {
        enable = true;
      };
    };
  };
}
