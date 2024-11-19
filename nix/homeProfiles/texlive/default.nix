{
  config,
  lib,
  my,
  pkgs,
  ...
}:
let
  inherit (lib)
    concatStringsSep
    flatten
    mapAttrsToList
    ;
in
{
  imports = [
    my.homeProfiles.envvars
  ];

  home = {
    packages = with pkgs; [
      ghostscript
    ];

    sessionVariables = {
      TEXMFCONFIG = "${config.xdg.configHome}/texmf";
      TEXMFHOME = "${config.xdg.dataHome}/texmf";
      TEXMFVAR = "${config.xdg.cacheHome}/texmf";
    };
  };

  programs = {
    texlive = {
      enable = true;
      extraPackages = tpkgs: {
        inherit (tpkgs)
          scheme-full
          ;
      };
    };
  };

  xdg = {
    dataFile = {
      "texmf/fonts" =
        let
          fonts = flatten (mapAttrsToList (n: v: v.fonts) config.fonts.fontconfig.languages);

          fontsPkg = pkgs.stdenvNoCC.mkDerivation {
            pname = "texmf-fonts";

            version = "0-unstable";

            buildInputs = [
              pkgs.xorg.lndir
            ] ++ fonts;

            unpackPhase = "true";

            installPhase = ''
              runHook preInstall

              mkdir -p $out
              for font in ${concatStringsSep " " fonts}; do
                ${pkgs.xorg.lndir}/bin/lndir -silent $font $out
              done

              runHook postInstall
            '';
          };
        in
        {
          recursive = true;
          source = "${fontsPkg}/share/font";
        };
    };
  };
}
