{
  config,
  lib,
  pkgs,
  ...
}:
let
  inherit (builtins)
    elem
    ;

  inherit (lib)
    filterAttrs
    flatten
    mapAttrsToList
    mdDoc
    mkOption
    optional
    types
    ;

  presets = {
    chinese =
      let
        inherit (config.fonts.fontconfig.languages.chinese)
          sansSerif
          serif
          monospace
          ;
      in
      ''
        <match target="pattern">
          <test name="lang" compare="contains">
            <string>zh</string>
          </test>
          <test name="family">
            <string>serif</string>
          </test>
          <edit name="family" mode="prepend">
            <string>${serif}</string>
          </edit>
        </match>

        <match target="pattern">
          <test name="lang" compare="contains">
            <string>zh</string>
          </test>
          <test name="family">
            <string>sans-serif</string>
          </test>
          <edit name="family" mode="prepend">
            <string>${sansSerif}</string>
          </edit>
        </match>

        <match target="pattern">
          <test name="lang" compare="contains">
            <string>zh</string>
          </test>
          <test name="family">
            <string>monospace</string>
          </test>
          <edit name="family" mode="prepend">
            <string>${monospace}</string>
          </edit>
        </match>

        <match target="pattern">
          <test name="lang" compare="contains">
            <string>zh</string>
          </test>
          <test qual="any" name="family">
            <string>system-ui</string>
          </test>
          <edit name="family" mode="prepend" binding="strong">
            <string>${sansSerif}</string>
          </edit>
        </match>

        <match target="pattern">
          <test name="lang" compare="contains">
            <string>zh</string>
          </test>
          <test qual="any" name="family">
            <string>ui-sans-serif</string>
          </test>
          <edit name="family" mode="prepend" binding="strong">
            <string>${sansSerif}</string>
          </edit>
        </match>

        <match target="pattern">
          <test qual="any" name="family">
            <string>WenQuanYi Zen Hei</string>
          </test>
          <edit name="family" mode="assign" binding="same">
            <string>${sansSerif}</string>
          </edit>
        </match>

        <match target="pattern">
          <test qual="any" name="family">
            <string>WenQuanYi Micro Hei</string>
          </test>
          <edit name="family" mode="assign" binding="same">
            <string>${sansSerif}</string>
          </edit>
        </match>

        <match target="pattern">
          <test qual="any" name="family">
            <string>WenQuanYi Micro Hei Light</string>
          </test>
          <edit name="family" mode="assign" binding="same">
            <string>${sansSerif}</string>
          </edit>
        </match>

        <match target="pattern">
          <test qual="any" name="family">
            <string>Microsoft YaHei</string>
          </test>
          <edit name="family" mode="assign" binding="same">
            <string>${sansSerif}</string>
          </edit>
        </match>

        <match target="pattern">
          <test qual="any" name="family">
            <string>SimHei</string>
          </test>
          <edit name="family" mode="assign" binding="same">
            <string>${sansSerif}</string>
          </edit>
        </match>

        <match target="pattern">
          <test qual="any" name="family">
            <string>SimSun</string>
          </test>
          <edit name="family" mode="assign" binding="same">
            <string>${serif}</string>
          </edit>
        </match>

        <match target="pattern">
          <test qual="any" name="family">
            <string>SimSun-18030</string>
          </test>
          <edit name="family" mode="assign" binding="same">
            <string>${serif}</string>
          </edit>
        </match>
      '';

    emoji =
      let
        inherit (config.fonts.fontconfig.emoji)
          defaultFont
          ;
      in
      ''
        <match>
          <test qual="any" name="family">
            <string>emoji</string>
          </test>
          <edit name="family" mode="prepend" binding="strong">
            <string>${defaultFont}</string>
          </edit>
        </match>
      '';

    english =
      let
        inherit (config.fonts.fontconfig.languages.english)
          sansSerif
          serif
          monospace
          ;
      in
      ''
        <match target="pattern">
          <test name="lang" compare="contains">
            <string>en</string>
          </test>
          <test qual="any" name="family">
            <string>serif</string>
          </test>
          <edit name="family" mode="prepend" binding="strong">
            <string>${serif}</string>
          </edit>
        </match>

        <match target="pattern">
          <test name="lang" compare="contains">
            <string>en</string>
          </test>
          <test qual="any" name="family">
            <string>sans-serif</string>
          </test>
          <edit name="family" mode="prepend" binding="strong">
            <string>${sansSerif}</string>
          </edit>
        </match>

        <match target="pattern">
          <test name="lang" compare="contains">
            <string>en</string>
          </test>
          <test qual="any" name="family">
            <string>monospace</string>
          </test>
          <edit name="family" mode="prepend" binding="strong">
            <string>${monospace}</string>
          </edit>
        </match>

        <match target="pattern">
          <test name="lang" compare="contains">
            <string>en</string>
          </test>
          <test qual="any" name="family">
            <string>system-ui</string>
          </test>
          <edit name="family" mode="prepend" binding="strong">
            <string>${sansSerif}</string>
          </edit>
        </match>

        <match target="pattern">
          <test name="lang" compare="contains">
            <string>en</string>
          </test>
          <test qual="any" name="family">
            <string>ui-sans-serif</string>
          </test>
          <edit name="family" mode="prepend" binding="strong">
            <string>${sansSerif}</string>
          </edit>
        </match>
      '';

    japanese =
      let
        inherit (config.fonts.fontconfig.languages.japanese)
          sansSerif
          serif
          monospace
          ;
      in
      ''
        <match target="pattern">
          <test name="lang" compare="contains">
            <string>ja</string>
          </test>
          <test name="family">
            <string>serif</string>
          </test>
          <edit name="family" mode="prepend">
            <string>${serif}</string>
          </edit>
        </match>

        <match target="pattern">
          <test name="lang" compare="contains">
            <string>ja</string>
          </test>
          <test name="family">
            <string>sans-serif</string>
          </test>
          <edit name="family" mode="prepend">
            <string>${sansSerif}</string>
          </edit>
        </match>

        <match target="pattern">
          <test name="lang" compare="contains">
            <string>ja</string>
          </test>
          <test name="family">
            <string>monospace</string>
          </test>
          <edit name="family" mode="prepend">
            <string>${monospace}</string>
          </edit>
        </match>
      '';

    korean =
      let
        inherit (config.fonts.fontconfig.languages.korean)
          sansSerif
          serif
          monospace
          ;
      in
      ''
        <match target="pattern">
          <test name="lang" compare="contains">
            <string>ko</string>
          </test>
          <test name="family">
            <string>serif</string>
          </test>
          <edit name="family" mode="prepend">
            <string>${serif}</string>
          </edit>
        </match>

        <match target="pattern">
          <test name="lang" compare="contains">
            <string>ko</string>
          </test>
          <test name="family">
            <string>sans-serif</string>
          </test>
          <edit name="family" mode="prepend">
            <string>${sansSerif}</string>
          </edit>
        </match>

        <match target="pattern">
          <test name="lang" compare="contains">
            <string>ko</string>
          </test>
          <test name="family">
            <string>monospace</string>
          </test>
          <edit name="family" mode="prepend">
            <string>${monospace}</string>
          </edit>
        </match>
      '';

    symbol =
      let
        inherit (config.fonts.fontconfig.symbol)
          defaultFont
          defaultMonoFont
          ;
      in
      ''
        <match>
          <test qual="any" name="family">
            <string>sans-serif</string>
          </test>
          <edit name="family" mode="prepend" binding="strong">
            <string>${defaultFont}</string>
          </edit>
        </match>

        <match>
          <test qual="any" name="family">
            <string>serif</string>
          </test>
          <edit name="family" mode="prepend" binding="strong">
            <string>${defaultFont}</string>
          </edit>
        </match>

        <match>
          <test qual="any" name="family">
            <string>monospace</string>
          </test>
          <edit name="family" mode="prepend" binding="strong">
            <string>${defaultMonoFont}</string>
          </edit>
        </match>
      '';
  };

  mkLangOptions =
    lang: fonts:
    {
      monospace,
      sansSerif,
      serif,
    }:
    {
      enable = mkOption {
        default = false;

        description = mdDoc ''
          Whether to enable ${lang} fontconfig.
        '';

        type = types.bool;
      };

      fonts = mkOption {
        default = fonts;

        description = mdDoc ''
          The fonts packages of ${lang} will be used.
        '';

        type = with types; listOf package;
      };

      monospace = mkOption {
        default = monospace;

        description = mdDoc ''
          The default monospace font name of ${lang}.
        '';

        type = types.str;
      };

      sansSerif = mkOption {
        default = sansSerif;

        description = mdDoc ''
          The default sans font name of ${lang}.
        '';

        type = types.str;
      };

      serif = mkOption {
        default = serif;

        description = mdDoc ''
          The default serif font name of ${lang}.
        '';

        type = types.str;
      };
    };

  fc =
    let
      cfg = config.fonts.fontconfig;

      mkFC =
        name: config:
        let
          pname = "fc-51-local-" + name + ".conf";
        in
        pkgs.writeText pname config;

      fcText =
        let
          isEnable = name: if cfg.${name}.enable then name else "";

          isLangEnable = name: if cfg.languages.${name}.enable then name else "";

          findPreset = name: if name == "" then "" else presets.${name};
        in
        ''
          <?xml version='1.0'?>
          <!DOCTYPE fontconfig SYSTEM 'urn:fontconfig:fonts.dtd'>
          <fontconfig>
            <description>Local fontconfig</description>

            ${findPreset (isEnable "emoji")}

            ${findPreset (isEnable "symbol")}

            ${findPreset (isLangEnable "english")}

            ${findPreset (isLangEnable "chinese")}

            ${findPreset (isLangEnable "japanese")}

            ${findPreset (isLangEnable "korean")}

          </fontconfig>
        '';
    in
    pkgs.runCommand "extra-local-fontconfig-conf"
      {
        preferLocalBuild = true;
        allowSubstitutes = false;
      }
      ''
        dst=$out/etc/fonts/conf.d
        mkdir -p $dst

        ln -s ${mkFC "local-fontconfig" fcText} $dst/51-local-fontconfig.conf
      '';
in
{
  options = {
    fonts = {
      fontconfig = {
        _configPackage = mkOption {
          default = fc;

          description = ''
            The path of fontconfig config file.
          '';

          internal = true;
          type = types.path;
        };

        _fonts =
          let
            cfg = config.fonts.fontconfig;

            getLangFonts =
              let
                languages = filterAttrs (n: v: v.enable) cfg.languages;
              in
              flatten (mapAttrsToList (_n: v: v.fonts) languages);
          in
          mkOption {
            default = flatten (
              getLangFonts
              ++ (optional cfg.emoji.enable cfg.emoji.fonts)
              ++ (optional cfg.symbol.enable cfg.symbol.fonts)
            );

            description = ''
              The list of font derivations.
            '';

            internal = true;
            type = types.unspecified;
          };

        languages = {
          chinese =
            mkLangOptions "Chinese"
              (with pkgs; [
                noto-fonts-cjk-sans
                noto-fonts-cjk-serif
              ])
              {
                monospace = "Noto Sans Mono CJK SC";
                sansSerif = "Noto Sans CJK SC";
                serif = "Noto Serif CJK SC";
              };

          english = mkLangOptions "English" [ pkgs.noto-fonts ] {
            monospace = "Noto Sans Mono";
            sansSerif = "Noto Sans";
            serif = "Noto Serif";
          };

          japanese =
            mkLangOptions "Japanese"
              (with pkgs; [
                noto-fonts-cjk-sans
                noto-fonts-cjk-serif
              ])
              {
                sansSerif = "Noto Sans CJK JP";
                serif = "Noto Serif CJK JP";
                monospace = "Noto Sans Mono CJK JP";
              };

          korean =
            mkLangOptions "Korean"
              (with pkgs; [
                noto-fonts-cjk-sans
                noto-fonts-cjk-serif
              ])
              {
                sansSerif = "Noto Sans CJK KR";
                serif = "Noto Serif CJK KR";
                monospace = "Noto Sans Mono CJK KR";
              };
        };

        emoji = {
          defaultFont = mkOption {
            type = types.str;
            default = "Noto Color Emoji";
            description = mdDoc ''
              The default emoji font name.
            '';
          };

          enable = mkOption {
            type = types.bool;
            default = false;
            description = mdDoc ''
              Whether to enable emoji fontconfig.
            '';
          };

          fonts = mkOption {
            type = with types; listOf package;
            default = with pkgs; [ noto-fonts-color-emoji ];
            description = mdDoc ''
              The fonts packages of emoji fontconfig will be used.
            '';
          };
        };

        symbol =
          let
            cfg = config.fonts.fontconfig.symbol;
          in
          {
            defaultFont = mkOption {
              default = cfg.nerdFonts.defaultFont;

              description = mdDoc ''
                The default symbol font name.
              '';

              type = types.str;
            };

            defaultMonoFont = mkOption {
              default = cfg.nerdFonts.defaultMonoFont;

              description = mdDoc ''
                The default monospace symbol font name.
              '';

              type = types.str;
            };

            enable = mkOption {
              default = false;

              description = mdDoc ''
                Whether to enable emoji fontconfig.
              '';

              type = types.bool;
            };

            fonts = mkOption {
              default = cfg.nerdFonts.fonts;

              description = mdDoc ''
                The fonts packages of symbol will be used.
              '';

              type = with types; listOf package;
            };

            nerdFonts = {
              defaultFont = mkOption {
                default = "Symbols Nerd Font";

                description = mdDoc ''
                  The default Nerd Font name.
                '';

                type = types.str;
              };

              defaultMonoFont = mkOption {
                default = "Symbols Nerd Font Mono";

                description = mdDoc ''
                  The default monospace Nerd Font name.
                '';

                type = types.str;
              };

              fonts = mkOption {
                default = with pkgs.nerd-fonts; [
                  symbols-only
                ];

                type = with types; listOf package;
              };
            };
          };
      };

      size = mkOption {
        type = types.nullOr types.int;
        default = 11;
        example = 16;
        description = ''
          The size of the font.
        '';
      };
    };
  };
}
