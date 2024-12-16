{
  lib,
  ...
}:
let
  inherit (lib)
    mdDoc
    mkOption
    types
    ;
in
{
  options = {
    rime = {
      dataDirectory = mkOption {
        default = "~/.config/fcitx/rime";

        description = mdDoc ''
          Data directory of rime.
        '';

        type = types.str;
      };
    };
  };
}
