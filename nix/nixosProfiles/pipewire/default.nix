{
  lib,
  ...
}:
let
  inherit (lib)
    mkForce
    ;
in
{
  services = {
    pulseaudio = {
      enable = mkForce false;
    };

    pipewire = {
      enable = true;

      alsa = {
        enable = true;
        support32Bit = true;
      };

      jack = {
        enable = true;
      };

      pulse = {
        enable = true;
      };

      wireplumber = {
        enable = true;
      };
    };
  };
}
