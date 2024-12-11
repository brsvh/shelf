{
  my,
  ...
}:
{
  imports = [
    my.nixosProfiles.kernel-zen
    my.nixosProfiles.network-manager
    my.nixosProfiles.pipewire
    my.nixosProfiles.plymouth
    my.nixosProfiles.starship
    my.nixosProfiles.zram
    my.nixosSuites.base
  ];

  boot = {
    kernelParams = [
      "quiet"
      "loglevel=3"
      "systemd.show_status=auto"
      "rd.udev.log_level=3"
    ];
  };
}
