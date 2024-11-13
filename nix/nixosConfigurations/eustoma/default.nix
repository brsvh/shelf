{
  my,
  ...
}:
{
  imports = [
    my.diskoConfigurations.eustoma
    my.nixosProfiles.binfmt
    my.nixosProfiles.chinese
    my.nixosProfiles.docker
    my.nixosProfiles.fprintd
    # my.nixosProfiles.guix
    my.nixosProfiles.libvirt
    my.nixosProfiles.virtual-camera
    my.nixosProfiles.secure-boot
    # REVIEW remove this file after nixos-facter support setup Touch Pad
    # See https://github.com/numtide/nixos-facter-modules/issues/47 .
    my.nixosProfiles.touchpad
    my.nixosSecrets.eustoma
    my.nixosSuites.gnomeWorkstation
    my.nixosUsers.bsc
  ];

  console = {
    keyMap = "us";
  };

  facter = {
    reportPath = my.etc.facter.eustoma;
  };

  i18n = {
    languages = {
      english = {
        isDefault = true;
      };
    };
  };

  networking = {
    hostName = "eustoma";
  };

  system = {
    stateVersion = "24.11";
  };

  time = {
    timeZone = "Asia/Shanghai";
  };

  virtualisation = {
    docker = {
      # TODO check storageDriver by current filesystems.
      storageDriver = "btrfs";
    };
  };
}
