{
  my,
  ...
}:
{
  imports = [
    my.diskoConfigurations.lilac
    my.nixosProfiles.binfmt
    my.nixosProfiles.chinese
    my.nixosProfiles.docker
    # my.nixosProfiles.guix
    my.nixosProfiles.libvirt
    my.nixosProfiles.nvidia
    my.nixosProfiles.virtual-camera
    my.nixosProfiles.secure-boot
    # REVIEW remove this file after nixos-facter support setup Touch Pad
    # See https://github.com/numtide/nixos-facter-modules/issues/47 .
    my.nixosProfiles.touchpad
    my.nixosSecrets.lilac
    my.nixosSuites.gnomeWorkstation
    my.nixosUsers.changbingshan
  ];

  console = {
    keyMap = "us";
  };

  facter = {
    reportPath = my.etc.facter.lilac;
  };

  hardware = {
    nvidia = {
      prime = {
        intelBusId = "PCI:0:2:0";
        nvidiaBusId = "PCI:1:0:0";

        offload = {
          enable = true;
          enableOffloadCmd = true;
        };

        reverseSync = {
          enable = true;
        };
      };
    };
  };

  i18n = {
    languages = {
      english = {
        isDefault = true;
      };
    };
  };

  networking = {
    hostName = "lilac";
  };

  system = {
    stateVersion = "25.05";
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
