{
  config,
  inputs,
  lib,
  modulesPath,
  pkgs,
  ...
}:
let
  inherit (inputs) disko hardware;

  # This device will not be exposed to the public network. The domain
  # name setting is fake, solely to automatically configure the correct
  # email domain in some software.
  domainName = "iscas.ac.cn";

  hostName = "lilac";

  system = "x86_64-linux";
in
{
  imports = [
    cell.nixosProfiles.dae
    # REVIEW re-enable after upstream compatibility with Cachix 1.7.3.
    # cell.nixosProfiles.hercules-ci-agent
    cell.nixosSecrets.lilac
    cell.nixosSuites.gnome-workstation
    cell.nixosSuites.laptop
    cell.nixosUsers.root
    cell.nixosUsers.changbingshan
    disko.nixosModules.disko
    hardware.nixosModules.common-cpu-intel
  ];

  bee = {
    inherit system;

    home = inputs.home-manager-unstable;

    pkgs = import inputs.nixos-unstable {
      inherit system;

      config = {
        allowUnfree = true;
      };
    };
  };

  boot = {
    initrd = {
      availableKernelModules = [
        "nvme"
        "sd_mod"
        "thunderbolt"
        "usb_storage"
        "vmd"
        "xhci_pci"
      ];
    };

    kernelModules = [ "kvm-intel" ];
    kernelPackages = pkgs.linuxPackages_zen;

    loader = {
      efi = {
        canTouchEfiVariables = true;
        efiSysMountPoint = "/boot/efi";
      };
    };
  };

  console = {
    earlySetup = true;
    font = "eurlatgr";
    keyMap = lib.mkDefault "us";
  };

  disko = cell.diskoConfigurations.lilac.disko;

  hardware = {
    enableRedistributableFirmware = lib.mkDefault true;

    nvidia = {
      modesetting = {
        enable = true;
      };

      open = true;

      powerManagement = {
        enable = false;
        finegrained = false;
      };

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

    opengl = {
      enable = true;
      driSupport = true;
      driSupport32Bit = lib.mkDefault true;
    };
  };

  i18n = {
    defaultLocale = "en_US.UTF-8";

    supportedLocales = [
      # ISO-8859-1
      "en_AU/ISO-8859-1"
      "en_BW/ISO-8859-1"
      "en_CA/ISO-8859-1"
      "en_DK/ISO-8859-1"
      "en_GB/ISO-8859-1"
      "en_HK/ISO-8859-1"
      "en_IE/ISO-8859-1"
      "en_NZ/ISO-8859-1"
      "en_PH/ISO-8859-1"
      "en_SG/ISO-8859-1"
      "en_US/ISO-8859-1"
      "en_ZA/ISO-8859-1"
      "en_ZW/ISO-8859-1"

      # ISO-8859-15
      "en_IE@euro/ISO-8859-15"

      # UTF-8
      "en_AU.UTF-8/UTF-8"
      "en_BW.UTF-8/UTF-8"
      "en_CA.UTF-8/UTF-8"
      "en_DK.UTF-8/UTF-8"
      "en_GB.UTF-8/UTF-8"
      "en_HK.UTF-8/UTF-8"
      "en_IE.UTF-8/UTF-8"
      "en_IL/UTF-8"
      "en_IN/UTF-8"
      "en_NG/UTF-8"
      "en_NZ.UTF-8/UTF-8"
      "en_PH.UTF-8/UTF-8"
      "en_SC.UTF-8/UTF-8"
      "en_SG.UTF-8/UTF-8"
      "en_US.UTF-8/UTF-8"
      "en_ZA.UTF-8/UTF-8"
      "en_ZM/UTF-8"
      "en_ZW.UTF-8/UTF-8"
    ];
  };

  networking = {
    inherit hostName;

    domain = domainName;

    fqdn = domainName;
  };

  services = {
    dae = {
      configFile = config.sops.secrets."dae/config.dae".path;
    };

    # REVIEW re-enable after upstream compatibility with Cachix 1.7.3.
    # hercules-ci-agent = {
    #   settings = {
    #     binaryCachesPath = config.sops.secrets."hercules-ci/binary-caches.json".path;
    #     clusterJoinTokenPath = config.sops.secrets."hercules-ci/cluster-join-token.key".path;
    #   };
    # };

    xserver = {
      videoDrivers = [ "nvidia" ];
    };
  };

  swapDevices = [
    {
      device = "/var/lib/swapfile";
      size = 16 * 1024;
    }
  ];

  system = {
    stateVersion = "24.05";
  };

  time = {
    timeZone = "Asia/Shanghai";
  };
}
