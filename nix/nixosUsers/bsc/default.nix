{
  config,
  my,
  ...
}:
{
  imports = [
    my.nixosProfiles.fish
    my.nixosProfiles.home-manager
  ];

  home-manager = {
    users = {
      bsc =
        {
          ...
        }:
        {
          imports = [
            my.homeConfigurations.bsc
          ];
        };
    };
  };

  system = {
    activationScripts = {
      avatar = {
        text =
          let
            avatar = my.etc.avatar.bsc;
            user = "bsc";
            icon = "/var/lib/AccountsService/icons/${user}";
            unit = "/var/lib/AccountsService/users/${user}";
          in
          ''
            mkdir -p /var/lib/AccountsService/{icons,users}
            cp ${avatar} ${icon}
            echo -e "[User]\nIcon=${icon}\n" > ${unit}
            chown root:root ${unit}
            chmod 0600 ${unit}

            chown root:root ${icon}
            chmod 0444 ${icon}
          '';
      };
    };
  };

  users = {
    users = {
      bsc = {
        description = "Burgess Chang";

        extraGroups = [
          "audio"
          "davfs2"
          "dialout"
          "disk"
          "docker"
          "input"
          "jackaudio"
          "libvirtd"
          "network"
          "networkmanager"
          "systemd-journal"
          "video"
          "wheel"
        ];

        initialHashedPassword = "$6$cB3EK3Lynl./0Bio$bgH7P93D1lpgvEIJ3iks7Dk2IyNue7ria2aH8.xkZZ1PPooxkb7p/bEMN1UtJaV0TIeVr/eTY8oAN38vBgMKe0";
        isNormalUser = true;

        openssh = {
          authorizedKeys = {
            keys = [
              "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIA4jP/6P5pGF7sTTUSB3TZfONcvuVnQj+L794Q5sUsKn changbingshan@lilac"
            ];
          };
        };

        shell = config.programs.fish.package;
      };
    };
  };
}
