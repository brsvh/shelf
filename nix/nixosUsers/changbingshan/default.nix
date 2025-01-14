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
      changbingshan =
        {
          ...
        }:
        {
          imports = [
            my.homeConfigurations.changbingshan
          ];
        };
    };
  };

  users = {
    users = {
      changbingshan = {
        description = "Bingshan Chang";

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
              "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIF8kaNm6jre8kIxo3LSWe9Ohz8tXc4jRcSCw1BeoC7tj bsc@eustoma"
            ];
          };
        };

        shell = config.programs.fish.package;
      };
    };
  };
}
