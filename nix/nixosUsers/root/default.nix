{
  config,
  my,
  ...
}:
{
  imports = [
    my.nixosProfiles.fish
  ];

  users = {
    users = {
      root = {
        initialHashedPassword = "$6$OKxyX8LiVd/RmeVj$CwpXDNgDjJ0FtGg71xxy88R8lBnN/IWk.wzlIQA9gvp56beeLT1asQhKsboaA2SB1xUfcdxSqtwB9eZ/NPeoj.";

        openssh = {
          authorizedKeys = {
            keys = [
              "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIHKpeonaEz9/yjXqZZlymbiky58VI2a78eGg3Q9ebW9r ssh@hercules-ci"
            ];
          };
        };

        shell = config.programs.fish.package;
      };
    };
  };
}
