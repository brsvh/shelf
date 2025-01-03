{
  my,
  ...
}:
{
  imports = [
    my.homeModules.sbcl
  ];

  programs = {
    sbcl = {
      config = ''

      '';

      enable = true;
    };
  };
}
