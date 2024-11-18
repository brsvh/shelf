{
  inputs,
  self,
  ...
}:
let
  inherit (inputs)
    browser
    chinese-fonts-overlay
    nix-alien
    nixpkgs
    ;
in
{
  nix = {
    registry = {
      nixpkgs = {
        flake = nixpkgs;
      };
    };
  };

  nixpkgs = {
    config = {
      allowUnfree = true;
    };

    overlays = [
      chinese-fonts-overlay.overlays.default
      nix-alien.overlays.default
      self.overlays.epkgs
      self.overlays.nixpkgs
      (
        final: prev:
        let
          inherit (prev.stdenv)
            system
            ;
        in
        if system == "x86_64-linux" then
          {
            inherit (browser.packages.${system})
              google-chrome
              google-chrome-beta
              google-chrome-dev
              ;
          }
        else
          { }
      )
    ];
  };
}
