{
  inputs,
  self,
  ...
}:
let
  inherit (inputs)
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
      self.overlays.emacs
      self.overlays.nixpkgs
    ];
  };
}
