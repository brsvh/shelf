{
  inputs,
  lib,
  self,
  ...
}:
let
  inherit (builtins)
    removeAttrs
    ;

  inherit (inputs)
    chinese-fonts-overlay
    devshellago
    home-manager
    nix-alien
    nixpkgs
    nixpkgs-darwin
    nixpkgs-stable
    nixpkgs-unstable
    rust-overlay
    x86_64-linux
    ;

  inherit (inputs.haumea.lib)
    load
    loaders
    matchers
    transformers
    ;

  home-manager-lib = home-manager.lib.hm;

  shelf-lib = import ./lib {
    inherit lib;
  };

  inherit (shelf-lib.collectors)
    rakeLeaves
    ;

  inherit (shelf-lib.importers)
    importSystem
    ;

  my =
    let
      my' = (
        removeAttrs (rakeLeaves ./.) [
          "channels"
          "packages"
          "templates"
        ]
      );
    in
    my'
    // {
      channels = load {
        inputs = {
          inherit
            nixpkgs-darwin
            nixpkgs-stable
            nixpkgs-unstable
            overlays
            ;

          x86_64-linux = importSystem x86_64-linux;
        };

        src = ./channels;
        transformer = transformers.liftDefault;
      };

      etc = load {
        loader = [
          (matchers.always loaders.path)
        ];

        src = ../etc;
      };

      lib = shelf-lib;

      root = ../.;
    };

  overlays = [
    chinese-fonts-overlay.overlays.default
    nix-alien.overlays.default
    self.overlays.emacs
    self.overlays.nixpkgs
  ];
in
{
  imports = [
    devshellago.flakeModule
    my.flakeModules.myConfigs
    my.flakeModules.options
  ];

  flake = {
    inherit
      my
      ;

    inherit (my)
      flakeModules
      ;

    lib = shelf-lib;

    overlays = {
      emacs = final: prev: import my.overlays.emacs final prev;
      nixpkgs = final: prev: import my.overlays.nixpkgs final prev;
    };

    templates = {
      flake-minimal = {
        description = "A minimal nix flake template";
        path = ./templates/flake-minimal;
      };
    };
  };

  myConfigs = {
    root = ./.;

    globalArgs = {
      inherit
        home-manager-lib
        inputs
        my
        self
        shelf-lib
        ;
    };

    home = {
      users = {
        bsc = {
          pkgs = my.channels.x86_64-linux-unstable;
        };

        changbingshan = {
          pkgs = my.channels.x86_64-linux-unstable;
        };
      };
    };
  };

  perSystem =
    {
      lib,
      pkgs,
      system,
      ...
    }:
    let
      inherit (lib)
        mapAttrs'
        nameValuePair
        ;

      devshells = mapAttrs' (
        n: s:
        nameValuePair n (
          import s {
            inherit
              lib
              pkgs
              ;
          }
        )
      ) my.devshells;
    in
    {
      _module = {
        args = {
          pkgs = import nixpkgs {
            inherit system;

            overlays = [
              rust-overlay.overlays.default
              self.overlays.emacs
              self.overlays.nixpkgs
            ];
          };
        };
      };

      devshells = devshells // {
        default = devshells.shelf;
      };

      formatter = pkgs.treefmt;
    };
}
