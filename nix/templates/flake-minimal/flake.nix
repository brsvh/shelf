{
  description = ''
    TODO DESCRIBE PROJECT HERE
  '';

  nixConfig = {
    experimental-features = [
      "ca-derivations"
      "flakes"
      "nix-command"
    ];

    extra-substituters = [
      # Add extra substituters here.
    ];

    extra-trusted-public-keys = [
      # Add the public key of extra substituters here.
    ];
  };

  # Channels
  inputs = {
    nixpkgs = {
      follows = "nixpkgs-unstable";
    };

    nixpkgs-stable = {
      url = "github:NixOS/nixpkgs/nixos-24.11";
    };

    nixpkgs-unstable = {
      url = "github:NixOS/nixpkgs/nixos-unstable";
    };
  };

  # Libraries
  inputs = {
    flake-compat = {
      url = "github:edolstra/flake-compat/master";
      flake = false;
    };

    systems = {
      url = "github:nix-systems/default-linux/main";
    };
  };

  outputs =
    {
      default-linux,
      nixpkgs,
      self,
      systems,
      ...
    }@inputs:
    let
      inherit (nixpkgs.lib)
        genAttrs
        ;

      eachSystem =
        f:
        genAttrs (import systems) (
          system:
          let
            overlays = [ ];

            pkgs = import nixpkgs {
              inherit
                overlays
                system
                ;
            };
          in
          f pkgs
        );
    in
    {
      devShells = eachSystem (pkgs: {
        default = pkgs.mkShell {
          packages = [ pkgs.emptyFile ];
          shellHook = ''
            # Here is what you need to do when activate the project shell.
          '';
        };
      });

      packages = eachSystem (pkgs: {
        default =
          let
            src = ../.;

            version = self.shortRev or "0000000";

            meta = {
              homepage = "";
              description = "";
              maintainers = [ ];
            };
          in
          pkgs.stdenv.mkDerivation {
            inherit
              meta
              src
              version
              ;

            pname = "PROJECT";

            buildInputs = [
              # Add build dependencies here.
            ];

            nativeBuildInputs = [
              # Add build dependencies here.
            ];

            buildPhase = ''
              runHook preBuild

              # Add build script here.

              runHook postBuild
            '';
          };
      });
    };
}
