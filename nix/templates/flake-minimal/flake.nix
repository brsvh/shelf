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
      url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    };
  };

  outputs =
    {
      nixpkgs,
      self,
      ...
    }:
    let
      inherit (nixpkgs.lib)
        genAttrs
        systems
        ;

      eachSystem =
        func:
        genAttrs systems.flakeExposed (
          system:
          let
            overlays = [
              self.overlays.default
            ];

            pkgs = import nixpkgs {
              inherit
                overlays
                system
                ;
            };
          in
          func {
            inherit
              pkgs
              self
              system
              ;
          }
        );
    in
    {
      devShells = eachSystem (
        {
          pkgs,
          ...
        }:
        {
          default = pkgs.mkShell {
            packages = [ pkgs.emptyFile ];
            shellHook = ''
              # Here is what you need to do when activate the project shell.
            '';
          };
        }
      );

      packages = eachSystem (
        {
          pkgs,
          ...
        }:
        {
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
        }
      );

      overlays = final: prev: { };
    };
}
