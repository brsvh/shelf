{ cell, inputs }:
let
  inherit (inputs) nixpkgs std;

  lib = nixpkgs.lib // builtins;

  pkgs = nixpkgs;
in
with std.lib;
{
  conform = dev.mkNixago cfg.conform {
    data = {
      commit = {
        body = {
          required = false;
        };

        conventional = {
          types = [
            "build"
            "chore"
            "ci"
            "docs"
            "feat"
            "refactor"
            "style"
            "test"
            "fix"
          ];

          scopes = [
            "ci"
            "flake"
          ];
        };

        descriptionLength = 72;

        gpg = {
          required = true;
        };

        header = {
          length = 68;
          imperative = true;
        };

        maximumOfOneCommit = false;
      };
    };
  };

  editorconfig = dev.mkNixago cfg.editorconfig {
    data = {
      root = true;

      "*" = {
        charset = "utf-8";
        end_of_line = "lf";
        indent_size = 2;
        indent_style = "space";
        insert_final_newline = true;
        max_line_length = 72;
        tab_width = 4;
        trim_trailing_whitespace = true;
      };

      "*.nix" = {
        tab_width = 2;
      };
    };
  };

  lefthook = dev.mkNixago cfg.lefthook {
    data = {
      commit-msg = {
        commands = {
          conform = {
            run = ''
              # Allow wip, fixup, squash.
              [[ "$(head -n 1 {1})" =~ ^WIP(:.*)?$|^wip(:.*)?$|fixup\!.*|squash\!.* ]] ||
              conform enforce --commit-msg-file {1}
            '';

            skip = [
              "merge"
              "rebase"
            ];
          };
        };
      };

      pre-commit = {
        commands = {
          treefmt = {
            run = "treefmt --fail-on-change {staged_files}";
            skip = [
              "merge"
              "rebase"
            ];
          };
        };

        skip = [ { ref = "update_flake_lock_action"; } ];
      };
    };
  };

  sops =
    dev.mkNixago
      {
        data = { };
        output = ".sops.yaml";
        format = "yaml";
        packages = with pkgs; [
          age
          sops
          ssh-to-age
          ssh-to-pgp
        ];
      }
      {
        data =
          let
            lilac = {
              age = "age1g5777szdqk5all8tq823v5gzzjag5j3xk92gy44e5rqz9ktya4hsnjr8m9";
            };
          in
          {
            creation_rules = [
              {
                path_regex = "^nix/nixos/secrets/lilac/secrets\.yaml$";
                key_groups = [ { age = [ lilac.age ]; } ];
              }
            ];
          };
      };

  treefmt = dev.mkNixago cfg.treefmt {
    data = {
      formatter = {
        nix = {
          command = "nixfmt";
          includes = [ "*.nix" ];
        };
      };
    };

    packages = [ pkgs.nixfmt-rfc-style ];
  };
}
