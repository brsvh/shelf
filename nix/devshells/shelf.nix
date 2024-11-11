{
  lib,
  pkgs,
  ...
}:
let
  inherit (builtins)
    attrNames
    concatStringsSep
    ;

  inherit (lib)
    generators
    getExe
    optional
    pipe
    ;
in
{
  devshell = {
    name = "shelf";

    startup = {
      "lefthook.yml" = {
        deps = [
          ".conform.yaml"
          "treefmt.toml"
        ];
      };
    };
  };

  nixago = [
    # conform
    {
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

      format = "yaml";
      output = ".conform.yaml";

      packages = with pkgs; [
        conform
      ];
    }
    # editorconfig
    {
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
          max_line_length = 120;
        };
      };

      engine =
        request:
        let
          inherit (request)
            data
            output
            ;

          name = baseNameOf output;

          value = {
            globalSection = {
              root = data.root or true;
            };

            sections = removeAttrs data [
              "root"
            ];
          };
        in
        pkgs.writeText name (generators.toINIWithGlobalSection { } value);

      output = ".editorconfig";

      packages = with pkgs; [
        editorconfig-checker
      ];
    }
    # lefthook
    {
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

          skip = [
            {
              ref = "update_flake_lock_action";
            }
          ];
        };
      };

      format = "yaml";

      hook = {
        extra =
          cfg:
          let
            mkScript =
              stage:
              pkgs.writeScript "lefthook-${stage}" ''
                #!${pkgs.runtimeShell}
                [ "$LEFTHOOK" == "0" ] || ${getExe pkgs.lefthook} run "${stage}" "$@"
              '';
          in
          pipe cfg [
            (
              config:
              removeAttrs config [
                "colors"
                "extends"
                "skip_output"
                "source_dir"
                "source_dir_local"
              ]
            )
            attrNames
            (map (stage: ''ln -sf "${mkScript stage}" ".git/hooks/${stage}"''))
            (stages: optional (stages != [ ]) "mkdir -p .git/hooks" ++ stages)
            (concatStringsSep "\n")
          ];
      };

      output = "lefthook.yml";

      packages = with pkgs; [
        lefthook
      ];
    }
    # sops
    {
      data =
        let
          eustoma = {
            age = "age1lgy77wf7vxlvvv8lzsgmq6wgf43c4hl93ls2mw8pspmdcuzqvems7svu6t";
          };

          lilac = {
            age = "age1g5777szdqk5all8tq823v5gzzjag5j3xk92gy44e5rqz9ktya4hsnjr8m9";
          };
        in
        {
          creation_rules = [
            {
              key_groups = [
                {
                  age = [
                    eustoma.age
                  ];
                }
              ];

              path_regex = "^etc/sops/eustoma\.yaml$";
            }
            {
              key_groups = [
                {
                  age = [
                    lilac.age
                  ];
                }
              ];

              path_regex = "^etc/sops/lilac\.yaml$";
            }
          ];
        };

      format = "yaml";
      output = ".sops.yaml";

      packages = with pkgs; [
        age
        sops
        ssh-to-age
        ssh-to-pgp
      ];
    }
    # treefmt
    {
      data = {
        formatter = {
          emacs-lisp = {
            command = "my-emacs-formatter";

            includes = [
              "*.el"
            ];
          };

          nix = {
            command = "nixfmt";

            includes = [
              "*.nix"
            ];
          };

          shellcheck = {
            command = "shellcheck";

            includes = [
              "*.sh"
            ];
          };

          shfmt = {
            command = "shfmt";

            includes = [
              "*.sh"
            ];
          };
        };
      };

      format = "toml";
      output = "treefmt.toml";

      packages = with pkgs; [
        my-emacs-formatter
        nixfmt-rfc-style
        shellcheck
        shfmt
        treefmt
      ];
    }
  ];
}
