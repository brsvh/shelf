{
  description = "shelf - collect things about Nix/NixOS";

  nixConfig = {
    experimental-features = [
      "ca-derivations"
      "flakes"
      "nix-command"
    ];

    extra-substituters = [
      "https://brsvh.cachix.org"
      "https://hercules-ci.cachix.org"
      "https://nix-community.cachix.org"
      "https://numtide.cachix.org"
    ];

    extra-trusted-public-keys = [
      "brsvh.cachix.org-1:DqtlvqnpP9g39l8Eo74AXRftGx1KJLid/ViADTNgDNE="
      "hercules-ci.cachix.org-1:ZZeDl9Va+xe9j+KqdzoBZMFJHVQ42Uu/c/1/KMC5Lw0="
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
      "numtide.cachix.org-1:2ps1kLBUWjxIneOy1Ik6cQjb41X0iXVXeHigGmycPPE="
    ];
  };

  # Channels
  inputs = {
    nixpkgs = {
      follows = "nixpkgs-unstable";
    };

    nixpkgs-darwin = {
      url = "github:NixOS/nixpkgs/nixpkgs-24.05-darwin";
    };

    nixpkgs-stable = {
      url = "github:NixOS/nixpkgs/nixos-24.05";
    };

    nixpkgs-unstable = {
      url = "github:NixOS/nixpkgs/nixos-unstable";
    };
  };

  # Libraries
  inputs = {
    browser = {
      inputs = {
        flake-utils = {
          follows = "flake-utils";
        };

        nixpkgs = {
          follows = "nixpkgs";
        };

        systems = {
          follows = "x86_64-linux";
        };
      };

      url = "github:nix-community/browser-previews/main";
    };

    darwin = {
      inputs = {
        nixpkgs = {
          follows = "nixpkgs-darwin";
        };
      };

      url = "github:lnl7/nix-darwin/master";
    };

    devshell = {
      inputs = {
        nixpkgs = {
          follows = "nixpkgs";
        };
      };

      url = "github:numtide/devshell/main";
    };

    devshellago = {
      url = "github:brsvh/devshellago/main";
    };

    crane = {
      url = "github:ipetkov/crane/master";
    };

    disko = {
      inputs = {
        nixpkgs = {
          follows = "nixpkgs";
        };
      };

      url = "github:nix-community/disko/master";
    };

    facter = {
      url = "github:numtide/nixos-facter-modules/main";
    };

    flake-compat = {
      url = "github:edolstra/flake-compat/master";
      flake = false;
    };

    flake-parts = {
      inputs = {
        nixpkgs-lib = {
          follows = "nixpkgs";
        };
      };

      url = "github:hercules-ci/flake-parts/main";
    };

    flake-utils = {
      inputs = {
        systems = {
          follows = "default-linux";
        };
      };

      url = "github:numtide/flake-utils/main";
    };

    git-hooks = {
      inputs = {
        flake-compat = {
          follows = "flake-compat";
        };

        gitignore = {
          follows = "gitignore";
        };

        nixpkgs = {
          follows = "nixpkgs";
        };

        nixpkgs-stable = {
          follows = "nixpkgs-stable";
        };
      };

      url = "github:cachix/git-hooks.nix/master";
    };

    gitignore = {
      inputs = {
        nixpkgs = {
          follows = "nixpkgs";
        };
      };

      url = "github:hercules-ci/gitignore.nix/master";
    };

    haumea = {
      url = "github:nix-community/haumea/main";

      inputs = {
        nixpkgs = {
          follows = "nixpkgs";
        };
      };
    };

    home-manager = {
      follows = "home-manager-unstable";
    };

    home-manager-stable = {
      inputs = {
        nixpkgs = {
          follows = "nixpkgs-stable";
        };
      };

      url = "github:nix-community/home-manager/release-24.05";
    };

    home-manager-unstable = {
      inputs = {
        nixpkgs = {
          follows = "nixpkgs-unstable";
        };
      };

      url = "github:nix-community/home-manager/master";
    };

    lanzaboote = {
      inputs = {
        crane = {
          follows = "crane";
        };

        flake-compat = {
          follows = "flake-compat";
        };

        flake-parts = {
          follows = "flake-parts";
        };

        nixpkgs = {
          follows = "nixpkgs";
        };

        pre-commit-hooks-nix = {
          follows = "git-hooks";
        };

        rust-overlay = {
          follows = "rust-overlay";
        };
      };

      url = "github:nix-community/lanzaboote/master";
    };

    nix-alien = {
      inputs = {
        flake-compat = {
          follows = "flake-compat";
        };

        flake-utils = {
          follows = "flake-utils";
        };

        nix-filter = {
          follows = "nix-filter";
        };

        nix-index-database = {
          follows = "nix-index-database";
        };

        nixpkgs = {
          follows = "nixpkgs";
        };
      };

      url = "github:thiagokokada/nix-alien/master";
    };

    nix-filter = {
      url = "github:numtide/nix-filter/main";
    };

    nix-flatpak = {
      url = "github:gmodena/nix-flatpak/v0.4.1";
    };

    nix-index-database = {
      inputs = {
        nixpkgs = {
          follows = "nixpkgs";
        };
      };

      url = "github:nix-community/nix-index-database/main";
    };

    nixago = {
      inputs = {
        flake-utils = {
          follows = "flake-utils";
        };

        nixpkgs = {
          follows = "nixpkgs";
        };

        nixago-exts = {
          follows = "nixago-extensions";
        };
      };

      url = "github:nix-community/nixago/master";
    };

    nixago-extensions = {
      inputs = {
        flake-utils = {
          follows = "nixago/flake-utils";
        };

        nixago = {
          follows = "nixago";
        };

        nixpkgs = {
          follows = "nixago/nixpkgs";
        };
      };

      url = "github:nix-community/nixago-extensions/master";
    };

    oh-my-rime = {
      flake = false;
      url = "github:Mintimate/oh-my-rime/main";
    };

    sops = {
      inputs = {
        nixpkgs = {
          follows = "nixpkgs-unstable";
        };

        nixpkgs-stable = {
          follows = "nixpkgs-stable";
        };
      };

      url = "github:Mic92/sops-nix/master";
    };
  };

  # Overlays
  inputs = {
    rust-overlay = {
      inputs = {
        nixpkgs = {
          follows = "nixpkgs";
        };
      };

      url = "github:oxalica/rust-overlay/master";
    };
  };

  # Systems
  inputs = {
    default-linux = {
      url = "github:nix-systems/default-linux/main";
    };

    x86_64-linux = {
      url = "github:nix-systems/x86_64-linux/main";
    };
  };

  outputs =
    inputs@{
      flake-parts,
      x86_64-linux,
      ...
    }:
    flake-parts.lib.mkFlake
      {
        inherit inputs;
      }
      {
        imports = [
          ./nix/flakeModule.nix
        ];

        systems = import x86_64-linux;
      };
}
