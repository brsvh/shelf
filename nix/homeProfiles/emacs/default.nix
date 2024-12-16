{
  config,
  lib,
  my,
  pkgs,
  ...
}:
let
  inherit (builtins)
    attrValues
    map
    ;

  inherit (lib)
    filterAttrs
    getName
    pipe
    removeSuffix
    ;

  inherit (pkgs)
    linkFarm
    stdenv
    tree-sitter-grammars
    ;

  parinferRustEmacsPath = "${pkgs.parinfer-rust-emacs}/lib";

  treeSitterGrammarsPath = linkFarm "treesit-grammars" (
    map
      (drv: {
        name = "lib${removeSuffix "-grammar" (getName drv)}${stdenv.targetPlatform.extensions.sharedLibrary}";
        path = "${drv}/parser";
      })
      (
        pipe tree-sitter-grammars [
          (filterAttrs (name: _: name != "recurseForDerivations"))
          attrValues
        ]
      )
  );

  tsdkPath = "${pkgs.nodePackages.typescript}/lib/node_modules/typescript/lib/";
in
{
  imports = [
    my.homeModules.emacs
    my.homeProfiles.direnv
    my.homeProfiles.email
    my.homeProfiles.git
    my.homeProfiles.texlive
    my.homeProfiles.tools
  ];

  programs = {
    emacs = {
      dirLocals = ''
        ((emacs-lisp-mode . ((elisp-flymake-byte-compile-load-path . load-path))))
      '';

      enable = true;

      extraDependencies =
        with pkgs;
        [
          editorconfig-checker
          emacs-lsp-booster
          graphviz
          guile
          librime
          lua-language-server
          mailutils
          mermaid-cli
          mitscheme
          multimarkdown
          nil
          nix
          nixfmt-rfc-style
          pyright
          racket-minimal
          rust-analyzer
          rustc
          rustfmt
          stylua
          tinymist
          typst
          typstyle
          vscode-langservers-extracted
          vue-language-server
        ]
        ++ (with haskellPackages; [
          ghc
          haskell-language-server
        ])
        ++ (with llvmPackages; [
          clang
          clang-tools
        ])
        ++ (with nodePackages; [
          eslint
          prettier
          typescript
          typescript-language-server
        ])
        ++ (with python3Packages; [
          black
          grip
          python
        ])
        ++ (with rubyPackages; [
          sass
        ]);

      extraEarlyInitConfig = ''
        ;; Avoid garbage collection during the initialization to achieve a
        ;; faster startup.
        (setq gc-cons-threshold most-positive-fixnum)

        ;; Prevent check mtime of Emacs Lisp Bytecode file to save time.
        (setq load-prefer-newer noninteractive)

        ;; Play the prelude of my-emacs.
        (require 'my-prelude)
      '';

      extraEarlyInitHeader = ''
        ;; Copyright (C) 2022-2024 Burgess Chang

        ;; Author: Burgess Chang <bsc@brsvh.org>
        ;; Keywords: local
        ;; Package-Requires: ((emacs "29.1"))
        ;; Version: 0.1.0
      '';

      extraInitConfig = ''
        (require 'my-prelude)
        (require 'my-lib)
        (require 'my-core)

        

        (setup rime
          (:when-loaded
            (:set
             rime-user-data-dir "${config.rime.dataDirectory}")))

        

        (require 'my-browser)
        (require 'my-comint)
        (require 'my-dired)
        (require 'my-editor)
        (require 'my-erc)
        (require 'my-mule)
        (require 'my-project)
        (require 'my-security)
        (require 'my-terminal)
        (require 'my-ui)
        (require 'my-workflow)
        (require 'my-workspace)

        

        (setup mermaid-mode
          (:set
           mermaid-mmdc-location "${pkgs.mermaid-cli}/bin/mmdc"))

        (setup ob-mermaid
          (:set
           ob-mermaid-cli-path "${pkgs.mermaid-cli}/bin/mmdc"))

        (setup parinfer-rust
          (:set
           parinfer-rust-auto-download nil
           parinfer-rust-library "${parinferRustEmacsPath}/libparinfer_rust.so"
           parinfer-rust-library-directory "${parinferRustEmacsPath}/lib/"))

        (setup treesit-grammars
          (:snoc
           treesit-extra-load-path "${treeSitterGrammarsPath}"))

        (setup my-programming-web
          (:when-loaded
            (:set
             my-vue-language-server-options
             '( "vue-language-server"
                "--stdio"
                :initializationOptions ( :typescript (:tsdk "${tsdkPath}"))))))

        

        (require 'my-drawing)
        (require 'my-programming)
        (require 'my-writing)

        

        (require 'my-postlude)
      '';

      extraInitHeader = ''
        ;; Copyright (C) 2022-2024 Burgess Chang

        ;; Author: Burgess Chang <bsc@brsvh.org>
        ;; Keywords: local
        ;; Package-Requires: ((emacs "29.1"))
        ;; Version: 0.1.0
      '';

      extraPackages =
        epkgs: with epkgs; [
          my-emacs
        ];

      package = pkgs.emacs-gtk3;

      isDefaultEditor = true;
    };
  };

  xdg = {
    configFile = {
      "emacs/authinfo.gpg" = {
        source = my.root + "/etc/authinfo/authinfo.gpg";
      };

      "emacs/eshell" = {
        recursive = true;
        source = my.root + "/etc/eshell";
      };

      "emacs/snippets" = {
        recursive = true;
        source = my.root + "/etc/snippets";
      };
    };

    mimeApps = {
      defaultApplications = {
        "application/pdf" = "emacsclient.desktop";
        "application/x-shellscript" = "emacsclient.desktop";
        "text/english" = "emacsclient.desktop";
        "text/plain" = "emacsclient.desktop";
        "text/x-c" = "emacsclient.desktop";
        "text/x-c++" = "emacsclient.desktop";
        "text/x-c++hdr" = "emacsclient.desktop";
        "text/x-c++src" = "emacsclient.desktop";
        "text/x-chdr" = "emacsclient.desktop";
        "text/x-csrc" = "emacsclient.desktop";
        "text/x-java" = "emacsclient.desktop";
        "text/x-makefile" = "emacsclient.desktop";
        "text/x-moc" = "emacsclient.desktop";
        "text/x-pascal" = "emacsclient.desktop";
        "text/x-tcl" = "emacsclient.desktop";
        "text/x-tex" = "emacsclient.desktop";
        "x-scheme-handler/mailto" = "emacsclient-mail.desktop";
        "x-scheme-handler/org-protocol" = "emacsclient.desktop";
      };
    };
  };
}
