{
  config,
  my,
  pkgs,
  ...
}:
let
  epkgs = pkgs.emacsPackagesFor config.programs.emacs.package;

  slime =
    let
      pkg = epkgs.slime;
    in
    # MELPA package.
    "${pkg}/share/emacs/site-lisp/elpa/${pkg.pname}-${pkg.version}";

  slime-doc-contribs =
    let
      pkg = epkgs.slime-doc-contribs;
    in
    # Package build by Trivial Builder.
    "${pkg}/share/emacs/site-lisp";
in
{
  imports = [
    my.homeModules.sbcl
  ];

  programs = {
    sbcl = {
      config = ''
        (require :asdf)

        (push #p"${slime}" asdf:*central-registry*)

        (require :swank)

        (push #p"${slime-doc-contribs}/" swank::*load-path*)
      '';

      enable = true;
    };
  };
}
