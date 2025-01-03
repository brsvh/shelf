{
  fetchgit,
  lib,
  slime,
  trivialBuild,
  ...
}:
let
  slime-doc-contribs = trivialBuild rec {
    pname = "slime-doc-contribs";

    version = "0.1-2025-01-02";

    src = fetchgit {
      url = "https://github.com/mmontone/slime-doc-contribs";
      rev = "f82e02281c3ee0f5b1b6f56e8db98223d034bf3c";
      hash = "sha256-4vMOi7GLQFCXQ5sRmsn8m6U3//vYozC+65HnU3Bx5LQ=";
    };

    meta = {
      homepage = "https://github.com/mmontone/slime-doc-contribs";
      description = "Documentation contribs for SLIME";
      license = lib.licenses.gpl3Plus;
      maintainers = with lib.maintainers; [ brsvh ];
    };

    buildInputs = propagatedUserEnvPkgs;

    propagatedUserEnvPkgs = [
      slime
    ];

    postInstall = ''
      install *.lisp $LISPDIR
    '';
  };
in
slime-doc-contribs.overrideAttrs (
  finalAttrs: prevAttrs: {
    postInstall =
      (prevAttrs.postInstall or "")
      + ''
        install *.lisp $LISPDIR
      '';
  }
)
