{
  fetchgit,
  lib,
  trivialBuild,
  ...
}:
trivialBuild {
  pname = "typst-ts-mode";

  version = "0.10.0-2024-11-20";

  src = fetchgit {
    url = "https://codeberg.org/meow_king/typst-ts-mode.git";
    rev = "d3e44b5361ed1bbb720a38dafdb29cb8d8b6d8be";
    hash = "sha256-fECXfTjbckgS+kEJ3dMQ7zDotqdxxBt3WFl0sEM60Aw=";
  };

  meta = {
    homepage = "https://codeberg.org/meow_king/typst-ts-mode";
    description = "Typst tree sitter major mode for Emacs";
    license = lib.licenses.gpl3Plus;
    maintainers = with lib.maintainers; [ brsvh ];
  };
}
