final: prev:
let
  ignoreCompilationError =
    pkg:
    pkg.overrideAttrs (
      finalAttrs: previousAttrs: {
        ignoreCompilationError = true;
      }
    );
in
{
  emacsPackagesFor =
    emacs:
    let
      inherit (prev)
        emacsPackagesFor
        fetchpatch
        ;

      inherit (prev.lib)
        packagesFromDirectoryRecursive
        ;

      scope = emacsPackagesFor emacs;

      epkgs =
        finalEpkgs: prevEpkgs:
        prevEpkgs.override {
          elpaPackages = prevEpkgs.elpaPackages // {
            setup = ignoreCompilationError prevEpkgs.elpaPackages.setup;
          };

          melpaPackages = prevEpkgs.melpaPackages // {
            slime = prevEpkgs.melpaPackages.slime.overrideAttrs (
              finalAttrs: prevAttrs: {
                patches = prevAttrs.patches or [ ] ++ [
                  (fetchpatch {
                    name = "0001-xdgify-slime-home.patch";
                    url = "https://github.com/brsvh/slime/commit/4bd27a3832dd31aca835d58a6215b3d29b2ab2d0.patch";
                    hash = "sha256-IQhq48iAe8nknC7hTQI3LdJBZI5oQr12LccPAfcu12g=";
                  })
                ];
              }
            );
          };

          manualPackages =
            prevEpkgs.manualPackages
            // packagesFromDirectoryRecursive {
              inherit (finalEpkgs)
                callPackage
                ;

              directory = ../../packages/emacs/manualPackages;
            };
        };
    in
    scope.overrideScope epkgs;
}
