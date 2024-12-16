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
        writeText
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
            org-roam = prevEpkgs.melpaPackages.org-roam.overrideAttrs (
              finalAttrs: prevAttrs: {
                recipe = writeText "org-roam" ''
                  (org-roam :fetcher github
                            :repo "tarsiiformes/org-roam"
                            :branch "sqlite-open"
                            :files (:defaults "extensions/*"))
                '';
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
