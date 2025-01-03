{
  lib,
  ...
}:
let
  inherit (lib)
    makeExtensible
    ;

  shelf-lib = makeExtensible (
    final:
    let
      callLibs =
        file:
        import file {
          inherit
            lib
            ;

          shelf-lib = final;
        };
    in
    {
      collectors = callLibs ./collectors.nix;

      filesystem = callLibs ./filesystem.nix;

      importers = callLibs ./importers.nix;
    }
  );
in
shelf-lib
