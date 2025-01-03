{
  lib,
  ...
}:
let
  inherit (lib.strings)
    hasSuffix
    ;

  directoryFileName = path: if hasSuffix "/" path then path else "${path}/";
in
{
  inherit
    directoryFileName
    ;
}
