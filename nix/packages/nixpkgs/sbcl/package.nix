{
  callPackage,
  wrapLisp,
  ...
}:
wrapLisp {
  faslExt = "fasl";

  flags = [
    "--dynamic-space-size"
    "3000"
  ];

  pkg = callPackage ./sbcl.nix {
    version = "2.4.10";
  };
}
