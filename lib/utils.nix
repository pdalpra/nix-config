{ lib }:

rec {
  filterFiles = test: dir:
    builtins.attrNames
      (lib.attrsets.filterAttrs
        (path: type: test path && type == "regular")
        (builtins.readDir dir)
      );

  listFiles = filterFiles (_: true);

  mergeAll = builtins.foldl' lib.attrsets.recursiveUpdate { };
}
