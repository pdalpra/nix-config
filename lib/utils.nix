{ lib, nixpkgs }:

rec {
  filterFiles = test: dir:
    builtins.attrNames
      (lib.attrsets.filterAttrs
        (path: type: test path && type == "regular")
        (builtins.readDir dir)
      );

  listFiles = filterFiles (_: true);

  mergeAll = builtins.foldl' lib.attrsets.recursiveUpdate { };

  isDarwin = nixpkgs.stdenv.isDarwin;

  mkOptional = pred: ifTrue: ifFalse: if (pred) then ifTrue else ifFalse;
}
