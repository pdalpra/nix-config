(import
  (
    let
      lock = builtins.fromJSON (builtins.readFile ./flake.lock);
      flakeCompat = lock.nodes.flake-compat;
    in
    fetchTarball {
      url = "https://github.com/edolstra/flake-compat/archive/${flakeCompat.locked.rev}.tar.gz";
      sha256 = flakeCompat.locked.narHash;
    }
  )
  {
    src = ./.;
  }
).shellNix
