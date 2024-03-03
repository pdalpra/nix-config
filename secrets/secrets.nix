let
  pdalpra = (import ../keys/pdalpra.nix).sshKeys;
  vm = (import ../keys/vm.nix).sshKeys;
in
{
  "pdalpra.age".publicKeys = pdalpra ++ vm;
  "root.age".publicKeys = pdalpra ++ vm;
}
