{ cell, ... }:
{
  imports = [
    cell.nixosProfiles.firewall
    cell.nixosProfiles.network-manager
    cell.nixosProfiles.systemd-boot
    cell.nixosProfiles.touchpad
    cell.nixosProfiles.zram
    cell.nixosSuites.base
  ];
}