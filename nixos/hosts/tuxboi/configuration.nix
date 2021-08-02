{ config, lib, pkgs, ... }:

{
  imports = [
    ../../common.nix
  ];

  networking.hostName = "tuxboi";
  # luks
  boot.initrd.luks.devices = {
    crypted = {
      device = "/dev/disk/by-uuid/d76a7ade-a702-455e-9e21-b5edb5533079";
      preLVM = true;
    };
  };

  hardware.tuxedo-keyboard.enable = true;

  # To Do: Figure out if this will solve the suspend/wake problem: https://askubuntu.com/questions/916465/ubuntu-17-04-keyboard-not-responding-after-suspend
  # services.xserver.displayManager.autoLogin.enable = true;
  # services.xserver.displayManager.autoLogin.user = "kiyengar";

  systemd.user.timers.battery-monitor.enable = true;
}
