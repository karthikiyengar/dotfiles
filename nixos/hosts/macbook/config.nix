{ config, lib, pkgs, ... }:

{
  # Wi-fi USB Dongle - TP-Link Archer T2U Nano 
  boot.extraModulePackages = [ config.boot.kernelPackages.rtl88xxau-aircrack ];

  networking.hostName = "kiyengar-macbook";
  services.xserver.displayManager.autoLogin.enable = false;
  systemd.user.timers.battery-monitor.enable = false; # Macbook reports bad ACPI information and leads to suspends

  powerManagement.cpuFreqGovernor = lib.mkForce "performance";
}
