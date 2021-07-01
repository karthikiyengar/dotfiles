{ inputs, config, lib, pkgs, ... }:

{
  programs.dconf.enable = true;

  services.xserver = {
    layout = "us";
    enable = true;
    xkbOptions = "terminate:ctrl_alt_bksp";
    desktopManager = {
      plasma5 = {
        enable = true;
      };
    };

    displayManager = {
      defaultSession = "none+xmonad";
      sddm = {
        enable = true;
      };
    };
    windowManager = {
      xmonad = {
        enable = true;
        enableContribAndExtras = true;
        extraPackages = haskellPackages: [
          haskellPackages.dbus
          haskellPackages.List
          haskellPackages.monad-logger
          haskellPackages.xmonad
        ];
      };
    };
  };

  # Compositing
  services.picom = {
    enable = false;
    backend = "glx";
  };

  services.autorandr.enable = true;
  environment.extraInit = ''
    xset s off -dpms
  '';
}
