{ inputs, config, lib, pkgs, ... }:

{
  programs.dconf.enable = true;

  services.xserver = {
    layout = "us";
    enable = true;
    xkbOptions = "terminate:ctrl_alt_bksp";
    desktopManager = {
      gnome3 = {
        enable = true;
      };
    };

    displayManager = {
      defaultSession = "none+xmonad";
      gdm = {
        enable = true;
      };
      autoLogin = {
        enable = true;
        user = "kiyengar";
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
    enable = true;
    activeOpacity = 1.0;
    inactiveOpacity = 0.8;
    shadow = false;
    backend = "glx";
    fade = true;
    fadeDelta = 5;
    menuOpacity = 1.0;
    opacityRules = [
      "100:class_g = 'i3lock'"
      "100:class_g = 'Rofi'"
      "100:class_g = 'Firefox'"
    ];
    shadowOpacity = 0.75;
  };

  services.autorandr.enable = true;
  environment.extraInit = ''
    xset s off -dpms
  '';
}
