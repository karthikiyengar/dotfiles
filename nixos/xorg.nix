{ inputs, config, lib, pkgs, ... }:

{
  programs.dconf.enable = true;
  programs.ssh.askPassword = pkgs.lib.mkForce "${pkgs.ksshaskpass.out}/bin/ksshaskpass"; # https://github.com/NixOS/nixpkgs/issues/75867

  services.xserver = {
    layout = "us";
    enable = true;
    xkbOptions = "terminate:ctrl_alt_bksp";
    desktopManager = {
      gnome = {
        enable = true;
      };
      plasma5 = {
        enable = false;
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
          haskellPackages.taffybar
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
