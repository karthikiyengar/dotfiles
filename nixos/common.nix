# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, lib, pkgs, ... }:
let
in
{
  imports =
    [
      <home-manager/nixos>
      # Include the results of the hardware scan.
      /etc/nixos/hardware-configuration.nix
      ./multimedia.nix
      ./xorg.nix
      ./dev.nix
      ./fonts.nix
    ];

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  # grub
  boot.loader.grub = {
    enable = true;
    version = 2;
    efiSupport = true;
    useOSProber = true;
    enableCryptodisk = true;
    device = "nodev";
  };

  # NixOS GC and optimization
  nix.gc.automatic = true;
  nix.gc.options = "--delete-older-than 30d";
  nix.autoOptimiseStore = true;

  # Use sandboxing
  nix.useSandbox = true;
  nix = {
    package = pkgs.nixUnstable;
    extraOptions = ''
      experimental-features = nix-command flakes
    '';
  };

  # enable ntfs
  boot.supportedFilesystems = [ "ntfs" ];

  # Set neovim as default editor
  environment.variables.EDITOR = "nvim";
  nixpkgs.overlays = [
    (self: super: {
      rofi = super.rofi.override {
        plugins = [
          pkgs.rofi-emoji
          pkgs.rofi-calc
        ];
      };
      neovim = super.neovim.override
        {
          configure.plug.plugins = with pkgs.vimPlugins; [
            vim-nix
            vim-vinegar
            ale
            vim-fugitive
            vim-airline
            vim-gitgutter
            vim-surround
            fzfWrapper
            fzf-vim
            vim-sleuth
            gruvbox
            coc-nvim
            coc-json
            coc-tsserver
            vim-markdown
            vim-javascript
            typescript-vim
            vim-jsx-typescript
          ];
          viAlias = true;
          vimAlias = true;
          configure.customRC = (builtins.readFile /home/kiyengar/.vimrc);
        };
    })
  ];


  # This takes too long, complies vbox from source
  # virtualisation.virtualbox.host.enable = true;
  # virtualisation.virtualbox.host.enableExtensionPack = true;
  # users.extraGroups.vboxusers.members = [ "kiyengar" ];

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs;
    let
      my-python-packages = python-packages: with python-packages; [
        dbus-python # used by xmonad-log
        recoll
        requests
      ];
      python-with-my-packages = unstable.python3.withPackages my-python-packages;
      appimage-run = pkgs.appimage-run.override {
        extraPkgs = p: [ p.gnome3.libsecret ];
      };
    in
    [
      # Utilities
      xsane
      unstable.flameshot
      unstable.authy
      bitwarden
      gimp
      veracrypt
      gnome.gnome-screenshot
      gnome3.gnome-calculator

      # Communication
      tdesktop
      element-desktop
      signal-desktop
      neochat
      slack
      kmail
      kontact
      korganizer
      gnome.geary
      thunderbird-bin
      unstable.zoom-us
      discord
      hexchat

      # File
      xfce.thunar
      mate.caja
      gnome.file-roller

      # Terminal Emulators
      termite
      haskellPackages.termonad
      rxvt-unicode
      tmux
      alacritty
      ranger

      # Terminal Apps
      ripgrep
      fzf
      unzip
      antigen
      ag
      gitAndTools.gh
      lsof
      thefuck
      trash-cli
      jq
      xorg.xkill
      stow
      xsel
      file
      atop
      htop
      shfmt
      xclip
      ncdu
      zip
      direnv

      # Productivity
      todoist-electron
      freemind
      unstable.joplin
      unstable.joplin-desktop
      zettlr
      obsidian

      # DE/WM
      rofi
      xorg.xev
      unstable.autorandr
      feh
      ant-theme
      arc-theme
      hicolor-icon-theme
      gnome-icon-theme
      tela-icon-theme
      moka-icon-theme
      taffybar
      haskellPackages.status-notifier-item
      arandr
      dunst
      slock
      pavucontrol
      haskellPackages.greenclip
      xidlehook
      xmonad-log
      networkmanagerapplet
      unstable.unipicker
      lxappearance
      appimage-run

      # Hardware
      lshw
      usbutils

      # Unclassified
      home-manager
      sysfsutils
      system-config-printer
      lxmenu-data
      shared_mime_info
      kdeconnect
      recoll
      blueman
      etcher
      gptfdisk
      partition-manager
      parted
      gparted
      nixpkgs-fmt
      gnome3.pomodoro
      brightnessctl
      acpi
      gnome3.gnome-keyring
      python-with-my-packages
      wget
      psmisc
      yarn
      libnotify
      openvpn
      unstable.protonvpn-cli
      unstable.protonvpn-gui
      pasystray
      libreoffice
      coreutils
      mkpasswd
      unstable.nextcloud-client
      lxqt.lxqt-policykit
    ];

  networking.networkmanager.enable = true;

  # Select internationalisation properties.
  i18n.defaultLocale = "en_US.UTF-8";

  # KDE Connect
  networking.firewall.allowedTCPPortRanges = [{ from = 1714; to = 1764; }];
  networking.firewall.allowedUDPPortRanges = [{ from = 1714; to = 1764; }];


  # Set your time zone.
  time.timeZone = "Europe/Berlin";

  # Some other applications
  programs.light.enable = true;
  programs.steam.enable = true;

  # nm-applet
  programs.nm-applet.enable = true;
  # Disable applet autostart
  systemd.user.services.nm-applet.enable = false;

  programs.slock.enable = true;

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  programs.gnupg.agent = {
    enable = true;
    enableSSHSupport = true;
    pinentryFlavor = "gnome3";
  };

  # Android
  programs.adb.enable = true;

  # List services that you want to enable:
  services.fprintd.enable = true;
  services.fwupd.enable = true;

  # Enable the OpenSSH daemon.
  # services.openssh.enable = true;


  # Enable CUPS to print documents.
  services.printing.enable = true;
  services.printing.drivers = [ pkgs.gutenprint ];
  services.avahi.enable = true;
  # Important to resolve .local domains of printers, otherwise you get an error
  # like  "Impossible to connect to XXX.local: Name or service not known"
  services.avahi.nssmdns = true;

  # Enable flatpak
  services.flatpak.enable = true;
  xdg.portal.extraPortals = [ pkgs.xdg-desktop-portal-gtk ];

  # Enable Scanning
  hardware.sane.enable = true;
  hardware.sane.extraBackends = [ pkgs.sane-airscan ];

  # Enable bluetooth
  services.blueman.enable = true;
  hardware.bluetooth.enable = true;

  # USB Automount
  services.gvfs.enable = true;

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.mutableUsers = false;
  users.users.kiyengar = {
    shell = pkgs.zsh;
    uid = 1000;
    home = "/home/kiyengar";
    description = "Karthik Iyengar";
    isNormalUser = true;
    extraGroups = [ "adbusers" "wheel" "scanner" "lp" "lpadmin" "video" "networkmanager" "audio" ]; # Enable ‘sudo’ for the user.
    passwordFile = "/home/kiyengar/dotfiles/nixos/password";
  };

  # Touchpad
  services.xserver.libinput = {
    enable = true;
    mouse = {
      middleEmulation = false;
    };
    touchpad = {
      naturalScrolling = true;
      additionalOptions = ''MatchIsTouchpad "on"'';
    };
  };

  # nixpkgs
  nixpkgs.config = {
    allowUnfree = true;
    packageOverrides = pkgs: {
      unstable = import <nixpkgs-unstable> {
        config = config.nixpkgs.config;
      };
    };
  };

  home-manager.users.kiyengar = import ./home.nix;

  # For taffybar
  systemd.user.services.status-notifier-watcher = {
    enable = true;
    serviceConfig = {
      Type = "simple";
      ExecStart = "${pkgs.haskellPackages.status-notifier-item}/bin/status-notifier-watcher";
    };
    description = "SNI watcher";
    after = [ "graphical-session-pre.target" ];
    partOf = [ "graphical-session.target" ];
    wantedBy = [ "graphical-session.target" ];
  };


  # battery monitor
  systemd.user.services.battery-monitor = {
    path = [ pkgs.bash pkgs.acpi pkgs.libnotify ];
    wantedBy = [ "basic.target" ];
    description = "Notifies when battery is low and suspends";
    serviceConfig = {
      Type = "oneshot";
      ExecStart = "${pkgs.bash}/bin/bash %h/.wm-scripts/battery-monitor.sh";
    };
  };

  systemd.user.timers.battery-monitor = {
    timerConfig = {
      OnUnitInactiveSec = "2s";
      AccuracySec = "1s";
    };
    wantedBy = [ "timers.target" ];
  };

  # Set location provider to geoclue for redshift, maps etc
  location.provider = "geoclue2";

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "20.09"; # Did you read the comment?
}
