# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:
let
in
{
  imports =
    [
      <home-manager/nixos>
      # Include the results of the hardware scan.
      ./hardware-configuration.nix
      ./multimedia.nix
      ./xorg.nix
      ./dev.nix
      ./tuxedo.nix
    ];

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  # Kernel
  boot.kernelPackages = pkgs.linuxPackages_latest;


  # NixOS GC and optimization
  nix.gc.automatic = true;
  nix.gc.options = "--delete-older-than 30d";
  nix.autoOptimiseStore = true;

  # Use sandboxing
  nix.useSandbox = true;

  # grub
  boot.loader.grub = {
    enable = true;
    version = 2;
    efiSupport = true;
    useOSProber = true;
    enableCryptodisk = true;
    device = "nodev";
  };

  # enable ntfs
  boot.supportedFilesystems = [ "ntfs" ];

  # luks
  boot.initrd.luks.devices = {
    crypted = {
      device = "/dev/disk/by-uuid/d76a7ade-a702-455e-9e21-b5edb5533079";
      preLVM = true;
    };
  };

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
  # To Package: rumno, unipicker, rofi stuff
  environment.systemPackages = with pkgs;
    let
      my-python-packages = python-packages: with python-packages; [
        dbus-python # used by xmonad-log
        recoll
        pulsectl # used by xob script
        watchdog # used by xob script 
        requests
      ];

      python-with-my-packages = unstable.python3.withPackages my-python-packages;
    in
    [
      # Browsers
      google-chrome
      chromium
      unstable.firefox
      torrential

      # Utilities
      xsane
      flameshot
      unstable.authy
      bitwarden
      gimp

      veracrypt
      gnome3.gnome-calculator

      # Communication
      tdesktop
      element-desktop
      signal-desktop
      slack
      thunderbird
      unstable.zoom-us
      discord
      hexchat
      weechat

      # File Managers
      xfce.thunar
      mate.caja

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
      fzf-zsh
      antigen
      oh-my-zsh
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
      # todoist-electron
      todoist
      freemind
      unstable.joplin
      unstable.joplin-desktop

      # DE/WM
      rofi
      feh
      ant-theme
      arc-theme
      polybar
      arandr
      i3lock
      dunst
      slock
      xob
      pavucontrol
      haskellPackages.greenclip
      xidlehook
      xmonad-log
      unstable.unipicker
      lxappearance
      redshift
      gnomeExtensions.appindicator

      # Unclassified
      ibus-engines.typing-booster
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
      gnome3.cheese
      python-with-my-packages
      wget
      psmisc
      networkmanagerapplet
      unstable.foliate
      yarn
      libnotify
      openvpn
      protonvpn-cli
      pasystray
      unstable.autorandr
      postman
      libreoffice
      bat
      coreutils
      mkpasswd
      nextcloud-client
      lxqt.lxqt-policykit
    ];


  networking.hostName = "kiyengar-nixos";
  networking.networkmanager.enable = true;

  # Select internationalisation properties.
  i18n.defaultLocale = "en_US.UTF-8";

  # Activate typing-booster (for emoji picker)
  i18n.inputMethod = {
    enabled = "ibus";
    ibus.engines = with pkgs.ibus-engines; [ typing-booster ];
  };

  # KDE Connect
  networking.firewall.allowedTCPPortRanges = [{ from = 1714; to = 1764; }];
  networking.firewall.allowedUDPPortRanges = [{ from = 1714; to = 1764; }];


  # Set your time zone.
  time.timeZone = "Europe/Berlin";

  # Some other applications
  programs.light.enable = true;
  programs.steam.enable = true;
  programs.nm-applet.enable = true;
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

  # Gnome
  services.udev.packages = with pkgs; [ gnome3.gnome-settings-daemon ];

  # List services that you want to enable:
  services.fprintd.enable = true;
  services.fwupd.enable = true;

  # Enable the OpenSSH daemon.
  # services.openssh.enable = true;

  # TLP for battery
  services.tlp.enable = true;

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

  # Tuxedo
  hardware.tuxedo-keyboard.enable = true;

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.mutableUsers = false;
  users.users.kiyengar = {
    shell = pkgs.zsh;
    uid = 1000;
    home = "/home/kiyengar";
    description = "Karthik Iyengar";
    isNormalUser = true;
    extraGroups = [ "adbusers" "wheel" "scanner" "lp" "video" "networkmanager" "audio" ]; # Enable ‘sudo’ for the user.
    hashedPassword = "***REMOVED***";
  };

  programs.zsh = {
    enable = true;
    autosuggestions.enable = true;
    ohMyZsh.enable = true;
    ohMyZsh.theme = "frisk";
    ohMyZsh.plugins = [ "git" "sudo" "docker" "kubectl" ];
    syntaxHighlighting.enable = true;
  };

  # Touchpad
  services.xserver.libinput = {
    enable = true;

    naturalScrolling = true;
    additionalOptions = ''MatchIsTouchpad "on"'';
    # 21.05
    # touchpad = {
    #   naturalScrolling = true;
    #   additionalOptions = ''MatchIsTouchpad "on"'';
    # };
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
    enable = true;
    timerConfig = {
      OnUnitInactiveSec = "2s";
      AccuracySec = "1s";
    };
    wantedBy = [ "timers.target" ];
  };

  # Fonts
  fonts.fontconfig = {
    # localConf = builtins.readFile /home/kiyengar/fontconfig.xml;
    defaultFonts = {
      emoji = [ "Noto Color Emoji" ];
      serif = [ "Bitstream Vera Serif" ];
      sansSerif = [ "Bitstream Vera Sans" ];
      monospace = [ "Bitstream Vera Sans Mono" ];
    };
  };

  # fonts.enableDefaultFonts = true;
  fonts.fonts = with pkgs; [
    noto-fonts
    noto-fonts-emoji
    ttf_bitstream_vera
    hasklig
    unifont
    font-awesome_4
    symbola
    fira-code
    fira-code-symbols
    unifont
  ];

  # Set location provider to geoclue for redshift, maps etc
  location.provider = "geoclue2";

  # Home Manager 
  home-manager.useGlobalPkgs = true;
  home-manager.users.kiyengar = { pkgs, ... }: {
    programs.firefox = {
      enable = true;
      profiles = {
        myprofile = {
          settings = {
            "ui.context_menus.after_mouseup" = true; # xmonad right click bug
          };
        };
      };
    };

    programs.git = {
      enable = true;
      userName = "Karthik Iyengar";
      userEmail = "hello@kiyengar.net";
    };

  };

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "20.03"; # Did you read the comment?
}
