# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:
let
  unstable = import <nixos-unstable> { config = { allowUnfree = true; }; };
  my-python-packages = python-packages: with python-packages; [
    pandas
    requests
  ];
in
{
  imports =
    [
      # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  # grub
  boot.loader.grub = {
    enable = true;
    version = 2;
    efiSupport = true;
    enableCryptodisk = true;
    device = "nodev";
  };

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
      neovim = super.neovim.override {
        viAlias = true;
        vimAlias = true;
      };
    })
  ];

  virtualisation.docker.enable = true;

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  # To Package: rumno, unipicker, rofi stuff
  environment.systemPackages = with pkgs;
    let
      my-python-packages = python-packages: with python-packages; [
        dbus-python
        requests
      ];
      python-with-my-packages = python3.withPackages my-python-packages;
    in
    [
      # unstable.obinskit
      unstable.robo3t
      unstable.mongodb-compass
      flameshot
      freemind
      pcmanfm
      kdeconnect
      heroku
      imagemagick
      simplescreenrecorder
      vlc
      stack
      blueman
      docker
      docker-compose
      thefuck
      i3lock
      acpi
      gnome3.gnome-keyring
      gnome3.cheese
      python-with-my-packages
      wget
      psmisc
      networkmanagerapplet
      nodejs
      yarn
      libnotify
      xsel
      xclip
      stow
      vscode-with-extensions
      haskellPackages.termonad
      rxvt-unicode
      st
      xidlehook
      pasystray
      unstable.autorandr
      postman
      arandr
      neovim
      vim_configurable
      libreoffice
      firefox
      direnv
      fzf
      fzf-zsh
      antigen
      oh-my-zsh
      haskellPackages.greenclip
      coreutils
      google-chrome
      chromium
      git
      unstable.joplin-desktop
      mkpasswd
      nextcloud-client
      dunst
      rofi
      feh
      jq
      xorg.xkill
      slack
      pavucontrol
      atop
      stalonetray
      xmobar
      gnome3.geary
      unstable.mailspring
      unstable.zoom-us
      spotify
      (
        with import <nixpkgs> { };

        vim_configurable.customize {
          name = "vim";
          vimrcConfig.customRC = ''set number'';
          vimrcConfig.plug.plugins = with pkgs.vimPlugins; [ vim-addon-nix vim-sleuth YouCompleteMe ];
        }
      )
      lxqt.lxqt-policykit
      kdeApplications.ark
      kdeApplications.okular
    ];


  # networking.hostName = "nixos"; # Define your hostname.
  networking.networkmanager.enable = true;
  # The global useDHCP flag is deprecated, therefore explicitly set to false here.
  # Per-interface useDHCP will be mandatory in the future, so this generated config
  # replicates the default behaviour.
  # networking.useDHCP = false;
  # networking.interfaces.enp59s0f1.useDHCP = true;
  # networking.interfaces.wlp64s0.useDHCP = true;

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Select internationalisation properties.
  # i18n.defaultLocale = "en_US.UTF-8";
  # console = {
  #   font = "Lat2-Terminus16";
  #   keyMap = "us";
  # };

  # KDE Connect
  networking.firewall.allowedTCPPortRanges = [{ from = 1714; to = 1764; }];
  networking.firewall.allowedUDPPortRanges = [{ from = 1714; to = 1764; }];


  # Set your time zone.
  time.timeZone = "Europe/Berlin";


  programs.light.enable = true;
  programs.nm-applet.enable = true;
  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  programs.gnupg.agent = {
    enable = true;
    enableSSHSupport = true;
    pinentryFlavor = "gnome3";
  };

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  # services.openssh.enable = true;

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  # Enable CUPS to print documents.
  # services.printing.enable = true;

  # Enable urxvtd
  services.urxvtd.enable = true;

  # Enable sound.
  sound.enable = true;
  hardware.pulseaudio.enable = true;
  hardware.pulseaudio.package = pkgs.pulseaudioFull;

  # Enable bluetooth
  services.blueman.enable = true;
  hardware.bluetooth.enable = true;

  # USB Automount
  services.gvfs.enable = true;

  # Tuxedo
  hardware.tuxedo-keyboard.enable = true;

  # Enable the X11 windowing system.
  services.xserver.layout = "us";
  # services.xserver.xkbOptions = "eurosign:e";

  # Enable XMonad Desktop Environment.
  services.autorandr.enable = true;
  environment.extraInit = ''
    xset s off -dpms
  '';
  services.xserver = {
    enable = true;
    xkbOptions = "terminate:ctrl_alt_bksp";
    displayManager = {
      defaultSession = "none+xmonad";
      lightdm = {
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
          haskellPackages.xmonad-contrib
          haskellPackages.xmonad-extras
          haskellPackages.xmonad
        ];
      };
    };
  };

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.mutableUsers = false;
  users.users.kiyengar = {
    shell = pkgs.zsh;
    uid = 1000;
    home = "/home/kiyengar";
    description = "Karthik Iyengar";
    isNormalUser = true;
    extraGroups = [ "wheel" "docker" "video" "networkmanager" "audio" ]; # Enable ‘sudo’ for the user.
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
  };

  # nixpkgs
  nixpkgs.config.allowUnfree = true;
  nixpkgs.config.permittedInsecurePackages = [
    "electron-3.1.13"
  ];

  # battery monitor
  systemd.user.services.batteryMonitor = {
    path = [ pkgs.bash pkgs.acpi pkgs.libnotify ];
    wantedBy = [ "multi-user.target" ];
    description = "Notifies when battery is low and suspends";
    serviceConfig = {
      Type = "simple";
      ExecStart = "${pkgs.bash}/bin/bash %h/.wm-scripts/battery-monitor.sh";
    };
  };

  systemd.user.timers.batteryMonitor = {
    timerConfig = {
      OnUnitInactiveSec = "2s";
      AccuracySec = "1s";
    };
    wantedBy = [ "timers.target" ];
  };

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "20.03"; # Did you read the comment?

}
