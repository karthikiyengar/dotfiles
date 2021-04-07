# Edit this configuration file to efine what should be installed on
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

  # For xbox controller
  hardware.xpadneo.enable = true;
  boot.extraModprobeConfig = ''options bluetooth disable_ertm=1'';
  hardware.bluetooth.config = {
    General = {
      Privacy = "device";
    };
  };

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



  virtualisation.docker.enable = true;

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
        pulsectl
        requests
      ];

      python-with-my-packages = python3.withPackages my-python-packages;
    in
    [
      # Development 
      unstable.robo3t
      unstable.mongodb-compass
      jetbrains.idea-community
      docker
      docker-compose
      nodejs
      stack
      cargo
      openjdk11
      heroku
      gitAndTools.tig
      git
      haskellPackages.ghc

      # Browsers
      google-chrome
      chromium
      firefox

      # Utilities
      xsane
      flameshot
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
      (st.overrideAttrs (oldAttrs: rec {
        patches = [
          # Fetch them directly from `st.suckless.org`
          (fetchpatch {
            url = "https://st.suckless.org/patches/scrollback/st-scrollback-20201205-4ef0cbd.diff";
            sha256 = "0p3pdk9sb92kllzpcgzkfghxfvxbrr2d4cd36xz76jibr57a6wr4";
          })
          (fetchpatch {
            url = "https://st.suckless.org/patches/scrollback/st-scrollback-mouse-20191024-a2c479c.diff";
            sha256 = "0qg20sv64im5lcnfnphnbbiyizwywrg1g6zhxyxqqyf8g33lpbb7";
          })
          (fetchpatch {
            url = "https://st.suckless.org/patches/gruvbox/st-gruvbox-dark-0.8.2.diff";
            sha256 = "14ajygrlz4z3p0w90cbdc7xk2wikhys4m761ci3ln7p16n48qxdz";
          })
        ];
      }))
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
      shfmt
      xclip
      ncdu
      zip
      direnv

      # Productivity
      todoist-electron
      freemind
      unstable.joplin-desktop

      # DE/WM
      rofi
      feh
      polybar
      stalonetray
      xmobar
      arandr
      dunst
      pavucontrol
      i3lock
      haskellPackages.greenclip
      xidlehook
      xmonad-log
      unstable.unipicker
      lxappearance

      # Editors
      vscode-with-extensions
      gnvim
      texstudio
      texlive.combined.scheme-full
      neovim
      vim_configurable

      # Multimedia
      audacity
      gthumb
      imagemagick
      simplescreenrecorder
      vlc
      spotify

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
      kdeApplications.ark
      kdeApplications.kfind
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

  # Activate typing-booster (for emoji picker)
  i18n.inputMethod = {
    enabled = "ibus";
    ibus.engines = with pkgs.ibus-engines; [ typing-booster ];
  };

  # KDE Connect
  networking.firewall.allowedTCPPortRanges = [{ from = 1714; to = 1764; }];
  networking.firewall.allowedUDPPortRanges = [{ from = 1714; to = 1764; }];


  # Set your time zone.
  time.timeZone = "Asia/Kolkata";

  # Sway!
  programs.sway = {
    enable = true;
    wrapperFeatures.gtk = true; # so that gtk works properly
    extraPackages = with pkgs; [
      swaylock
      swayidle
      wl-clipboard
      mako # notification daemon
      alacritty # Alacritty is the default terminal in the config
      dmenu # Dmenu is the default in the config but i recommend wofi since its wayland native
    ];
  };

  programs.light.enable = true;
  programs.steam.enable = true;
  programs.nm-applet.enable = true;
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

  # Enable the OpenSSH daemon.
  # services.openssh.enable = true;

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;


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

  # Compositing
  services.picom = {
    enable = true;
    activeOpacity = 1.0;
    inactiveOpacity = 0.8;
    backend = "glx";
    fade = true;
    fadeDelta = 5;
    shadow = true;
    opacityRules = [
      "100:class_g = 'i3lock'"
      "100:class_g = 'Rofi'"
    ];
    shadowOpacity = 0.75;
  };

  # Enable Scanning
  hardware.sane.enable = true;
  hardware.sane.extraBackends = [ pkgs.sane-airscan ];

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

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.mutableUsers = false;
  users.users.kiyengar = {
    shell = pkgs.zsh;
    uid = 1000;
    home = "/home/kiyengar";
    description = "Karthik Iyengar";
    isNormalUser = true;
    extraGroups = [ "adbusers" "wheel" "scanner" "lp" "docker" "video" "networkmanager" "audio" ]; # Enable ‘sudo’ for the user.
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
    localConf = builtins.readFile /home/kiyengar/fontconfig.xml;
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
