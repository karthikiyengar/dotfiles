let mozilla = import (builtins.fetchGit {
  url = "https://github.com/mozilla/nixpkgs-mozilla.git";
  ref = "master";
}); in

{ config, pkgs, ... }:
{
  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;
  nixpkgs.config.allowUnfree = true;
  home.username = "kiyengar";
  home.homeDirectory = "/home/kiyengar";

  services.lorri.enable = true;

  nixpkgs.overlays = [
    mozilla
    (self: super: {
      latest = {
        firefox-nightly-bin = super.latest.firefox-nightly-bin;
      };
    })
  ];

  home.packages = with pkgs; [
    atop
    latest.firefox-nightly-bin
    google-chrome
    chromium
    torrential
    qbittorrent
    neochat
  ];

  programs.vscode = {
    enable = true;
  };

  programs.firefox = {
    enable = false;
    profiles = {
      myprofile = {
        settings = {
          "ui.context_menus.after_mouseup" = true; # xmonad right click bug
        };
      };
    };
  };

  programs.zsh = {
    enable = true;
    autocd = true;
    enableAutosuggestions = true;
    enableCompletion = true;
    history = {
      size = 999999999;
      save = 999999999;
    };

    initExtra = ''
      source ~/.p10k.zsh

      # Fix shitty vi mode bindings for Home/Ins/Del/Ctrl+Arrow
      bindkey "^[[1;5C" forward-word
      bindkey "^[[1;5D" backward-word
      bindkey  "^[[H"   beginning-of-line
      bindkey  "^[[F"   end-of-line
      bindkey  "^[[3~"  delete-char

      eval $(thefuck --alias f) # Enable thefuck
      eval "$(direnv hook zsh)" # Enable direnv
        
      export PATH="$PATH:$HOME/.cabal/bin:$HOME/.yarn/bin:$HOME/.config/yarn/global/node_modules/.bin:$HOME/.deno/bin:$HOME/.npm-global/bin:$HOME/.cargo/bin"
    '';

    shellAliases = {
      vim = "nvim";
      ls = "ls -lah --color";
      vz = "vim ~/.zshrc";
      sz = "source ~/.zshrc";
      r = "sudo nixos-rebuild switch";
      wp = "~/.wm-scripts/change-wallpaper.sh";
      nclean = "find . -name \" node_modules \" -exec rm -rf '{}' +";

      # Git
      gup = "git fetch --all; git rebase origin/master";
      ga = "git add --all";
      gc = "git commit";
      gst = "git status";
      gd = "git diff --staged";

      # NixOS 
      nixgc = "sudo nix-collect-garbage -d; nix-collect-garbage -d; sudo nix-store --optimize";
      u = "sudo nix-channel --update; sudo nixos-rebuild switch --upgrade";
      vn = "code ~/dotfiles/nixos";

      # Dev Folders
      dev = "cd ~/development";
      crocks = "cd ~/development/crocks";
      mapi = "cd ~/development/lyra-api";
      dotf = "code ~/dotfiles";
      vx = "code ~/.xmonad/";

      # VPN
      vpn = "sudo protonvpn c -f";
      vpnin = "sudo protonvpn c --cc IN";
      vpnde = "sudo protonvpn c --cc DE";
      vpnd = "sudo protonvpn d";
      vpns = "sudo protonvpn s";
    };

    zplug = {
      enable = true;
      plugins = [
        { name = "romkatv/powerlevel10k"; tags = [ as:theme depth:1 ]; }
        { name = "Aloxaf/fzf-tab"; }
        { name = "agkozak/zsh-z"; }
        { name = "kutsan/zsh-system-clipboard"; }
      ];
    };
  };

  programs.git = {
    enable = true;
    userName = "Karthik Iyengar";
    userEmail = "hello@kiyengar.net";
  };

  programs.fzf = {
    enable = true;
    enableBashIntegration = true;
    enableZshIntegration = true;
  };

  # This value determines the Home Manager release that your
  # configuration is compatible with. This helps avoid breakage
  # when a new Home Manager release introduces backwards
  # incompatible changes.
  #
  # You can update Home Manager without changing this value. See
  # the Home Manager release notes for a list of state version
  # changes in each release.
  home.stateVersion = "20.03";
}
