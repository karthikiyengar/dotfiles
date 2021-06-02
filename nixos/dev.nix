{ inputs, config, lib, pkgs, ... }:

{
  virtualisation.docker.enable = true;
  users.users.kiyengar.extraGroups = [ "docker" ];

  environment.systemPackages = with pkgs;
    [
      # Development 
      unstable.robo3t
      unstable.mongodb-compass
      jetbrains.idea-ultimate
      docker
      docker-compose
      nodejs
      stack
      rustup
      gcc
      openjdk11
      heroku
      gitAndTools.tig
      git
      haskellPackages.ghc

      # Editors
      unstable.vscode-with-extensions
      gnvim
      texstudio
      texlive.combined.scheme-full
      neovim
      glade
    ];

    home-manager.users.kiyengar = { pkgs, ... }: {
      programs.git = {
        enable = true;
        userName = "Karthik Iyengar";
        userEmail = "hello@kiyengar.net";
      };
    };
}
