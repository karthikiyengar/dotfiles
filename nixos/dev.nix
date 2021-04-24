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
    ];
}
