#/bin/sh

sudo ln -sf "$PWD/hosts/$HOSTNAME/configuration.nix" /etc/nixos/configuration.nix
# sudo nix-channel --add https://github.com/nix-community/home-manager/archive/release-20.09.tar.gz home-manager
# sudo nix-channel --add https://nixos.org/channels/nixpkgs-unstable nixpkgs-unstable