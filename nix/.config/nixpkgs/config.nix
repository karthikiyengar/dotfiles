{
	allowUnfree = true;
  packageOverrides = pkgs: with pkgs; {
    myPackages = pkgs.buildEnv {
      name = "my-packages";
      paths = [
				flameshot
				rofi
				dunst
				haskellPackages.greenclip
				polybar
				xidlehook
				jetbrains.idea-ultimate
				neovim
				nodejs-10_x
				stow
				vscode
				git
				rofi-calc
				stack
				cabal-install
				thefuck
				oh-my-zsh
				zsh
				docker
				docker-compose
				antigen
				fzf
				fzf-zsh
				direnv
				thunderbird
				haskellPackages.xmonad-contrib
				haskellPackages.xmonad-extras
				haskellPackages.xmonad
			];
    };
  };
}

