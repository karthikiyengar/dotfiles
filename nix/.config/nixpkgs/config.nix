{
	allowUnfree = true;
  packageOverrides = pkgs: with pkgs; {
    myPackages = pkgs.buildEnv {
      name = "my-packages";
      paths = [
				flameshot
				rofi
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
				thefuck
				oh-my-zsh
				zsh
				antigen
				fzf
				fzf-zsh
				haskellPackages.xmonad-contrib
        haskellPackages.xmonad-extras
        haskellPackages.xmonad
			];
    };
  };
}

