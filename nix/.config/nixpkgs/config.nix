{
  allowUnfree = true;
  packageOverrides = pkgs: with pkgs; {
    myPackages = pkgs.buildEnv {
      name = "my-packages";
      paths = [
				flameshot
				rofi
				dunst
				awscli
				haskellPackages.greenclip
				feh
				slack
				spotify
				polybar
				glibc
				firefox
				xidlehook
				pavucontrol
				jetbrains.idea-ultimate
				neovim
				stow
				vscode-with-extensions
				git
				git-lfs
				rofi-calc
				stack
				pkg-config
				librsvg
				cairo
				cabal-install
				jq
				thefuck
				oh-my-zsh
				zsh
				antigen
				fzf
				fzf-zsh
				direnv
				jdk
				android-studio
				maven
				haskellPackages.xmonad-contrib
				haskellPackages.xmonad-extras
				haskellPackages.xmonad
			];
    };
  };
}

