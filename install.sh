sudo apt-get install build-essential cmake python3-dev
mkdir ~/.npm-global
npm config set prefix ~/.npm-global
ln -rs .zshrc ~/.zshrc
ln -rs .vimrc ~/.vimrc
mkdir -p ~/.config/nvim
ln -rs .vimrc ~/.config/nvim/init.vim
