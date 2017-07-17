#!/bin/sh
PKGS="xmonad stow rofi feh compton rxvt-unicode-256color i3lock cabal-install"
HPKG="yaml"
DIRS="bin xmonad compton fonts x"
sudo apt-get install $PKGS
cabal update
cabal install $HPKG
cd ~/xmonad && stow -S $DIRS
cd ~/bin/src && make

if [ ! -f ~/.env.yml ]; then
echo "~/.env.yml does not exist. Creating it"
echo "Sign up for an unsplash api key to set new backgrounds on login"
echo "https://unsplash.com/join"
echo "https://unsplash.com/oauth/applications/new"
echo -n "unsplash key (Application ID): "
read UKEY

echo 'alias lockscr="golock -s 23 && i3lock -u -i ~/.lock.png"' >> ~/.bashrc
echo 'export PATH="$PATH:$HOME/bin"' >> ~/.bashrc
echo "unsplash_key : $UKEY" >> ~/.env.yml
echo 'bg_location: $HOME/.bg' >> ~/.env.yml
echo 'bg_arg: --bg-fill' >> ~/.env.yml
echo 'bg_arg1: --no-xinerama' >> ~/.env.yml
echo 'bg_cmd: feh' ~/.env.yml
fi
