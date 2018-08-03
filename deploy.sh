#!/bin/sh
PKGS="xmonad stow xsecurelock hsetroot rofi feh compton rxvt-unicode-256color cabal-install fonts-inconsolata"
HPKG="yaml"
DIRS="xmonad compton x"
sudo apt-get install $PKGS
cabal update
cabal install $HPKG
cd ~/xmonad && stow -S $DIRS
