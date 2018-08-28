#!/bin/sh
WDIR=$(pwd)
PKGS="xmonad stow xsecurelock hsetroot rofi feh compton rxvt-unicode-256color cabal-install fonts-inconsolata dunst"
HPKG="yaml"
DIRS="xmonad compton x"
SPOTIFY_URL="https://github.com/dasJ/spotifywm"
SPOTIFY_DIR="/tmp/spotifywm"

# Install packages
echo "sudo apt-get install $PKGS" 
sudo apt-get install $PKGS

# Update xmonad
cabal update
cabal install $HPKG

# Get spotifywm
if [ -d "$SPOTIFY_DIR" ]; then
  rm -vrf $SPOTIFY_DIR
fi
git clone $SPOTIFY_URL $SPOTIFY_DIR
cd /tmp/spotifywm && make
cp spotifywm.so $WDIR/xmonad/.xmonad/

# Stow files
cd $WDIR && stow -S $DIRS