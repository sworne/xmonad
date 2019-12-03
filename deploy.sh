#!/bin/bash
WDIR=$(pwd)
PKGS="xmonad stow xsecurelock rofi feh rxvt-unicode-256color cabal-install fonts-inconsolata dunst git make g++ compton"
DIRS="xmonad compton x rofi dunst xmobar"
SPOTIFY_URL="https://github.com/dasJ/spotifywm"
COMPTON_URL="https://github.com/tryone144/compton"
UNSPLASH="https://unsplash.com/oauth/applications/new"
SPOTIFY_DIR="/tmp/spotifywm"
KEY_FILE="~/.unsplash_key"
UNSPLASH_MSG="Please create an application at $UNSPLASH and copy/paste the key here:"

# Install packages
function install_pkg() {
  apt-get update
  apt-get install -y $PKGS
}

# Update xmonad
function update_xmonad() {
  cd $WDIR/xmonad/.xmonad
  cabal update
  cabal install -fwith_datezone -fwith_xft -fwith_utf8 --only-dependencies --enable-tests
}

# Get spotifywm
function get_spotifywm() {
  if [ -d "$SPOTIFY_DIR" ]; then
    rm -vrf $SPOTIFY_DIR
  fi
  git clone $SPOTIFY_URL $SPOTIFY_DIR
  cd /tmp/spotifywm && make
  cp spotifywm.so $WDIR/xmonad/.xmonad/
}

# Get unsplash key
function get_key() {
  if [ ! -f "$KEY_FILE" ]; then
    echo UNSPLASH_MSG
    read KEY
    echo $KEY > $KEY_FILE
  fi
}

# Stow files
function stow_files() {
  cd $WDIR && stow -t $HOME -v -S $DIRS
}

# Get conf
function get_conf() {
  read -p "$@" answer
  case ${answer:0:1} in
      y|Y )
          return 0
      ;;
      * )
          return 1
      ;;
  esac
}


function interactive() {
  if $(get_conf "Install linux dependencies using apt-get? "); then
    echo "sudo apt-get install $PKGS"
    install_pkg
  fi
  if $(get_conf "Install haskell dependencies using cabal? "); then
    update_xmonad
  fi
  if $(get_conf "Download spotify window class library? "); then
    get_spotifywm
  fi
  if $(get_conf "Use Unsplash for wallpapers? "); then
    echo $UNSPLASH_MSG
    get_key
  fi
  if $(get_conf "Create symbolic links? "); then
    stow_files
  fi
}

function ci() {
  echo "sudo apt-get install $PKGS"
  install_pkg
  echo "cabal update"
  update_xmonad
  echo "Download spotify window class mod"
  get_spotifywm
  echo "Create symbolic links"
  stow_files
}

function build() {
  cd $WDIR
  sudo docker build -t xmonad:test .
  sudo docker run xmonad:test /bin/bash -c "xmonad --recompile"
}

if [ "$1" == "--docker" ]; then
  ci
elif [ "$1" == "--travis" ]; then
  sudo ci
elif [ "$1" == "--test" ]; then
  build
else
  interactive
fi