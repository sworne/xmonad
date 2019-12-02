## sworne/xmonad

### Install
`git clone https://github.com/sworne/xmonad.git && cd xmonad && ./deploy.sh`


### Dependencies

#### system
* cabal-install
* compton (optional [compton-tyrone](https://github.com/tryone144/compton))
* dunst
* feh
* fonts-inconsolata
* git
* g++
* make
* rofi
* rxvt-unicode-256color 
* stow
* xsecurelock
* xmonad

#### haskell
* aeson
* async
* base
* bytestring
* control
* data-default 
* lens
* QuickCheck
* split
* text
* wreq
* xmobar
* xmonad
* xmonad-contrib

#### unsplash
Random wallpapers are downloaded using Unsplash, an `Access Key` is needed in order to do so. Please sign up for a key via the [developer page](https://unsplash.com/oauth/applications/new).
Xmonad looks for the Unsplash `Access Key` in the `~/.unsplash-key` file.

If you don't want to use Unsplash the xmonad config will fallback to the wallpaper path set in the `UnsplashConfig` instance.

