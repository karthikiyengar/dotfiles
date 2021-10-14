flags: {}
extra-package-dbs: []
packages:
- .
- location: ./taffybar
  extra-dep: true
- location: ../xmonad/xmonad-contrib
  extra-dep: true
- location: ../xmonad/xmonad
  extra-dep: true
# - location: ../../../../Projects/status-notifier-item
#   extra-dep: true
# - location: ../../../../Projects/gtk-sni-tray
#   extra-dep: true
extra-deps:
- ConfigFile-1.1.4
- broadcast-chan-0.2.0.2
- dbus-hslogger-0.1.0.1
- gi-cairo-connector-0.0.1
- gi-cairo-render-0.0.1
- gi-dbusmenu-0.4.6
- gi-dbusmenugtk3-0.4.7
- gi-gdkx11-3.0.8
- gi-xlib-2.0.7
- gtk-sni-tray-0.1.6.0
- gtk-strut-0.1.3.0
- haskell-gi-0.22.6
- rate-limit-1.4.1
- status-notifier-item-0.3.0.3
- time-units-1.0.0
- xml-helpers-1.0.0
resolver: nightly-2019-06-19
allow-newer: true
nix:
  packages:
    - cairo
    - gcc
    - gnome2.pango
    - gobjectIntrospection
    - gtk3
    - hicolor-icon-theme
    - libdbusmenu-glib
    - libdbusmenu-gtk3
    - libxml2
    - numix-icon-theme-circle
    - pkgconfig
    - x11
    - xorg.libX11
    - xorg.libXext
    - xorg.libXinerama
    - xorg.libXrandr
    - xorg.libXrender
    - zlib

