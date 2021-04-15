{ config, lib, pkgs, ... }:

{
  sound.enable = true;
  security.rtkit.enable = true;

  services = {
    pipewire = {
      enable = true;
      alsa.enable = true;
      pulse.enable = true;
      jack.enable = true;

      media-session.config.bluez-monitor = {
        bluez5.msbc-support = true;
        properties = {
          bluez5.codecs = ["ldac" "aptx_hd"];
        };
      };
    };
  };

  environment.systemPackages = with pkgs; [
    audacity
    ardour
    ladspaPlugins
    pipewire
    gthumb
    obs-studio
    kdenlive
    ffmpeg-full
    frei0r # needed by kdenlive
    gnome3.cheese
    imagemagick
    simplescreenrecorder
    vlc
    spotify
  ];
}
