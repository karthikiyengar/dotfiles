{ inputs, config, lib, pkgs, ... }:

{
  # Fonts
  fonts.fontconfig = {
    defaultFonts = {
      emoji = [ "Noto Color Emoji" ];
      serif = [ "Bitstream Vera Serif" ];
      sansSerif = [ "Bitstream Vera Sans" ];
      monospace = [ "Bitstream Vera Sans Mono" ];
    };
  };

  # fonts.enableDefaultFonts = true;
  fonts.fonts = with pkgs; [
    noto-fonts
    noto-fonts-emoji
    ttf_bitstream_vera
    hasklig
    meslo-lgs-nf
    unifont
    font-awesome_4
    symbola
    fira-code
    fira-code-symbols
    unifont
  ];
}
