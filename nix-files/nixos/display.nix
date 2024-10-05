{ pkgs, ... }:

{
  environment.systemPackages = [ pkgs.awesome ];
  services = {
    displayManager = {
      defaultSession = "none+awesome";
    };

    xserver = {
      enable = true;

      displayManager = {
        lightdm.enable = true;

        sessionCommands = ''
          ${pkgs.xorg.xrdb}/bin/xrdb -merge <<EOF
            Xcursor.theme: Adwaita
            Xcursor.size: 32
          EOF
        '';
      };

      windowManager.awesome = {
        enable = true;
        luaModules = [ pkgs.luaPackages.vicious ];
      };
    };

    redshift = {
      enable = true;
      # A basic copy of @related [redshift.conf](redshift/.config/redshift/redshift.conf)
      brightness = {
        day = "0.8";
        night = "0.8";
      };
      temperature = {
        day = 4200;
        night = 3800;
      };

      extraOptions = [
        "-r" # No fading
      ];
    };
  };
}
