{ pkgs, ... }:
{
  # rtkit is optional but recommended
  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
    # If you want to use JACK applications, uncomment this
    jack.enable = true;
    wireplumber.configPackages = [
      (pkgs.writeTextDir "share/wireplumber/main.lua.d/99-fix-tv.lua" ''
          alsa_monitor.rules = {
          {
            matches = {
              {
                -- Matches PHILIPS FTV.
                { "alsa.name", "matches", "HDMI 0" },
                { "node.name", "matches", "alsa_output.*" },
              }
            },
            apply_properties = {
            ["audio.format"]           = "S16LE",
            ["audio.channels"]         = 2,
            ["audio.position"]         = "FR,FL"
            },
          }
        }
      '')
    ];
  };
}
