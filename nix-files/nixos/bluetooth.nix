{ pkgs, ... }:
{
  environment.systemPackages = [ pkgs.blueman ];

  hardware.bluetooth = {
    enable = true;
    powerOnBoot = true; # powers up the default Bluetooth controller on boot
  };

  services.blueman.enable = true;
}
