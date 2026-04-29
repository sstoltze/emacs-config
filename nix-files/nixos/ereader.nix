{ ... }:

{
  fileSystems = {
    "/media/kobo" = {
      device = "/dev/disk/by-uuid/5F45-F1AB";
      fsType = "auto";
      options = [ "defaults" "nofail" ];
    };
  };
}
