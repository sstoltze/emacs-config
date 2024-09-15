{ ... }:
{
  nixpkgs.config = {
    # Allow unfree packages
    allowUnfree = true;
  };

  nix = {
    settings.experimental-features = [ "nix-command" "flakes" ];
    optimise.automatic = true;
  };
}
