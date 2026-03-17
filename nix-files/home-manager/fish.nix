{ ... }:
{
  programs.fish = {
    enable = true;
    shellInitLast = ''
      fish_config theme choose default
      source ~/.config/fish/config.fish.mine
    '';
  };
}
