{ ... }:
{
  programs.fish = {
    enable = true;
    shellInitLast = ''
      source ~/.config/fish/config.fish.mine
    '';
  };
}
