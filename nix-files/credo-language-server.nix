{ stdenv, fetchFromGitHub }:
stdenv.mkDerivation {
  name = "credo-language-server";

  src = fetchFromGitHub {
    owner = "elixir-tools";
    repo = "credo-language-server";
    # Release 0.3.0
    rev = "106be2476073d5ec85ed1695dc2b11e94abf650a";
    hash = "sha256-YrwLzQTwtMNiaIT0Ar59PwsT0q+lhAVNvBKAlewHZ2Y=";
  };

  installPhase = ''
    mkdir -p "$out"/bin
    cp -a "$src"/bin/credo-language-server "$out"/bin/credo-language-server
    chmod +x "$out"/bin/credo-language-server
  '';

  dontFixup = true;
}
