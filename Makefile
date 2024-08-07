common_packages   = fish git
common_no_folding = emacs stack leiningen ghc
linux_cygwin_common = mbsync screen xdg sbcl bin ocaml direnv
linux_packages    = $(common_packages) $(linux_cygwin_common) awesome kitty x nix redshift
linux_no_folding  = $(common_no_folding)
cygwin_packages   = $(common_packages) $(linux_cygwin_common)
cygwin_no_folding = $(common_no_folding)

ifeq ($(shell echo "check_quotes"),"check_quotes")
platform = WINDOWS
else
platform = $(shell uname)
ifeq ($(findstring CYGWIN,$(platform)),CYGWIN)
platform = CYGWIN
endif
endif

.PHONY: install uninstall remove update linux linux-remove cygwin cygwin-remove windows windows-remove mu4e-setup ubuntu-setup programming-setup latex-setup mbsync-setup kitty-setup setup nixos

.DEFAULT_GOAL:=update

install:
ifeq ($(platform),Linux)
	make linux
else ifeq ($(platform),CYGWIN)
	make cygwin
else ifeq ($(platform),WINDOWS)
	make windows
else ifeq ($(platform),Darwin)
	make darwin
endif

uninstall:
ifeq ($(platform),Linux)
	make linux-remove
else ifeq ($(platform),CYGWIN)
	make cygwin-remove
else ifeq ($(platform),WINDOWS)
	make windows-remove
endif

remove: uninstall

update:
	git pull
	make install

nixos:
	sudo stow --no-folding -S -t / nixos

linux:
	stow              -S -t ~ $(linux_packages)
	stow --no-folding -S -t ~ $(linux_no_folding)

linux-remove:
	stow              -D -t ~ $(linux_packages)
	stow              -D -t ~ $(linux_no_folding)

cygwin: windows
	stow              -S -t ~ $(cygwin_packages)
	stow --no-folding -S -t ~ $(cygwin_no_folding)

cygwin-remove: windows-remove
	stow              -D -t ~ $(cygwin_packages)
	stow              -D -t ~ $(cygwin_no_folding)

windows:
ifeq ($(platform),CYGWIN)
	cp emacs/.emacs.d/init.el /cygdrive/C/Users/$$USER/AppData/Roaming/.emacs.d/
	cp git/.gitconfig /cygdrive/C/Users/$$USER/AppData/Roaming/
	cp git/.config/git/private.git /cygdrive/C/Users/$$USER/.config/git/
	cp git/.config/git/work.git /cygdrive/C/Users/$$USER/.config/git/
else ifeq ($(platform),WINDOWS)
	copy /Y "emacs\.emacs.d\init.el"         "%APPDATA%\.emacs.d"
	copy /Y "git\.gitconfig"                 "%USERPROFILE%"
	if not exist "%USERPROFILE%\.config\git" (mkdir "%USERPROFILE%\.config\git")
	copy /Y "git\.config\git\private.git"    "%USERPROFILE%\.config\git"
	copy /Y "git\.config\git\work.git"       "%USERPROFILE%\.config\git"
	copy /Y "ghc\.ghc\ghci.conf"             "%APPDATA%\ghc"
endif

windows-remove:
ifeq ($(platform),CYGWIN)
	rm /cygdrive/C/Users/$$USER/AppData/Roaming/.emacs.d/init.el
	rm /cygdrive/C/Users/$$USER/.gitconfig
	rm /cygdrive/C/Users/$$USER/.config/git/private.git
	rm /cygdrive/C/Users/$$USER/.config/git/work.git
else ifeq ($(platform),WINDOWS)
	del "%APPDATA%\.emacs.d\init.el"
	del "%USERPROFILE%\.gitconfig"
	del "%USERPROFILE%\.config\git\private.git"
	del "%USERPROFILE%\.config\git\work.git"
	del "%APPDATA%\ghc\ghci.conf"
endif

darwin:
	stow -S -t ~ home-manager
	make linux
	stow -D -t ~ direnv # Messes with the home-manager config

mu4e-setup:
	sudo apt install isync mu4e
	mkdir -p ~/.local/.mail/gmail
	mkdir -p ~/.local/.mail/work

ubuntu-setup: .repository
	sudo apt install awesome fonts-terminus emacs-snapshot fish i3lock scrot sox direnv solaar imagemagick xautolock librsvg2-bin
	# Froms http://phd-sid.ethz.ch/debian/fonts-iosevka/
	sudo dpkg -i ./fonts/fonts-iosevka_4.2.0+ds-1_all.deb
	sudo dpkg-reconfigure fontconfig
	chsh -s /usr/bin/fish

programming-setup: .repository
	sudo apt install opam racket plotutils sbcl
	# Anything earlier than 4.11.0 breaks man pages on fish
	opam switch create 4.11.0
	opam init
	opam install dune utop tuareg merlin ocamlformat user-setup
	opam user-setup install
	curl -sSL https://get.haskellstack.org/ | bash
	curl https://sh.rustup.rs -sSf | bash
	curl -L https://nixos.org/nix/install | bash

latex-setup:
	sudo apt install texlive-full

mbsync-setup:
	$(shell test -s /home/$$USER/.gnupg/pubring.gpg || gpg2 --generate-key)
	$(shell test -s /home/$$USER/.mailpass.gpg || gpg2 -o ~/.mailpass.gpg -r $$USER -e) # Enter password in file

# Test if kitty already installed?
kitty-setup:
	curl -L https://sw.kovidgoyal.net/kitty/installer.sh | bash /dev/stdin
	ln -s ~/.local/kitty.app/bin/kitty ~/.local/bin/kitty
	cp -r ~/.local/kitty.app/share/ ~/.local/
	sudo update-alternatives --install /usr/bin/x-terminal-emulator x-terminal-emulator ~/.local/bin/kitty 50
	sudo update-alternatives --config x-terminal-emulator

.repository:
	# Newest version of fish
	sudo apt-add-repository ppa:fish-shell/release-3
	# Newest version of emacs
	sudo add-apt-repository ppa:ubuntu-elisp/ppa
	sudo add-apt-repository ppa:plt/racket
	#	sudo add-apt-repository ppa:avsm/ppa # Opam 2.0, for ubuntu < 20.04
	# Newest version of git
	sudo add-apt-repository ppa:git-core/ppa
	sudo apt update
	touch .repository

setup: kitty-setup ubuntu-setup programming-setup latex-setup
