common_packages   = fish
common_no_folding = emacs
linux_packages    = $(common_packages) awesome git
linux_no_folding  = $(common_no_folding)
cygwin_packages   = $(common_packages) gitcyg
cygwin_no_folding = $(common_no_folding)

.PHONY: install pull linux update cygwin uninstall linux-remove cygwin-remove windows windows-remove new-comp

.DEFAULT_GOAL:=update

install:
ifeq ($(shell uname),Linux)
	make linux
else ifeq ($(findstring CYGWIN,$(shell uname)),CYGWIN)
	make cygwin
endif

uninstall:
ifeq ($(shell uname),Linux)
	make linux-remove
else ifeq ($(findstring CYGWIN,$(shell uname)),CYGWIN)
	make cygwin-remove
endif

update: pull install

pull:
	git pull

linux:
	stow --no-folding -S -t ~ $(linux_no_folding)
	stow -S -t ~ $(linux_packages)

linux-remove:
	stow -D -t ~ $(linux_packages)
	stow -D -t ~ $(linux_no_folding)

cygwin: windows
	stow --no-folding -S -t ~ $(cygwin_no_folding)
	stow -S -t ~ $(cygwin_packages)

cygwin-remove: windows-remove
	stow -D -t ~ $(cygwin_packages)
	stow -D -t ~ $(cygwin_no_folding)

windows:
	cp emacs/.emacs.d/init.el /cygdrive/C/Users/$$USER/AppData/Roaming/.emacs.d/
	cp git/.gitconfig /cygdrive/C/Users/$$USER/AppData/Roaming/

windows-remove:
	rm /cygdrive/C/Users/$$USER/AppData/Roaming/.emacs.d/init.el
	rm /cygdrive/C/Users/$$USER/AppData/Roaming/.gitconfig

new-comp: install
	chsh -s /usr/bin/fish
