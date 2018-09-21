common_packages = emacs fish
linux_packages = $(common_packages) awesome git
cygwin_packages = $(common_packages) gitcyg

.PHONY: install pull linux update cygwin uninstall linux-remove cygwin-remove

.DEFAULT_GOAL:=update

install:
ifeq ($(shell uname),Linux)
	make linux
else ifeq ($(findstring CYGWIN,$(shell uname)),CYGWIN) # check name for cygwin here
	make cygwin
endif

uninstall:
ifeq ($(shell uname),Linux)
	make linux-remove
else ifeq ($(findstring CYGWIN,$(shell uname)),CYGWIN) # check name for cygwin here
	make cygwin-remove
endif

update: pull install

pull:
	git pull

linux:
	stow --no-folding -t ~ $(linux_packages)

linux-remove:
	stow -D -t ~ $(linux_packages)

cygwin:
	stow --no-folding -t ~ $(cygwin_packages)
	cp emacs/.emacs /cygdrive/C/Users/$$USER/AppData/Roaming

cygwin-remove:
	stow -D -t ~ $(cygwin_packages)

new-comp: install
	chsh -s /usr/bin/fish

windows:
	cp /cygdrive/C/Users/$$USER/AppData/Roaming.emacs emacs/
