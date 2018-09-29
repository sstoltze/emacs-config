common_packages   = fish git
common_no_folding = emacs
linux_packages    = $(common_packages) awesome mbsync systemd gpg
linux_no_folding  = $(common_no_folding)
cygwin_packages   = $(common_packages)
cygwin_no_folding = $(common_no_folding)

.PHONY: install uninstall update linux linux-remove cygwin cygwin-remove windows windows-remove work work-remove new-comp mbsync-setup remove-work remove update-work work-update

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

remove: uninstall

update:
	git pull
	make install

linux:
	stow              -S -t ~ $(linux_packages)
	stow --no-folding -S -t ~ $(linux_no_folding)

linux-remove:
	stow -D -t ~ $(linux_packages)
	stow -D -t ~ $(linux_no_folding)

cygwin: windows
	stow              -S -t ~ $(cygwin_packages)
	stow --no-folding -S -t ~ $(cygwin_no_folding)

cygwin-remove: windows-remove
	stow -D -t ~ $(cygwin_packages)
	stow -D -t ~ $(cygwin_no_folding)

windows:
	cp emacs/.emacs.d/init.el /cygdrive/C/Users/$$USER/AppData/Roaming/.emacs.d/
	cp git/.gitconfig /cygdrive/C/Users/$$USER/AppData/Roaming/

windows-remove:
	rm /cygdrive/C/Users/$$USER/AppData/Roaming/.emacs.d/init.el
	rm /cygdrive/C/Users/$$USER/AppData/Roaming/.gitconfig

work:
	stow -D -t ~ git
	stow -S -t ~ git-work
ifeq ($(findstring CYGWIN,$(shell uname)),CYGWIN)
	rm /cygdrive/C/Users/$$USER/AppData/Roaming/.gitconfig
	cp git-work/.gitconfig /cygdrive/C/Users/$$USER/AppData/Roaming/
endif

work-remove:
	stow -D -t ~ git-work
ifeq ($(findstring CYGWIN,$(shell uname)),CYGWIN)
	rm /cygdrive/C/Users/$$USER/AppData/Roaming/.gitconfig
endif

remove-work: work-remove

update-work: work-remove update work

work-update: update-work

new-comp: install
	chsh -s /usr/bin/fish

mbsync-setup:
	$(shell test -s /home/$$USER/.gnupg/pubring.gpg || gpg2 --generate-key)
	$(shell test -s /home/$$USER/.mailpass.gpg || gpg2 -o ~/.mailpass.gpg -r $$USER -e) # Enter password in file
