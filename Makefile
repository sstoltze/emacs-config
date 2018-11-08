common_packages   = fish git
common_no_folding = emacs
linux_packages    = $(common_packages) awesome mbsync gpg
linux_no_folding  = $(common_no_folding)
cygwin_packages   = $(common_packages)
cygwin_no_folding = $(common_no_folding)

ifeq ($(shell echo "check_quotes"),"check_quotes")
platform = WINDOWS
else
platform = $(shell uname)
ifeq ($(findstring CYGWIN,$(platform)),CYGWIN)
platform = CYGWIN
endif
endif

.PHONY: install uninstall update linux linux-remove cygwin cygwin-remove windows windows-remove work work-remove work-update new-comp mbsync-setup remove-work remove update-work

.DEFAULT_GOAL:=update

install:
ifeq ($(platform),Linux)
	make linux
else ifeq ($(platform),CYGWIN)
	make cygwin
else ifeq ($(platform),WINDOWS)
	make windows
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
ifeq ($(platform),CYGWIN)
	cp emacs/.emacs.d/init.el /cygdrive/C/Users/$$USER/AppData/Roaming/.emacs.d/
	cp git/.gitconfig /cygdrive/C/Users/$$USER/AppData/Roaming/
else ifeq ($(platform),WINDOWS)
	copy /Y "emacs\.emacs.d\init.el" "%USERPROFILE%\AppData\Roaming\.emacs.d"
	copy /Y "git\.gitconfig"         "%USERPROFILE%\AppData\Roaming"
endif

windows-remove:
ifeq ($(platform),CYGWIN)
	rm /cygdrive/C/Users/$$USER/AppData/Roaming/.emacs.d/init.el
	rm /cygdrive/C/Users/$$USER/AppData/Roaming/.gitconfig
else ifeq ($(platform),WINDOWS)
	del "%USERPROFILE%\AppData\Roaming\.emacs.d\init.el"
	del "%USERPROFILE%\AppData\Roaming\.gitconfig"
endif

work:
ifneq (,$(filter Linux CYGWIN,$(platform)))
	stow -D -t ~ git
	stow -S -t ~ git-work
endif
ifeq ($(platform),CYGWIN)
	rm /cygdrive/C/Users/$$USER/AppData/Roaming/.gitconfig
	cp git-work/.gitconfig /cygdrive/C/Users/$$USER/AppData/Roaming/
else ifeq ($(platform),WINDOWS)
	del "%USERPROFILE%\AppData\Roaming\.gitconfig"
	copy /Y "git-work\.gitconfig" "%USERPROFILE%\AppData\Roaming"
endif

work-remove:
ifneq (,$(filter Linux CYGWIN,$(platform)))
	stow -D -t ~ git-work
endif
ifeq ($(platform),CYGWIN)
	rm /cygdrive/C/Users/$$USER/AppData/Roaming/.gitconfig
else ifeq ($(platform),WINDOWS)
	del "%USERPROFILE%\AppData\Roaming\.gitconfig"
endif

work-update: work-remove update work

remove-work: work-remove

update-work: update-work

new-comp: install
	chsh -s /usr/bin/fish

mbsync-setup:
	$(shell test -s /home/$$USER/.gnupg/pubring.gpg || gpg2 --generate-key)
	$(shell test -s /home/$$USER/.mailpass.gpg || gpg2 -o ~/.mailpass.gpg -r $$USER -e) # Enter password in file
