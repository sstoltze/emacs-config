linux_packages = emacs fish git awesome

.PHONY: install pull linux update
linux:
	stow -t ~ $(linux_packages)

install: linux

pull:
	git pull

update: pull linux
