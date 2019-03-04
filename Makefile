RC := $(HOME)/.rc

.PHONY: all profile vim gdbinit gitconfig zsh xmonad xmodmap

all: profile vim gdbinit gitconfig zsh xmonad xmodmap

profile:
	rm -f $(HOME)/.profile
	ln -s $(RC)/profile $(HOME)/.profile

vim:
	rm -rf $(HOME)/.vim
	rm -f $(HOME)/.vimrc
	ln -s $(RC)/vim $(HOME)/.vim
	ln -s $(RC)/vimrc $(HOME)/.vimrc

gdbinit:
	rm -f $(HOME)/.gdbinit
	ln -s $(RC)/gdbinit $(HOME)/.gdbinit

gitconfig:
	rm -f $(HOME)/.gitconfig
	ln -s $(RC)/gitconfig $(HOME)/.gitconfig

zsh:
	rm -f $(HOME)/.zshrc
	ln -s $(RC)/zshrc $(HOME)/.zshrc
	rm -rf $(HOME)/.oh-my-zsh
	ln -s $(RC)/oh-my-zsh $(HOME)/.oh-my-zsh

xmonad:
	cd $(RC)/ez-xmonad
	./install.sh

xmodmap:
	rm -f $(HOME)/.xmodmap
	ln -s $(RC)/xmodmap $(HOME)/.xmodmap
