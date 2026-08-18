RC := $(HOME)/.rc

.PHONY: all profile vim gdbinit gitconfig zsh xmonad xmodmap hgrc

all: profile vim gdbinit gitconfig zsh xmonad xmodmap hgrc

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

hgrc:
	rm -f $(HOME)/.hgrc
	ln -s $(RC)/hgrc $(HOME)/.hgrc

subl:
	rm -f $(HOME)/.config/sublime-text-3
	ln -s $(RC)/sublime-text-3 $(HOME)/.config/sublime-text-3

zsh:
	rm -f $(HOME)/.zshrc
	ln -s $(RC)/zshrc $(HOME)/.zshrc
	rm -rf $(HOME)/.oh-my-zsh
	ln -s $(RC)/oh-my-zsh $(HOME)/.oh-my-zsh

xmonad:
	cd $(RC)/ez-xmonad
	./install.sh
	ln -s $(RC)/xmonad $(HOME)/.xmonad
	ln -s $(RC)/xsession $(HOME)/.xsession

xmodmap:
	rm -f $(HOME)/.xmodmap
	ln -s $(RC)/xmodmap $(HOME)/.xmodmap
