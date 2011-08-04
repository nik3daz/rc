#!/bin/bash


#
# Platform specific (first because some stuff in here relies on it)
#

case `uname` in
  Darwin) source $HOME/.rc/profile_Darwin ;;
  Linux)  source $HOME/.rc/profile_Linux ;;
esac

#
# Paths etc
#

# Mac gets crappy hostname sometimes.
__hostname() {
  hostname -s | sed -E 's/dhcp-(.*)$/mac/'
}
export PS1='\[\033[01;32m\]$(__hostname)\[\033[01;34m\] \w\[\033[31m\]$(__git_ps1 "(%s)") \[\033[00m\]'

export GOROOT="$HOME/local/go"
export EDITOR="vim"
export SVN_LOG_EDITOR="$EDITOR"

export PATH="$HOME/local/depot_tools:$PATH"
export PATH="$HOME/src/chromium/src/third_party/WebKit/Tools/Scripts:$PATH"
export PATH="$GOROOT/bin:$PATH"

#
# General
#

alias fn='find . -name'
alias l=ls
alias ll='ls -l'
alias la='ls -A'
alias lla='ls -lA'
alias v=vim
alias vp='vim -p'
alias vs='vim -S'
alias wg='wget --no-check-certificate -O-'
alias grr="grep -rn --color=auto --exclude='.svn'"
alias s="screen -DR"
alias prepend='sed "s|^|$1"'

#
# Git
#

source "$HOME/.rc/git_completion"

alias gitch="git checkout"
alias gitb="git branch"
alias gitd="git diff"
alias gits="git status"
alias gitc="git commit"
alias gitst="git status"
alias gitl="git log"
alias gitr="git rebase"
alias gitp="git pull"
alias gitls="git ls-files"
alias gitm="git merge"
alias gita="git add"
alias gitchm="git checkout master"
alias gitdnm="git diff --numstat master"
alias gitdns="git diff --name-status"

complete -o default -o nospace -F _git_checkout gitch

unmerged() {
  git status -s | grep '^[AUD][AUD] ' | cut -f2 -d' '
}

gitcb() {
  gitb | grep '^*' | cut -f2- -d' '
}

#
# Chromium/WebKit
#

alias cdw="cd $HOME/chromium/third_party/WebKit"
alias cdc="cd $HOME/chromium"
alias bw=build-webkit
alias rwt=run-webkit-tests
alias nrwt=new-run-webkit-tests
alias lkgr='curl http://chromium-status.appspot.com/lkgr'
alias rl=run-launder
alias pc='prepare-ChangeLog --merge-base `git merge-base master HEAD`'

wkup() {
  git fetch && git svn rebase
  # && update-webkit --chromium
}

crup() {
  old_dir=`pwd`

  cdw
  if [ `gitcb` != gclient ]; then
    echo 'ERROR: WebKit not on gclient.'
    return
  fi

  cdc

  echo; echo "Updating Chromium..."
  git pull

  lkgr=`lkgr 2>/dev/null`
  git_lkgr=`gitl --grep=src@${lkgr} | head -n1 | cut -f2- -d' '`
  echo; echo "Resetting to lkgr $git_lkgr ($lkgr)..."
  git reset --hard "$git_lkgr"

  echo; echo "Syncing non-WebKit deps..."
  gclient sync --jobs=32

  echo; echo "Syncing WebKit..."
  tools/sync-webkit-git.py
  cdw
  git reset --hard

  echo; echo "Done."
  cd "$old_dir"
}

po() {
  old_dir=`pwd`
  if [ -d "$1" ]; then
    cd "$1"
  elif [ -f "$1" ]; then
    cd `dirname "$1"`
  else
    echo "Couldn't find file or directory $1"
    return 1
  fi

  print_owners() {
    if [ -f OWNERS ]; then
      echo "=== `pwd`"
      cat OWNERS
      echo
    fi
  }

  while [ `pwd` != "$old_dir" -a `pwd` != / ]; do
    print_owners
    cd ..
  done
  print_owners

  unset print_owners
  cd "$old_dir"
}
