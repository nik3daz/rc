#!/bin/bash

export GYP_GENERATORS='ninja'
export GYP_DEFINES='component=shared_library clang=0 win_use_external_manifest=1'
export GYP_PARALLEL=1

#
# Undo stuff before sourcing anything
#

for a in l ll la; do
  if alias -p | grep $a &>/dev/null; then
    unalias $a &>/dev/null
  fi
done

#
# Platform specific (first because some stuff in here relies on it)
#

case `uname` in
  Darwin) source $HOME/.rc/profile_Darwin ;;
  Linux)  source $HOME/.rc/profile_Linux ;;
esac

alias vi='vim'
#
# Paths etc
#

# Mac gets crappy hostname sometimes.
export PS1='\[\033[01;32m\]# $(hostname)\[\033[01;34m\] \w \[\033[31m\]$(__git_ps1 "(%s)")\n\[\033[01;32m\]> \[\033[00m\]'

export GOROOT="$HOME/local/go"
export EDITOR="vim"
export SVN_LOG_EDITOR="$EDITOR"

export PATH="$HOME/local/bin:$PATH"
export PATH="$HOME/local/rc_scripts:$PATH"
export PATH="$HOME/depot_tools:$PATH"
export PATH="$GOROOT/bin:$PATH"

#
# General
#

fn()      { find . -name "$@"; }
c()       { cd -P "$@"; }
ll()      { l -l "$@"; }
la()      { l -A "$@"; }
lla()     { l -lA "$@"; }
v()       { vim -p "$@"; }
e()       { vim -p "$@"; }
wg()      { wget --no-check-certificate -O- "$@"; }
grr()     { grep -rn --color --exclude='.svn' "$@"; }
s()       { screen -DR "$@"; }
prepend() { sed "s|^|$1" "$@"; }

vl() {
  file=`echo "$1" | cut -d: -f1`
  line=`echo "$1" | cut -d: -f2`
  v "$file" +"$line"
}

#
# Git
#

source "$HOME/.rc/git_completion"

g()    { git "$@"; }
gch()  { git checkout "$@"; }
gb()   { git branch "$@" | grep -v '^  __' | grep -v ' master$'; }
gd()   { git diff "$@"; }
gs()   { git status "$@"; }
gc()   { git commit "$@"; }
gst()  { git status "$@"; }
gl()   { git log "$@"; }
gr()   { git rebase "$@"; }
gup()  { git branch --set-upstream "$@"; }
gp()   { git pull "$@"; }
gls()  { git ls-files "$@"; }
gm()   { git merge "$@"; }
ga()   { git add "$@"; }
gchm() { git checkout master "$@"; }
gdnm() { git diff --numstat master "$@"; }
gdns() { git diff --name-status "$@"; }
gds()  { git diff --stat "$@"; }
glf()  { git ls-files "$@"; }
gmb()  { git merge-base "$@"; }
gg()   { git grep "$@"; }
gcp()  { git cherry-pick "$@"; }
oops() { git commit -a --amend "$@"; }
gdu()  { git diff '@{u}'; }
gdgmb() {
  x=$1;
  y=$2
  if [ -z $x ]; then
    x=HEAD;
  fi;
  if [-z $y]; then
    y=master;
  fi
  git diff `git merge-base $y $x` $x;
}

gclup() { gdu | grep '^+.*LOG' || git cl upload "$@"; }
git-archive() {
  x=$2;
  if [ -z $x ]; then
    x=$1;
  fi;
  git tag "archive/$x" "$1" && git branch -D "$1";
}

complete -o default -o nospace -F _git_branch gb
complete -o default -o nospace -F _git_branch git-archive
complete -o default -o nospace -F _git_branch ghide
complete -o default -o nospace -F _git_checkout gch
complete -o default -o nospace -F _git_checkout gchr
complete -o default -o nospace -F _git_diff changed
complete -o default -o nospace -F _git_diff gd
complete -o default -o nospace -F _git_diff gdns
complete -o default -o nospace -F _git_diff gds
complete -o default -o nospace -F _git_merge_base gmb
complete -o default -o nospace -F _git_merge_base gdgmb
complete -o default -o nospace -F _git_log gl
complete -o default -o nospace -F _git_rebase gr
complete -o default -o nospace -F _git_rebase grm

unmerged() {
  git status -s | grep '^[AUD][AUD] ' | cut -f2 -d' '
}

gC() {
  gc -m `gcb` "$@"
}

gcb() {
  git branch | grep '^*' | cut -f2- -d' '
}

gbase() {
  branch=`gcb`
  gmb $branch master
}

gtry() {
  tag=`date +%H:%M`
  revision=`gl | grep src@ | head -n1 | sed -E 's/.*(src@[0-9]+).*/\1/g'`

  bots="$1"
  if [ -z "$bots" ]; then
    default_bots=`echo $(g try --print_bots | grep '^  ') | tr ' ' ,`
    bots="${default_bots},win,mac,linux"
  fi

  tests="$2"
  if [ -n "$tests" ]; then
    tests="-t $tests"
  fi

  g try -n "`gcb`-$tag" -r "$revision" -b "$bots" $tests
}

ghide() {
  if [ -z "$1" ]; then
    echo "ERROR: no branch(es) supplied"
    return
  fi
  for branch in "$@"; do
    gb "$branch" -m "__`date +%F`__$branch"
  done
}

gclean() {
  current_date=`date +%F | tr -d -`
  for branch in `git branch | grep -E '__[0-9-]+__'`; do
    branch_date=`echo "$branch" | grep -Eo '__[0-9-]+__' | tr -d _-`
    if [ $branch_date -lt $(( $current_date - 100 )) ]; then
      read -N 1 -p "Delete $branch [N/y] "
      echo
      if [ "$REPLY" = y ]; then
        git branch -D "$branch"
        echo
      fi
    fi
  done

  read -N 1 -p "Run \"git gc\" [N/y] "
  echo
  if [ "$REPLY" = y ]; then
    git gc
  fi
}

changed() {
  base="$1"
  if [ -z "$base" ]; then
    base=`gbase`
  fi
  gdns "$base" | cut -f2
}

gdiff() {
  base="$1"
  if [ -z "$base" ]; then
    base=`gbase`
  fi
  gd "$base"
}

gchr() {
  oldBranch=`gcb`
  branch="$1"
  if [ -z "$branch" ]; then
    echo "ERROR: no branch supplied"
    return
  fi
  gch "$branch"
  gr "$oldBranch"
}

crb() {
  crclang "$@"
}

crt() {
  target="$1"
  filter="$2"
  shift 2
  if [ -z "$target" ]; then
    echo "Usage: $0 target [filter]"
    return
  fi
  if [ -n "$filter" ]; then
    filter="--gtest_filter=$filter"
  fi
  "$target" "$filter" "$@"
}

gfind() {
  gls "*/$1"
}

gsquash() {
  g reset `gbase`
  ga chrome
  gC
}

lkgr() {
  curl http://chromium-status.appspot.com/lkgr
}

car() {
  local OPTIND
  builddir="out/Debug"
  norun=0
  while getopts "d:n" o; do
    case "$o" in
      d) builddir=$"$OPTARG";;
      n) norun=1;;
      ?) echo >&2 "Usage: car [-d dir] [-n] target [opts]"; return 1;;
    esac
  done
  shift $((OPTIND - 1))
  target="$1"
  shift
  ninja -C $builddir
  if [ $? -eq 0 ] && [ $norun -eq 0 ]; then
    $builddir/$target $@
  fi
}

crbr() {
  if [ -z "$1" ]; then
    echo "Usage: $0 TESTNAME args..."
    return 1
  fi

  local testname="$1"
  shift

  crclang "$testname" && "b/$testname" "$@"
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

greplace() {
  from="$1"
  to="$2"

  if [ -z "$from" -o -z "$to" ]; then
    echo "greplace from to <args>"
    return 1
  fi

  shift 2

  for f in `gg -l "$@" "$from"`; do
    echo "Replacing in $f"
    sed -i $SED_I_SUFFIX "s:$from:$to:g" "$f"
  done
}

crsync() {
  gclient sync -fDj32 -r src@HEAD
}

gclu() {
  g cl upload `gbase` "$@"
}
umask 022
