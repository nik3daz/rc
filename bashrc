# ~/.bashrc: executed by bash(1) for non-login shells.
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples

# If not running interactively, don't do anything
[ -z "$PS1" ] && return

# don't put duplicate lines or lines starting with space in the history.
# See bash(1) for more options
HISTCONTROL=ignoreboth

# append to the history file, don't overwrite it
shopt -s histappend

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
HISTSIZE=1000
HISTFILESIZE=2000

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# If set, the pattern "**" used in a pathname expansion context will
# match all files and zero or more directories and subdirectories.
#shopt -s globstar

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

# set variable identifying the chroot you work in (used in the prompt below)
if [ -z "$debian_chroot" ] && [ -r /etc/debian_chroot ]; then
    debian_chroot=$(cat /etc/debian_chroot)
fi

__hostname() {
  hostname -s | sed -E 's/dhcp-(.*)$/mac/'
}
export PS1='\[\033[01;32m\]# $(__hostname)\[\033[01;34m\] \w \[\033[31m\]$(__git_ps1 "(%s)")\n\[\033[01;32m\]> \[\033[00m\]'
export PROMPT_COMMAND='~/.rc/scripts/prompt_command'
export GIT_MERGE_AUTOEDIT=no

# If this is an xterm set the title to user@host:dir
case "$TERM" in
xterm*|rxvt*)
    PS1="\[\e]0;${debian_chroot:+($debian_chroot)}\u@\h: \w\a\]$PS1"
    ;;
*)
    ;;
esac

# enable color support of ls and also add handy aliases
if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
    alias ls='ls --color=auto'
    #alias dir='dir --color=auto'
    #alias vdir='vdir --color=auto'

    alias grep='grep --color=auto'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'
fi

# some more ls aliases
alias ll='ls -alF'
alias la='ls -A'
alias l='ls -CF'

# Add an "alert" alias for long running commands.  Use like so:
#   sleep 10; alert
alias alert='notify-send --urgency=low -i "$([ $? = 0 ] && echo terminal || echo error)" "$(history|tail -n1|sed -e '\''s/^\s*[0-9]\+\s*//;s/[;&|]\s*alert$//'\'')"'

# Alias definitions.
# You may want to put all your additions into a separate file like
# ~/.bash_aliases, instead of adding them here directly.
# See /usr/share/doc/bash-doc/examples in the bash-doc package.

if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
fi

# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
if [ -f /etc/bash_completion ] && ! shopt -oq posix; then
    . /etc/bash_completion
fi

#
# General
#

fn()      { find . -name "$@"; }
c()       { cd -P "$@"; }
lla()     { ls -lA "$@"; }
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
gchu()  { git checkout "@{u}" "$@"; }
gchp() {
  for b in "$@"; do
    git checkout "$b" && git pull 1>/dev/null;
    if [ $? -ne 0 ]; then
      echo $b;
      return 1;
    fi
  done
}
gb()   { git branch "$@" | grep -v '^  __' | grep -v ' master$'; }
gd()   { git diff "$@"; }
gs()   { git status "$@"; }
gc()   { git commit "$@"; }
gst()  { git status "$@"; }
gl()   { git log "$@"; }
gr()   { git rebase "$@"; }
gup()  { git branch --set-upstream-to "$@"; }
gp()   { git pull "$@"; }
gls()  { git ls-files "$@"; }
gm()   { git map-branches "$@"; }
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
gdu()  { git diff '@{u}' "$@"; }
gdgmb() {
  x=$1;
  y=$2
  if [ -z $x ]; then
    x=HEAD;
  fi;
  if [ -z $y ]; then
    y=master;
  fi
  git diff $GDGMBFLAGS `git merge-base $y $x` $x;
}

gclup() { gdu | grep '^+.*LOG' || git cl upload "$@"; }
git-archive() {
  for x in $@; do
    git tag "archive/$x" "$x" && git branch -D "$x";
  done
}

complete -o default -o nospace -F _git_branch gb
complete -o default -o nospace -F _git_branch git-archive
complete -o default -o nospace -F _git_branch ghide
complete -o default -o nospace -F _git_checkout gch
complete -o default -o nospace -F _git_checkout gchp
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

ginsertnode() {
  local current=`git rev-parse --abbrev-ref HEAD`
  gch @{u} &&
  gch -b $1 &&
  gch $current &&
  gup $1 &&
  gch $1
}

gdeletenode() {
  local upstream=`git rev-parse --abbrev-ref @{u}`
  local current=`git rev-parse --abbrev-ref HEAD`
  for b in `git for-each-ref --format='%(refname:short):%(upstream:short)' refs/heads | grep ":$current\$"`; do
    local child=`echo $b | sed 's/:.*$//'`
    gch $child && gup $upstream
  done
  echo "Run gb -D $current"
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

rot13() {
  tr 'A-Za-z' 'N-ZA-Mn-za-m' $@
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
  local gndir="out-gn/"
  local buildtype="Default"
  local norun=0
  while getopts "t:d:n" o; do
    case "$o" in
      t) buildtype="$OPTARG";;
      d) gndir="${OPTARG%/}/";;
      n) norun=1;;
      ?) echo >&2 "Usage: car [-d dir] [-g] [-n] target [opts]"; return 1;;
    esac
  done
  shift $((OPTIND - 1))
  local target="$1"
  shift
  builddir="${gndir}${buildtype}/"
  ninja -j1000 -C $builddir $target || return 1
  if [ $norun -eq 0 ]; then
    ${builddir}${target} $@
  fi
}

cargyp() {
  local OPTIND
  local gypdir="out-gn/"
  local buildtype="Default"
  local gypflag=""
  local norun=0
  local rungyp=0
  while getopts "t:d:ng" o; do
    case "$o" in
      t) buildtype="$OPTARG";;
      d) gypdir="${OPTARG%/}/";
         gypflag="--generator-output=$gypdir";;
      n) norun=1;;
      g) rungyp=1;;
      ?) echo >&2 "Usage: car [-d dir] [-g] [-n] target [opts]"; return 1;;
    esac
  done
  shift $((OPTIND - 1))
  local target="$1"
  shift
  if [ $rungyp -eq 1 ]; then
    local GYP_DEFINES_BEFORE="$GYP_DEFINES"
    if [ "$gypdir" == "out-chromeos/" ]; then
      export GYP_DEFINES="$GYP_DEFINES chromeos=1"
    fi
    echo $gypdir
    echo $gypflag
    echo $GYP_DEFINES
    trap "export GYP_DEFINES=\"$GYP_DEFINES_BEFORE\"" SIGINT
    build/gyp_chromium $gypflag
    local gypresult=$?
    export GYP_DEFINES="$GYP_DEFINES_BEFORE"
    if [ $gypresult -ne 0 ]; then
      return 1;
    fi
  fi
  builddir="${gypdir}${buildtype}/"
  ninja -j1000 -C $builddir $target || return 1
  if [ $norun -eq 0 ]; then
    ${builddir}${target} $@
  fi
}

gcar() {
  car -g $@;
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

resource() {
  . ~/.profile
  . ~/.bashrc
}

. ~/.profile

