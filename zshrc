export ZSH_DISABLE_COMPFIX=true
# Path to your oh-my-zsh installation.
export ZSH=$HOME/.oh-my-zsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
ZSH_THEME="agnoster"

# Uncomment the following line to use case-sensitive completion.
# CASE_SENSITIVE="true"

# Uncomment the following line to use hyphen-insensitive completion. Case
# sensitive completion must be off. _ and - will be interchangeable.
# HYPHEN_INSENSITIVE="true"

# Uncomment the following line to disable bi-weekly auto-update checks.
# DISABLE_AUTO_UPDATE="true"

# Uncomment the following line to change how often to auto-update (in days).
# export UPDATE_ZSH_DAYS=13

# Uncomment the following line to disable colors in ls.
# DISABLE_LS_COLORS="true"

# Uncomment the following line to disable auto-setting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment the following line to enable command auto-correction.
# ENABLE_CORRECTION="true"

# Uncomment the following line to display red dots whilst waiting for completion.
COMPLETION_WAITING_DOTS="true"

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Uncomment the following line if you want to change the command execution time
# stamp shown in the history command output.
# The optional three formats: "mm/dd/yyyy"|"dd.mm.yyyy"|"yyyy-mm-dd"
# HIST_STAMPS="mm/dd/yyyy"

# Would you like to use another custom folder than $ZSH/custom?
# ZSH_CUSTOM=/path/to/new-custom-folder

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
plugins=(gitfast zsh-syntax-highlighting)

# User configuration

export PATH="$HOME/local/go/bin:$HOME/local/depot_tools:$HOME/depot_tools:$HOME/local/rc_scripts:$HOME/local/bin:$HOME/local/go/bin:$HOME/local/depot_tools:$HOME/depot_tools:$HOME/local/rc_scripts:$HOME/local/bin:/usr/lib/google-golang/bin:/usr/local/buildtools/java/jdk/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:$HOME/local/src/third_party/android_tools/sdk/platform-tools:$HOME/node_modules/bin:$HOME/local/crosfleet"
# export MANPATH="/usr/local/man:$MANPATH"

source $ZSH/oh-my-zsh.sh
source /etc/bash_completion.d/hgd 2>/dev/null

fpath=(~/.zsh $fpath)

alias gm='git map-branches'
alias gchu='git checkout @{u}'
alias gdun='git diff --name-only @{u} | cat'
alias gdu='git diff @{u}'
alias gdt='git difftool -y'
alias gdtu='git difftool -y @{u}'
alias gg='git grep'
alias gch='git checkout'
alias gls='git ls-files'
alias oops='git commit -a --amend'

export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && . "$NVM_DIR/nvm.sh"  # This loads nvm

loadreview() {
  gch -b $1 && git cl patch $@
}

resource() {
  . ~/.zshrc
}

gdn() {
  git diff --name-only $@ | cat
}

localserver() {
  python -c 'import BaseHTTPServer as bhs, SimpleHTTPServer as shs; bhs.HTTPServer(("127.0.0.1", 8888), shs.SimpleHTTPRequestHandler).serve_forever()'
}

car() {
  local OPTIND
  local gndir="out-chromeos/"
  local buildtype="Default"
  local norun=0
  local noxvfb=0
  local verbose=''
  while getopts "t:d:nx" o; do
    case "$o" in
      t) buildtype="$OPTARG";;
      d) gndir="${OPTARG%/}/";;
      v) verbose='-v';;
      n) norun=1;;
      x) noxvfb=1;;
      ?) echo >&2 "Usage: car [-d dir] [-x] [-n] target [opts]"; return 1;;
    esac
  done
  shift $((OPTIND - 1))
  local target="$1"
  shift
  builddir="${gndir}${buildtype}/"
  if [ $norun -eq 0 ]; then
    # ninja -j800 -l80 -C $builddir $target || return 1
    if [[ $target == *"webui_closure_compile"* && $noxvfb -eq 0 ]]; then
      ninja -j20 $verbose -l60 -C $builddir $target || return 1
    else
      autoninja -l60 $verbose -C $builddir $target || return 1
    fi
    if [[ $target == *"test"* && $noxvfb -eq 0 ]]; then
      ./testing/xvfb.py ${builddir}${target} $@
    else
      ${builddir}${target} $@ --enable-pixel-output-in-tests --use-system-clipboard --ui-test-action-max-timeout=1000000 --test-launcher-timeout=1000000
    fi
  else
    autoninja -l80 -C $builddir $target $@ || return 1
  fi
}

gdgmb() {
  x=$1;
  y=$2
  if [ -z $x ]; then
    x=HEAD;
  fi;
  if [ -z $y ]; then
    y=master;
  fi
  git diff `git merge-base $y $x` $x;
}

gchp() {
  if [ -z "$1" ]; then
    1=HEAD
  fi
  for b in "$@"; do
    git checkout "$b" && git pull 1>/dev/null;
    if [ $? -ne 0 ]; then
      echo $b;
      return 1;
    fi
  done
}

alias __git-checkout_main=_git_checkout
compdef _git gchp=git_checkout

gchpall() {
  gchp `gm --no-color | grep '  '`
}

ginsertnode() {
  if [ -z "$1" ]; then
    echo "Needs branch name"
    return
  fi

  local current=`git rev-parse --abbrev-ref HEAD`
  gch @{u} &&
  gch -b $1 &&
  gch $current &&
  gb --set-upstream-to=$1 &&
  gch $1
}

gdeletenode() {
  local upstream=`git rev-parse --abbrev-ref @{u}`
  local current=`git rev-parse --abbrev-ref HEAD`
  for b in `git for-each-ref --format='%(refname:short):%(upstream:short)' refs/heads | grep ":$current\$"`; do
    local child=`echo $b | sed 's/:.*$//'`
    gch $child && gb --set-upstream-to=$upstream
  done
  echo "Run gb -D $current"
}

greplace() {
  sed -i "s/$1/$2/g" `gdu --name-only`
}

greplaceall() {
  sed -i "s/$1/$2/g" `gg --name-only $1`
}

greview() {
  echo $1 | grep '[^0-9]' && echo "error: CL numbers only" && exit 1
  gb -D $1 2> /dev/null
  gch master &&
  gch -b $1 &&
  git cl patch $1
}

#__git_complete gchp _git_checkout
#__git_complete gdgmb _git_merge_base

alias gmb='git merge-base'

ckbraces() {
  perl -lne '$s = $. if /(if|while|for) ?\(.*\{$/; print "$ARGV:$." if /^ *\}$/ && $. - 2 == $s; $. = 0 if eof' `gdu --name-only | egrep '\.cc|\.h|\.js'`
}

cklog() {
  gdu | grep -i '+.*\blog\b'
}

upload() {
  local ck="`ckbraces; cklog`"
  if [ $ck ]; then
    echo $ck
    return 1;
  fi
  git cl upload $@
}

screenshotdiff() {
  local dir=$HOME/Downloads
  local f1=`ls $dir | grep ^Screenshot | sed -n 'x;$p'`
  local f2=`ls $dir | grep ^Screenshot | tail -1`
  compare $dir/$f1 $dir/$f2 /tmp/diff.png &&
  display /tmp/diff.png
}

# You may need to manually set your language environment
# export LANG=en_US.UTF-8

# Preferred editor for local and remote sessions
# if [[ -n $SSH_CONNECTION ]]; then
#   export EDITOR='vim'
# else
#   export EDITOR='mvim'
# fi

# Compilation flags
# export ARCHFLAGS="-arch x86_64"

# ssh
# export SSH_KEY_PATH="~/.ssh/dsa_id"

# Set personal aliases, overriding those provided by oh-my-zsh libs,
# plugins, and themes. Aliases can be placed here, though oh-my-zsh
# users are encouraged to define aliases within the ZSH_CUSTOM folder.
# For a full list of active aliases, run `alias`.
#
# Example aliases
# alias zshconfig="mate ~/.zshrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"
# The following lines were added by compinstall

zstyle ':completion:*' completer _complete _ignored
zstyle :compinstall filename "$HOME/.zshrc"

autoload -Uz compinit
compinit
# End of lines added by compinstall
alias gup='gb --set-upstream-to'

alias ncar='car -n'

alias gd~='gd HEAD~'

alias src='cd ~/local/src'
alias nodedir='cd ~/local/src/third_party/node'
alias mwc='cd ~/local/src/third_party/material_web_components'

#alias __git-checkout_main=_git_checkout
#complete -o default -o nospace -F _git_checkout gchp

#alias __git-merge-base_main=_git_merge_base
compdef _git gdgmb=git-merge-base
export CODESERVER_DIR="~/Downloads/code-server-4.0.2-linux-amd64"
alias startcodeserver="screen -d -m zsh -c \"$CODESERVER_DIR/bin/code-server --auth none --host localhost --port 8123 ~/local/src\""

start_env() {
  startcodeserver
  goma_ctl ensure_start
}

export NODE_PATH='/usr/local/lib/node_modules'

gen_local_webui_tsconfig() {
  OUT_DIR=$1
  SRC_DIR=$2
  REL_OUT_DIR=`realpath --relative-to=$SRC_DIR $OUT_DIR`
  echo '{ "references": [{"path": "'$REL_OUT_DIR'/gen/'$SRC_DIR'/tsconfig.json" }]}' > $SRC_DIR/tsconfig.json
}

reldir() {
  TO=$1
  FROM=$2
  if [ -z $FROM ]; then
    FROM=.
  fi;
  realpath --relative-to=$FROM $TO
}

startup() {
  startcodeserver
  goma_ctl ensure_start
}


alias mountsshfs='sshfs calamity@freshprince.syd.corp.google.com:/usr/local/google/home/calamity/local/src remote_src'
alias sshdesk='ssh freshprince.syd.corp.google.com'
alias sshcloudtop='ssh asperitas.c.googlers.com'
alias sshcodeserver='ssh -L9000:localhost:8123 freshprince.syd.corp.google.com'

