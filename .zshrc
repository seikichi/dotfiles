#!/bin/zsh

### .zshrc
### seikichi[at]kmc.gr.jp

[[ -f ~/.zshrc.local ]] && source ~/.zshrc.local

## envs
export LANG=ja_JP.UTF-8
export OUTPUT_CHARSET=utf-8
export LV="-Ou8"
export EDITOR="env TERM=xterm-256color emacsclient -t"

## prompt
PROMPT="%m:%n%% "
PROMPT2="%_%% "
SPROMPT="%r is correct? [n,y,a,e]: "
RPROMPT='[%~]'

## history
HISTFILE=$HOME/.zsh_history
HISTSIZE=1000000
SAVEHIST=1000000

## history search
autoload history-search-end
zle -N history-beginning-search-backward-end history-search-end
zle -N history-beginning-search-forward-end history-search-end
bindkey "^P" history-beginning-search-backward-end
bindkey "^N" history-beginning-search-forward-end

autoload -Uz is-at-least
if is-at-least 4.3.10; then
    bindkey '^R' history-incremental-pattern-search-backward
    bindkey '^S' history-incremental-pattern-search-forward
fi

## bindkey
# emacs bind
bindkey -e
bindkey '	' reverse-menu-complete

## setopt
# ignore duplication command history list
setopt hist_ignore_dups
# share command history data
setopt share_history
# autocd
setopt auto_cd
# use comment out in interactive mode
setopt INTERACTIVE_COMMENTS
# glob
setopt EXTENDED_GLOB
setopt AUTO_PUSHD
setopt PUSHD_IGNORE_DUPS
setopt nolistbeep
setopt correct
setopt correct_all
setopt list_packed
setopt magic_equal_subst
setopt no_flow_control

# edit command line in emacs (C-o)
autoload -U edit-command-line
zle -N edit-command-line
bindkey "^O" edit-command-line

## alias
alias ll="ls -lh --color=auto"
alias ls="ls --color=auto"
alias df="df -h"
alias e=$EDITOR
alias tree="tree -C"
alias ltree="tree -lpshDFC"
# sudo
alias sudo='sudo '
# pipe
alias -g L='| less -c'
alias -g L12=' 2>&1 | less -c'
alias -g L2=' 2>&1 1>/dev/null | less -c'
alias -g P='pygmentize -O encoding=utf-8 -O style=fruity -f console256 -g '
alias -g H='| head'
alias -g T='| tail'
alias -g G='| grep'
alias -g V='| grep -v'
alias -g W='| wc'
for i in $(seq 1 10); do alias -g C$i="| awk '{ print \$$i }'"; done
# ..
alias -g '...'='../..'
alias -g '....'='../../..'

# ps + grep
function psg() {
    ps -ef | tee >(head -n 1) | grep -E "$1" | grep -v grep
}

# extract archives for alias -s
function _extract() {
    case $1 in
        *.tar.gz|*.tgz) tar xzvf $1 ;;
        *.tar.xz) tar Jxvf $1 ;;
        *.zip) unzip $1 ;;
        *.lzh) lha e $1 ;;
        *.tar.bz2|*.tbz) tar xjvf $1 ;;
        *.tar.Z) tar zxvf $1 ;;
        *.gz) gzip -dc $1 ;;
        *.bz2) bzip2 -dc $1 ;;
        *.Z) uncompress $1 ;;
        *.tar) tar xvf $1 ;;
        *.arj) unarj $1 ;;
        *.rar) unrar x $1 ;;
    esac
}

# alias -s
if which aunpack 1>/dev/null 2>&1; then
    alias -s {gz,tgz,zip,lzh,bz2,tbz,Z,tar,arj,xz,rar,7z}=aunpack
else
    alias -s {gz,tgz,zip,lzh,bz2,tbz,Z,tar,arj,xz,rar,7z}=_extract
fi

# use grc to color some commands command if exists
if which grc  1>/dev/null 2>&1; then
    alias mount='grc mount'
    alias ifconfig='grc ifconfig'
    alias dig='grc dig'
    alias ldap='grc ldap'
    alias netstat='grc netstat'
    alias ping='grc ping'
    alias ps='grc ps'
    alias traceroute='grc traceroute'
    alias gcc='grc gcc'
fi

# color man page
export MANPAGER='less -R'
function man() {
    env \
        LESS_TERMCAP_mb=$(printf "\e[1;31m") \
        LESS_TERMCAP_md=$(printf "\e[1;31m") \
        LESS_TERMCAP_me=$(printf "\e[0m") \
        LESS_TERMCAP_se=$(printf "\e[0m") \
        LESS_TERMCAP_so=$(printf "\e[1;44;33m") \
        LESS_TERMCAP_ue=$(printf "\e[0m") \
        LESS_TERMCAP_us=$(printf "\e[1;32m") \
        man "$@"
}

# color pager
export LESS='-R -x2'
if which pygmentize  1>/dev/null 2>&1; then
    export LESSOPEN='| ~/dotfiles/.lessfilter %s'
fi

function tex2pdf() {
    platex $1 && dvipdf ${1/.tex/.dvi} && acroread ${1/.tex/.pdf}
}
alias -s tex=tex2pdf

function p3m() { pdftohtml $1 -stdout | w3m -T text/html; }
alias -s pdf=p3m

[[ -f ~/.dir_colors ]] && eval $(dircolors -b ~/.dir_colors)
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}

# completion
autoload -U compinit
compinit

# url escaping
autoload -Uz url-quote-magic
zle -N self-insert url-quote-magic

# C-w
WORDCHARS='*?_-.[]~=&;!#$%^(){}<>'

# Go
export GOPATH=$HOME/go
export PATH=$PATH:$HOME/go/bin

# peco
function peco-select-history() {
    local tac
    if which tac > /dev/null; then
        tac="tac"
    else
        tac="tail -r"
    fi
    BUFFER=$(\history -n 1 | \
        eval $tac | \
        peco --query "$LBUFFER")
    CURSOR=$#BUFFER
    zle clear-screen
}
zle -N peco-select-history

autoload -Uz is-at-least
if is-at-least 4.3.11; then
    autoload -Uz chpwd_recent_dirs cdr add-zsh-hook
    add-zsh-hook chpwd chpwd_recent_dirs
    zstyle ':chpwd:*' recent-dirs-max 5000
    zstyle ':chpwd:*' recent-dirs-default yes
    zstyle ':completion:*' recent-dirs-insert both
fi
function peco-cdr () {
    local selected_dir="$(cdr -l | awk '{ print $2 }' | peco)"
    if [ -n "$selected_dir" ]; then
        BUFFER="cd ${selected_dir}"
        zle accept-line
    fi
    zle clear-screen
}
zle -N peco-cdr

function peco-kill-process () {
    \ps -ef | peco | awk '{ print $2 }' | xargs -n1 kill
    zle clear-screen
}
zle -N peco-kill-process

function peco-find-file () {
    ls | peco | xargs -n1 env TERM=xterm-256color emacsclient -t
    zle clear-screen
}
zle -N peco-find-file

function peco-open-app () {
    ls | peco | xargs open
    zle clear-screen
}
zle -N peco-open-app

function peco-src () {
    local selected_dir="$(ghq list -p | peco --query "$LBUFFER")"
    if [ -n "$selected_dir" ]; then
        BUFFER="cd ${selected_dir}"
        zle accept-line
    fi
    zle clear-screen
}
zle -N peco-src

if [ -x "`which peco`" ]; then
    bindkey '^r' peco-select-history

    bindkey '^xr' peco-select-history
    bindkey '^xf' peco-find-file
    bindkey '^xc' peco-cdr
    bindkey '^xk' peco-kill-process
    bindkey '^xo' peco-open-app
    bindkey '^xs' peco-src
fi
