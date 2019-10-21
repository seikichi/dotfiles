#!/bin/zsh

### .zshrc
### seikichi[at]kmc.gr.jp

[[ -f ~/.zshrc.local ]] && source ~/.zshrc.local

## envs
export LANG=ja_JP.UTF-8
export OUTPUT_CHARSET=utf-8
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
alias e=$EDITOR
# sudo
alias sudo='sudo '
# pipe
alias -g L='| less -c'
alias -g L12=' 2>&1 | less -c'
alias -g L2=' 2>&1 1>/dev/null | less -c'
alias -g H='| head'
alias -g T='| tail'
alias -g G='| grep'
alias -g V='| grep -v'
alias -g W='| wc'

# ..
alias -g '...'='../..'
alias -g '....'='../../..'

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
alias -s {gz,tgz,zip,lzh,bz2,tbz,Z,tar,arj,xz,rar,7z}=_extract

# dir_colors
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

# cdr
autoload -Uz is-at-least
if is-at-least 4.3.11; then
    autoload -Uz chpwd_recent_dirs cdr add-zsh-hook
    add-zsh-hook chpwd chpwd_recent_dirs
    zstyle ':chpwd:*' recent-dirs-max 5000
    zstyle ':chpwd:*' recent-dirs-default yes
    zstyle ':completion:*' recent-dirs-insert both
fi
