# export PATH=

export LANG=en_US
export LC_ALL=en_US

# export NIX_REMOTE=daemon

export INFOPATH=$PREFIX/share/info
export DICPATH=$PREFIX/share/hunspell
# export EDITOR=@emacs@/bin/emacsclient

# set by nix-profile
# export PATH=$PREFIX/bin
# export SSL_CERT_FILE=@cacert@/etc/ssl/certs/ca-bundle.crt
# export MANPATH=$PREFIX/share/man
# export NIX_SSL_CERT_FILE=@cacert@/etc/ssl/certs/ca-bundle.crt

export CLICOLOR=1
export LSCOLORS=ExFxCxDxBxegedabagacad

alias ls="ls -G --color"
alias l="ls -lF"
alias ..="cd .."
alias ...="cd ../.."
alias ....="cd ../../.."
alias .....="cd ../../../.."
alias tree='tree -Csuh'
alias grep="grep --color=auto"

# only define LC_CTYPE if undefined
if [[ -z "$LC_CTYPE" && -z "$LC_ALL" ]]; then
    export LC_CTYPE=${LANG%%:*} # pick the first entry from LANG
fi

export HISTFILE=$HOME/.history
export HISTSIZE=10000
export SAVEHIST=10000
