# export PATH=

export LANG=en_US.UTF-8
export LC_ALL=en_US.UTF-8

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
export GREP_OPTIONS='--color=auto'
export GREP_COLOR='3;33'
export LC_COLLATE=C

# alias ls="ls -G --color"
alias ls="TERM=ansi ls --color=always"
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
export HISTSIZE=16000
export SAVEHIST=15000

if [ "$TERM" = dumb ] && [ "$INSIDE_EMACS" ]; then
    export TERM=dumb-emacs-ansi COLORTERM=1
fi

# PS1=" > "

# @fortune@/bin/fortune

export SHELL_SESSION_HISTORY=1

update_terminal_cwd() {
    local URL_PATH=''
    {
        # Use LC_CTYPE=C to process text byte-by-byte.
        local i ch hexch LC_CTYPE=C
        for ((i = 1; i <= ${#PWD}; ++i)); do
            ch="$PWD[i]"
            if [[ "$ch" =~ [/._~A-Za-z0-9-] ]]; then
                URL_PATH+="$ch"
            else
                hexch=$(printf "%02X" "'$ch")
                URL_PATH+="%$hexch"
            fi
        done
    }

    local PWD_URL="file://$HOST$URL_PATH"
    #echo "$PWD_URL"        # testing
    printf '\e]7;%s\a' "$PWD_URL"
}
