. @out@/etc/profile

case "$TERM" in
     dumb)
         unsetopt \
           zle \
           prompt_cr \
           prompt_subst
         if whence -w precmd >/dev/null; then
           unfunction precmd
         fi
         if whence -w preexec >/dev/null; then
           unfunction preexec
         fi
         PS1='$ '
         return
         ;;

esac

# Tell zsh how to find installed completions
fpath+=(@out@/share/zsh/site-functions)

. @zsh-autosuggestions@/share/zsh-autosuggestions/zsh-autosuggestions.zsh

autoload -U colors && colors


unsetopt \
  menu_complete \
  flowcontrol

setopt \
  always_to_end \
  append_history \
  auto_cd \
  auto_menu \
  auto_name_dirs \
  auto_pushd \
  cdablevarS \
  correct \
  correctall \
  complete_in_word \
  extended_glob \
  extended_history \
  hist_expire_dups_first \
  hist_ignore_dups \
  hist_ignore_space \
  hist_find_no_dups \
  hist_reduce_blanks \
  hist_verify \
  inc_append_history \
  interactive_comments \
  long_list_jobs \
  multios \
  no_beep \
  prompt_subst \
  pushd_ignore_dups \
  pushdminus \
  share_history \
  transient_rprompt

ZSH_COMPDUMP="${HOME}/.zcompdump-${SHORT_HOST}-${ZSH_VERSION}"
autoload -U compaudit compinit && compinit -d "${ZSH_COMPDUMP}"
zmodload -i zsh/complist

zstyle ':vcs_info:*' actionformats \
       '%F{5}(%f%s%F{5})%F{3}-%F{5}[%F{2}%b%F{3}|%F{1}%a%F{5}]%f '
zstyle ':vcs_info:*' formats '%F{5}(%f%s%F{5})%F{3}-%F{5}[%F{2}%b%F{5}]%f '
zstyle ':vcs_info:*' enable git

zstyle ':completion:*' matcher-list 'r:|=*' 'l:|=* r:|=*'
zstyle ':completion:*' matcher-list 'm:{a-zA-Z-_}={A-Za-z_-}' 'r:|=*' 'l:|=*'
zstyle ':completion:*' list-colors ''
zstyle ':completion:*:*:kill:*:processes' list-colors \
       '=(#b) #([0-9]#) ([0-9a-z-]#)*=01;34=0=01'
zstyle ':completion:*:*:*:*:processes' command \
       "ps -u $USER -o pid,user,comm -w -w"
zstyle ':completion:*:cd:*' tag-order local-directories directory-stack \
       path-directories
zstyle ':completion::complete:*' use-cache 1
zstyle ':completion::complete:*' cache-path ~/.zsh/cache/$HOST
zstyle ':completion:*' select-prompt \
       '%SScrolling active: current selection at %p%s'
zstyle ':completion:*::::' completer _expand _complete _ignored _approximate
zstyle ':completion:*' menu select=1 _complete _ignored _approximate
zstyle ':completion:*:*:-subscript-:*' tag-order indexes parameters
zstyle ':completion:*' verbose yes
zstyle ':completion:*:descriptions' format '%B%d%b'
zstyle ':completion:*:messages' format '%d'
zstyle ':completion:*:warnings' format 'No matches for: %d'
zstyle ':completion:*:corrections' format '%B%d (errors: %e)%b'
zstyle ':completion:*' group-name ''
zstyle ':completion:*:functions' ignored-patterns '_*'
zstyle ':completion:*:scp:*' tag-order files users \
       'hosts:-host hosts:-domain:domain hosts:-ipaddr"IP\ Address *'
zstyle ':completion:*:scp:*' group-order files all-files users hosts-domain \
       hosts-host hosts-ipaddr
zstyle ':completion:*:ssh:*' tag-order users \
       'hosts:-host hosts:-domain:domain hosts:-ipaddr"IP\ Address *'
zstyle ':completion:*:ssh:*' group-order hosts-domain hosts-host users \
       hosts-ipaddr
zstyle '*' single-ignored show

# Apply theming defaults

typeset -AHg FX FG BG

FX=(
    reset     "%{[00m%}"
    bold      "%{[01m%}" no-bold      "%{[22m%}"
    italic    "%{[03m%}" no-italic    "%{[23m%}"
    underline "%{[04m%}" no-underline "%{[24m%}"
    blink     "%{[05m%}" no-blink     "%{[25m%}"
    reverse   "%{[07m%}" no-reverse   "%{[27m%}"
)

for color in {000..255}; do
    FG[$color]="%{[38;5;${color}m%}"
    BG[$color]="%{[48;5;${color}m%}"
done

export RPROMPT=

local ret_status="%(?:%{$fg_bold[green]%}➜ :%{$fg_bold[red]%}➜ )"
PROMPT='%{$fg[blue]%}%n@%m:%{$fg[cyan]%}%c%{$fg[yellow]%} $%{$reset_color%} '

######################### history options ############################

if [[ $TERM == "xterm-256color" ]]; then
    #Mac OSX/BSD bindings for Home/End/Del
    bindkey "^[[H" beginning-of-line
    bindkey "^[[F" end-of-line
    bindkey "^[[3~" delete-char
fi

# Tell the terminal about the working directory whenever it changes.
if [[ "$TERM_PROGRAM" == "Apple_Terminal" ]] && [[ -z "$INSIDE_EMACS" ]]; then
    update_terminal_cwd() {
        # Identify the directory using a "file:" scheme URL, including
        # the host name to disambiguate local vs. remote paths.

        # Percent-encode the pathname.
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

    # Register the function so it is called whenever the working
    # directory changes.
    autoload add-zsh-hook
    add-zsh-hook chpwd update_terminal_cwd

    # Tell the terminal about the initial directory.
    update_terminal_cwd
fi

# @fortune@/bin/fortune
