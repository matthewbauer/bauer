. @out@/etc/profile

shopt -s cdspell checkwinsize cmdhist dotglob extglob histappend nocaseglob

set -o notify
set -o noclobber
set -o ignoreeof

shopt -u mailwarn

# Enable options:
shopt -s cdspell
shopt -s cdable_vars
shopt -s checkhash
shopt -s checkwinsize
shopt -s sourcepath
shopt -s no_empty_cmd_completion
shopt -s cmdhist
shopt -s histappend histreedit histverify

export SHELL_SESSION_HISTORY=0

# if [ -z "$INSIDE_EMACS" ] || [ "$EMACS_BASH_COMPLETE" = "t" ]; then
#     # Check whether we're running a version of Bash that has support for
#     # programmable completion. If we do, enable all modules installed in
#     # the system (and user profile).
#     if shopt -q progcomp &>/dev/null; then
#       nullglobStatus=$(shopt -p nullglob)
#       shopt -s nullglob
#       for m in "@out@/etc/bash_completion.d/"* "@out@/share/bash-completion/completions/"*; do
#             . $m
#       done
#       eval "$nullglobStatus"
#       unset nullglobStatus p m
#     fi
# fi

# prompt
GREEN="\[$(tput setaf 2)\]"
YELLOW="\[$(/usr/bin/tput setaf 3)\]"
BLUE="\[$(/usr/bin/tput setaf 4)\]"
RESET="\[$(/usr/bin/tput sgr0)\]"


function set-eterm-dir {
    echo -e "\033AnSiTu" "$LOGNAME" # $LOGNAME is more portable than using whoami.
    echo -e "\033AnSiTc" "$(pwd)"
    if [ $(uname) = "SunOS" ]; then
	# The -f option does something else on SunOS and is not needed anyway.
        hostname_options="";
    else
        hostname_options="-f";
    fi
    echo -e "\033AnSiTh" "$(hostname $hostname_options)" # Using the -f option can
    # cause problems on some OSes.
    history -a # Write history to disk.
}

case "$TERM" in
    dumb)
        PS1="\W > "
        ;;
    eterm-color)
        PROMPT_COMMAND=set-eterm-dir
        PS1="${BLUE}\u@\h${RESET}:${GREEN}\w${RESET} ${YELLOW}$ ${RESET}"
        ;;
    *)
        PS1="${BLUE}\u@\h${RESET}:${GREEN}\w${RESET} ${YELLOW}$ ${RESET}"
        ;;
esac

set -o emacs

if [[ $- == *i* ]]; then
  bind '"\e/": dabbrev-expand'
  bind '"\ee": edit-and-execute-command'
fi
