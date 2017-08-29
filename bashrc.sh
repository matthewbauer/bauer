. @out@/etc/profile

case "$TERM" in
    dumb)
        PS1="\W > "
        return
        ;;
    # eterm*)
    #           cd()    { command cd    "$@"; printf '\033AnSiTc %s\n' "$PWD"; }
    #           pushd() { command pushd "$@"; printf '\033AnSiTc %s\n' "$PWD"; }
    #           popd()  { command popd  "$@"; printf '\033AnSiTc %s\n' "$PWD"; }

    #           printf '\033AnSiTc %s\n' "$PWD"
    #           printf '\033AnSiTh %s\n' "$HOSTNAME"
    #           printf '\033AnSiTu %s\n' "$USER"
    #             ;;
esac

shopt -s \
      cdspell \
      checkwinsize \
      cmdhist \
      dotglob \
      extglob \
      histappend \
      nocaseglob \
      cdspell \
      cdable_vars \
      checkhash \
      checkwinsize \
      sourcepath \
      no_empty_cmd_completion \
      cmdhist \
      histappend \
      histreedit \
      histverify

shopt -u mailwarn

set -o notify \
    -o noclobber \
    -o ignoreeof \
    -o emacs

export SHELL_SESSION_HISTORY=0

# if [ "${BASH_VERSINFO}" -ge 4 ] && ([ -z "$INSIDE_EMACS" ] || [ "$EMACS_BASH_COMPLETE" = "t" ]); then
#     . @out@/share/bash-completion/bash_completion

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

case "$TERM" in
    *)
        PS1="${BLUE}\u@\h${RESET}:${GREEN}\w${RESET} ${YELLOW}$ ${RESET}"
        ;;
esac

if [[ $- == *i* ]]; then
    bind '"\e/": dabbrev-expand'
    bind '"\ee": edit-and-execute-command'
fi

if [ "$TERM_PROGRAM" = "Apple_Terminal" ] && [ -z "$INSIDE_EMACS" ]; then
      PROMPT_COMMAND="update_terminal_cwd;$PROMPT_COMMAND"
      update_terminal_cwd
fi

# @fortune@/bin/fortune
