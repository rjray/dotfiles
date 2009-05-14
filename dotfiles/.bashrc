# .bashrc

# Source global definitions
for file in /etc/bashrc; do
    if [ -f $file ]; then
        . $file
    fi
done

# Account settings (global across hosts, order is important)
for file in env aliases; do
    if [ -f ${HOME}/.bash_${file} ]; then
        . ${HOME}/.bash_${file}
    fi
done

# If there is a file for this host, load it
if [ -f ${HOME}/.bash-${HOSTNAME} ]; then
	. ${HOME}/.bash-${HOSTNAME}
fi

# If this is an interactive shell, enable completions
if test -n "$PS1"; then
    # Init completion, as some of the command-level bits need this
    . /etc/bash_completion

    # Source desired system-level completion settings
    for cmd in git subversion; do
        if [ -f /etc/bash_completion.d/${cmd} ]; then
	        . /etc/bash_completion.d/${cmd}
        fi
    done

    # Source any local completion scripts
    if [ -d ${HOME}/.bash_completion.d ]; then
        for file in ${HOME}/.bash_completion.d/*; do
            . $file
        done
    fi
fi

export PATH=${HOME}/bin:${PATH}
export PS1='\[\e]0;\h: \w\007\]{ \h: \! } '
# Stripped-down un-ornamented prompt for consoles:
if [ "x${TERM}" == "xlinux" ]; then
    export PS1='{ \h: \! } '
fi
