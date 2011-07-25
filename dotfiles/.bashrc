# .bashrc

shopt -s no_empty_cmd_completion

# Source global definitions
for file in /etc/bashrc; do
    if [ -f $file ]; then
        . $file
    fi
done

# Because MacOS is so very different in some (annoying) ways, note the
# machine type for use here and in .bash_env:
export UNAME=$(uname)

# Set the basic path here (not in .bash_env) so that it can be added to by
# per-host files. I just prefer $PATH to be here rather than .bash_env. But
# don't do this for MacOS, because the default $PATH is set up through plists.
if [ "x$UNAME" != "xDarwin" ]; then
    export PATH=/bin:/sbin:/usr/bin:/usr/sbin:/usr/local/bin:/usr/local/sbin:/usr/games
fi

# Account settings (global across hosts, order is important)
for file in env aliases; do
    if [ -f ${HOME}/.bash_${file} ]; then
        . ${HOME}/.bash_${file}
    fi
done

# If there is a file for this domain, load it
if [ -f /bin/domainname ]; then
    DOMAIN=$(domainname)
    if [ -f ${HOME}/.bash-${DOMAIN} ]; then
	    . ${HOME}/.bash-${DOMAIN}
    fi
fi

# If there is a file for this host, load it
if [ -f ${HOME}/.bash-${HOSTNAME} ]; then
	. ${HOME}/.bash-${HOSTNAME}
fi

# If this is an interactive shell, enable completions
if test -n "$PS1"; then
    # Init completion, as some of the command-level bits need this
    if [ -f /etc/bash_completion ]; then
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
fi

# Do we have any stand-alone "command" files?
if [ -d ${HOME}/.bash ]; then
    for file in ${HOME}/.bash/*; do
        . $file
    done
fi

export PATH=${HOME}/bin:${PATH}
export PS1='\[\e]0;\h: \w\007\]{ \h: \! } '
# Stripped-down un-ornamented prompt for consoles:
if [ "x${TERM}" == "xlinux" -o "x${TERM}" == "xdumb" ]; then
    export PS1='{ \h: \! } '
fi

# Added by perlbrew

source /Users/rjray/perl5/perlbrew/etc/bashrc
