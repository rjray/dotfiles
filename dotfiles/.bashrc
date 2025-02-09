# .bashrc

shopt -s no_empty_cmd_completion
umask 0022

# Source global definitions
for file in /etc/bashrc; do
    if [ -f $file ]; then
        . $file
    fi
done

# ble.sh
if [ -f $HOME/.local/share/blesh/ble.sh ]; then
    [[ $- == *i* ]] && source ~/.local/share/blesh/ble.sh
fi

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

# If there is a private file on this host, load it
if [ -f ${HOME}/.bash-private ]; then
    . ${HOME}/.bash-private
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

function set_win_title(){
    echo -ne "\033]0;$HOSTNAME: $(dirs +0) \007"
}
starship_precmd_user_func="set_win_title"

export PATH=${HOME}/bin:${PATH}

# Is the oneAPI toolchain available?
#if [ -f /opt/intel/oneapi/setvars.sh ]; then
    #source /opt/intel/oneapi/setvars.sh > /dev/null
#fi

# Do we have Linuxbrew set up?
if [ -d /home/linuxbrew/.linuxbrew ]; then
    eval $(/home/linuxbrew/.linuxbrew/bin/brew shellenv)
    export XDG_DATA_DIRS="/home/linuxbrew/.linuxbrew/share:$XDG_DATA_DIRS"
fi

if [ -d /opt/homebrew ]; then
    eval $(/opt/homebrew/bin/brew shellenv)
fi

# If Starship is available, use it.
if which starship > /dev/null; then
    eval "$(starship init bash)"
fi

# If rustup is on this machine, add to the PATH:
if [ -f $HOME/.cargo/env ]; then
    . $HOME/.cargo/env
fi

# If pyenv is available, enabled it.
if which pyenv > /dev/null; then
    export PATH="$HOME/.pyenv/bin:$PATH"
    eval "$(pyenv init --path)"
    eval "$(pyenv virtualenv-init -)"
fi
# Some Python stuff seems to land here:
if [ -d $HOME/.local/bin ]; then
    export PATH="$PATH:$HOME/.local/bin"
fi

# Deno
if [ -d $HOME/.deno/bin ]; then
    export DENO_INSTALL="$HOME/.deno"
    export PATH="$PATH:$DENO_INSTALL/bin"
fi

# Bun
if [ -d $HOME/.bun/bin ]; then
    export BUN_INSTALL="$HOME/.bun"
    export PATH="$PATH:$BUN_INSTALL/bin"
fi

# nvm
if [ -s "$NVM_DIR/nvm.sh" ]; then
    . "$NVM_DIR/nvm.sh"
    . "$NVM_DIR/bash_completion"
fi

# fzf
if which fzf > /dev/null; then eval "$(fzf --bash)"; fi

# plenv
if which plenv > /dev/null; then eval "$(plenv init -)"; fi

# nodenv
if which nodenv > /dev/null; then eval "$(nodenv init -)"; fi

# Atuin
if which atuin > /dev/null; then
    eval "$(atuin init bash)"
fi

# More ble.sh
[[ ${BLE_VERSION-} ]] && ble-attach
