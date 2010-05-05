#--------------------------------------------------------------------
# @(#) cdstack - maintain stack of recently visited directories
#--------------------------------------------------------------------
# $Id: cdstack,v 3.18 2010/03/19 18:53:26 david4 Exp $

# cdstack provides a transparent wrapper designed to extend the
# original idea of 'cd -' to a larger list of recently visited
# directories.  cdstack extends the 'cd -' syntax to 'cd -n' to
# select the nth recently visited directory.
#
# This file automatically executes this shell alias,
#
#     alias cd=cdstack
#
# where cdstack is the name of the shell function (the wrapper)
# implemented in this file.
#
# This wrapper does not use the classic methods provided by the
# dirs/pushd/popd interface because those methods have no special
# consideration for home, cwd, or duplicate dirs.  cdstack maintains
# a list of recently visited directories as a push-down stack, with
# the extra proviso that home and cwd are never pushed, and duplicates
# are always removed.
#
# Typing 'cd -n' means chdir to nth element in the stack.  The nth
# element is "popped" from the stock and cdstack changes to it, then
# the previous working directory is (usually) pushed onto the stack.
#
# Typing 'cd .' means print the stack as a numbered list.  The point
# of 'cd .' is to find values for n which correspond to recently
# visited directories to change to.  That is, use 'cd .' to see
# which directories correspond to which values of n, then change
# to the desired directory by using its number n, aka 'cd -n'.
#
# Typing 'cd -' means cd to the previous working directory.
#
# Typing 'cd' means cd to home, same as you would expect.
#
# Note that a naked 'cd' and the 'cd -' command both use the normal
# builtin mechanisms of the bash shell.
#
# Note that in all cases, cdstack never pushes home or cwd onto the
# stack, since they would clutter 'cd .' output with values that
# would never be chosen.  (That is, if you had wanted to change to
# your home directory, you would have just typed 'cd' because that's
# much faster than typing 'cd .' choosing 'n' and typing 'cd -n'.)
#
# You should now understand the purpose of cdstack is to expand the
# shell's concept of 'cd -' to 'cd -1', 'cd -2', etc, up to 'cd -n'
# for a stack of n recently visited directories.  Since 'cd -' is
# implemented in bash itself, it makes sense that 'cd -1' would be
# similar to but not quite the same as 'cd -', because home, cwd,
# and any duplicate dirs already in the stack are never pushed.
#
# Note that cdstack is specifically designed to operate correctly
# on directory names that contain whitespace characters (this can
# happen often on a Mac).
#
# This version of cdstack should work with all versions of bash.
# I have tested specifically with bash versions 1.14.7, 2.05.0,
# 3.1.17, and 3.2.29, mostly on FreeBSD, but also Linux, MacOS X,
# and Cygwin.
#
# To load cdstack, add these or similar lines to your ~/.bashrc,
# note that I store my bash shell functions in my ~/.bash directory,
#
#     ## -- this loads support for directory stacks
#     ## -- by making 'cd' an alias for 'cdstack'
#     if [ -r ~/.bash/cdstack ] ; then
#         . ~/.bash/cdstack
#     fi
#
# During operation, cdstack maintains its stack in the shell variable
# CDSTACK.  The format for CDSTACK is the same as that of PATH.  Use
# CDSTACKSIZE to specify the maximum number of paths maintained in
# CDSTACK.
#
# Note that CDSTACK and CDSTACKSIZE are shell variables, meaning they
# are not exported (but you can export them if you need to).
#
# You can take a snapshot of your current CDSTACK for future use,
#
#     $ echo "$CDSTACK" > ~/.cdstack
#
# which will be automatically restored the next time cdstack is
# loaded.
#
# By David Thompson  dat1965 -at- yahoo.com

# maximum size of directory stack
: ${CDSTACKSIZE:=36}

# restore stack elements from saved file, if any
if [ -r ~/.cdstack ] ; then
    read CDSTACK < ~/.cdstack
fi

# newer bashes appear to do 'cd -P' on login, but if part
# of your $HOME is symlinked (ie, your $HOME is /home/david
# but /home is a symlink to /usr/home) this symlink will
# mess up initial cdstack printing of $HOME as '~', but
# cd'ing to your symlinked $HOME seems to clear this up
#builtin cd

alias cd=cdstack
cdstack()
{
    local dir new sep
    local cnt indx total=0
    local IFS=: PS3= HOME=${HOME%/}

    # count all elements in the stack
    for dir in $CDSTACK ; do
        total=$(( $total + 1 ))
    done

    # typing 'cd .' means print the stack
    # since stack elements are stored with $HOME expanded
    # let's normalize $HOME into shorter tilde ~ notation
    if [ "$1" = "." ] ; then

        if [ $total -eq 0 ] ; then
            echo "Stack empty" >&2
            return 1
        fi

        new= sep=
        for dir in $CDSTACK ; do
            case "$dir" in "$HOME"/*)
                # normalize into ~ notation
                dir="~${dir#$HOME}"
            esac
            new="$new$sep$dir"
            sep="$IFS"
        done

        # use 'select' for nice multi-column numbered output
        select dir in $new ; do
            :
        done < /dev/null 2>&1

        return 0

    fi

    # typing 'cd -n' means chdir to nth element in stack
    # note how we assume '-n' is the first positional argument
    # eg, on bash 2.0 and above, 'cd [-L|-P] -n' won't work
    # see 'man bash' for explanation of other cd options
    case "$1" in -[1-9]*)

        if [ $total -eq 0 ] ; then
            echo "Stack empty" >&2
            return 1
        fi

        indx=${1#-}
        if [ $indx -gt $total ] ; then
            echo "Stack element out of range" >&2
            return 1
        fi

        cnt=0 new=
        for dir in $CDSTACK ; do
            cnt=$(( $cnt + 1 ))
            if [ $cnt -eq $indx ] ; then
                # found nth element
                new="$dir"
                break
            fi
        done

        # install nth element as positional argument
        set -- "$new"

    esac

    # change to new directory as requested
    builtin cd "$@" || return $?

    # build temporary stack, popping old cwd
    # also remove duplicates and other clutter
    new= sep=
    for dir in $CDSTACK ; do
        [ "$dir" = "" ] && continue
        [ "$dir" = "." ] && continue
        [ "$dir" = "$PWD" ] && continue
        [ "$dir" = "$HOME" ] && continue
        [ "$dir" = "$OLDPWD" ] && continue
        case :"$dir": in *:"$new":*)
            # found duplicate
            continue
        esac
        new="$new$sep$dir"
        sep="$IFS"
    done

    # now push old cwd onto top of stack
    # but never push home or cwd, those are clutter
    if [ "$OLDPWD" != "$HOME" -a "$OLDPWD" != "$PWD" ] ; then
        new="$OLDPWD$sep$new"
    fi

    # copy temporary stack to $CDSTACK variable
    # trimming stack to first $CDSTACKSIZE elements
    CDSTACK= cnt=0 sep=
    for dir in $new ; do
        cnt=$(( $cnt + 1 ))
        if [ $cnt -le $CDSTACKSIZE ] ; then
            CDSTACK="$CDSTACK$sep$dir"
            sep="$IFS"
        fi
    done

    return 0
}
# vim: ft=sh ai et ts=4 sts=4 sw=4
