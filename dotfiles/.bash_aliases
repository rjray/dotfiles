# General aliases
alias ?=man
alias -- -=%-
alias chars='wc -c'
alias clr=clear
alias com='gzip -v'
alias ff='echo firefox --new-tab "file://$(perl -MFile::Spec -le "print File::Spec->rel2abs(shift)")"'
alias h='history +50'
alias hup='kill -HUP \!*'
alias j='jobs -l'
alias l=less
alias la="uptime | perl -ne 's/.*load/load/;print'"
alias lines='wc -l'
alias ll='/bin/ls -lasi'
alias lll='/bin/ls -saild'
alias ls='ls -F'
alias lsd='ls -d .[_0-9a-zA-Z]*'
alias po=popd
alias pu=pushd
alias rehash='hash -r'
alias Rsync='rsync -e ssh -auzv'
alias sds=dirs
alias today='cal -3'
alias tt=taskwarrior-tui
alias uncom=gunzip
alias words='wc -w'
alias z=suspend
alias zombies='ps al | grep " Z "'

psg() {
    ps aux | head -1 | grep -v Broken ; ps aux | grep $* | grep -v grep
}

pod() {
    pod2text -o "$*" | less
}

zl() {
    gzcat "$*" | less
}

localtime () {
    perl -le 'for (@ARGV) { print scalar localtime(($_ =~ /(\d+)/)[0]) }' $*
}

iplist() {
    ifconfig | perl -nle '/dr:(\S+)/ && print $1'
}

em () {
    for file in $*; do emacsclient -e "(find-file \"$file\")"; done
}

addnodebin () {
    export PATH=$PATH:$PWD/node_modules/.bin
}

upremote () {
    (cd $* && (git remote | grep upstream > /dev/null) && git fetch upstream && git merge upstream/master && git push origin master)
}

# Explicitly add the oneAPI toolchain (if available and not already in $PATH)
add_intel() {
    if [ -z $(which icx) ]
    then
        if [ -f /opt/intel/oneapi/setvars.sh ]; then
            source /opt/intel/oneapi/setvars.sh > /dev/null
        fi
    else
        echo "oneAPI toolchain not available."
    fi
}
