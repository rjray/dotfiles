# General aliases
alias ?=man
alias -- -=%-
alias chars='wc -c'
alias clr=clear
alias com='gzip -v'
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
    zcat "$*" | less
}

moduse() {
    pkg="$1"
    shift
    ack -L "use $pkg" `ack -l "$pkg" $*`
}

localtime () {
    perl -le 'for (@ARGV) { print scalar localtime($_) }' $*
}

iplist() {
    ifconfig | perl -nle '/dr:(\S+)/ && print $1'
}

em () {
    for file in $*; do emacsclient -e "(find-file \"$file\")"; done
}
