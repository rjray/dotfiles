# Bash settings specific to NetApp compute-farm machines
unset LESSCHARSET
unset LANG
export LC_ALL=C
unset LC_CTYPE
unset LC_COLLATE
unset LC_TIME
unset LC_NUMERIC
unset LC_MONETARY
unset LC_MESSAGES
unset LC_PAPER
unset LC_NAME
unset LC_ADDRESS
unset LC_TELEPHONE
unset LC_MEASUREMENT
unset LC_IDENTIFICATION
