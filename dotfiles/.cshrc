unlimit
#limit descriptors 256
umask 002

if ($?prompt) then
    if ( -f /bin/bash ) exec /bin/bash -login
    if ( -f /usr/bin/bash ) exec /usr/bin/bash -login
    if ( -f /usr/local/bin/bash ) exec /usr/local/bin/bash -login
    if ( -f /usr/software/bin/bash ) exec /usr/software/bin/bash -login
endif
