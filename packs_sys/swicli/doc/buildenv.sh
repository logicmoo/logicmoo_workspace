# This file contains the environment that can be used to
# build the foreign pack outside Prolog.  This file must
# be loaded into a bourne-compatible shell using
#
#   $ source buildenv.sh

PATH='/usr/lib/swipl/bin/x86_64-linux:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/snap/bin:/usr/lib/jvm/java-8-oracle/bin:/usr/lib/jvm/java-8-oracle/db/bin:/usr/lib/jvm/java-8-oracle/jre/bin'
SWIPL='/usr/lib/swipl/bin/x86_64-linux/swipl'
SWIPLVERSION='70333'
SWIHOME='/usr/lib/swipl'
SWIARCH='x86_64-linux'
PACKSODIR='lib/x86_64-linux'
SWISOLIB=''
SWILIB='-lswipl'
CC='gcc'
LD='gcc'
CFLAGS='-fno-strict-aliasing -pthread -fPIC  -I"/usr/lib/swipl/include"'
LDSOFLAGS='-rdynamic -O2 -pthread -Wl,-rpath=/usr/lib/swipl/lib/x86_64-linux  -shared'
SOEXT='so'
USER='root'
HOME='/root'

export  PATH SWIPL SWIPLVERSION SWIHOME SWIARCH PACKSODIR SWISOLIB SWILIB CC LD CFLAGS LDSOFLAGS SOEXT USER HOME
