# This file contains the environment that can be used to
# build the foreign pack outside Prolog.  This file must
# be loaded into a bourne-compatible shell using
#
#   $ source buildenv.sh

PATH='/opt/logicmoo_workspace/lib/swipl/bin/x86_64-linux:/opt/logicmoo_workspace/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin'
SWIPL='/opt/logicmoo_workspace/lib/swipl/bin/x86_64-linux/swipl'
SWIPLVERSION='80322'
SWIHOME='/opt/logicmoo_workspace/lib/swipl'
SWIARCH='x86_64-linux'
PACKSODIR='lib/x86_64-linux'
SWISOLIB=''
SWILIB='-lswipl'
CC='gcc'
LD='/usr/bin/cc'
CFLAGS='-fPIC -pthread -I"/opt/logicmoo_workspace/lib/swipl/include"'
LDSOFLAGS=' -shared'
SOEXT='so'
USER='root'
HOME='/root'

export  PATH SWIPL SWIPLVERSION SWIHOME SWIARCH PACKSODIR SWISOLIB SWILIB CC LD CFLAGS LDSOFLAGS SOEXT USER HOME
