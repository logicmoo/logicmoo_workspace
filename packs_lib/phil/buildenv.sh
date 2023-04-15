# This file contains the environment that can be used to
# build the foreign pack outside Prolog.  This file must
# be loaded into a bourne-compatible shell using
#
#   $ source buildenv.sh

PATH='/opt/logicmoo_workspace/lib/swipl/bin/x86_64-linux:/opt/logicmoo_workspace/packs_sys/instant_prolog_docs/biocham:/opt/logicmoo_workspace/packs_sys/instant_prolog_docs/biocham:/root/.cargo/bin:/opt/ros/noetic/bin:/opt/miniconda3/bin:/root/.nvm/versions/node/v18.8.0/bin:/opt/logicmoo_workspace/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/snap/bin:/root/.dotnet/tools:/opt/logicmoo_workspace/packs_lib/sparqlprog/bin'
SWIPL='/opt/logicmoo_workspace/lib/swipl/bin/x86_64-linux/swipl'
SWIPL_PACK_VERSION='1'
SWIPLVERSION='80515'
SWIHOME='/opt/logicmoo_workspace/lib/swipl'
SWIARCH='x86_64-linux'
PACKSODIR='lib/x86_64-linux'
SWISOLIB=''
SWILIB='-lswipl'
CC='gcc'
LD='/usr/lib/ccache/cc'
SWIPL_INCLUDE_DIRS='/opt/logicmoo_workspace/lib/swipl/include'
SWIPL_LIBRARIES_DIR='/opt/logicmoo_workspace/lib/swipl/lib/x86_64-linux'
CFLAGS='-fPIC -pthread -I"/opt/logicmoo_workspace/lib/swipl/include"'
LDSOFLAGS=' -shared'
SOEXT='so'
PREFIX='/opt/logicmoo_workspace'
USER='root'
HOME='/root'
LANG='en_US.UTF-8'

export  PATH SWIPL SWIPL_PACK_VERSION SWIPLVERSION SWIHOME SWIARCH PACKSODIR SWISOLIB SWILIB CC LD SWIPL_INCLUDE_DIRS SWIPL_LIBRARIES_DIR CFLAGS LDSOFLAGS SOEXT PREFIX USER HOME LANG
