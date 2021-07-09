#!/bin/bash

command -v git >/dev/null 2>&1 || { echo >&2 "Require git but it's not installed.  Aborting."; kill -INT $$; }
if [[ $EUID -ne 0 ]]; then 
  echo "This script can only be run or configured by root."
  kill -INT $$
fi

\cp -f logicmoo_etc_profile_d.sh /etc/profile.d/

export OLDPWD="`pwd`"
export NEWPWD="$( cd "$( dirname "${BASH_SOURCE[0]}" )"/.. && pwd )"   
export APTGET="apt-get -o Acquire::ForceIPv4=true "

apt-get install python-software-properties

add-apt-repository -y ppa:webupd8team/java
apt-get update
apt-get install oracle-java8-installer

echo select java 8
update-alternatives --config java

cd /opt/logicmoo_workspace/

git pull

mkdir -p bin/
mkdir lib/swipl/pack -p
chmod 555 lib/swipl/pack

export LOGICMOO_WS="$(cd "$(dirname "${BASH_SOURCE[0]}")"; pwd -P)"


if [[ -z "${LOGICMOO_WS}" ]]; then
 WS_MAYBE="$(cd "$(dirname "${BASH_SOURCE[0]}")"; pwd -P)"

 if [[ -d "${WS_MAYBE}/packs_sys" ]]; then
  export LOGICMOO_WS=$WS_MAYBE
 else
  export LOGICMOO_WS=/opt/logicmoo_workspace
 fi

 echo LOGICMOO_WS=$LOGICMOO_WS

fi

if ! [[ "$PATH:" == "$LOGICMOO_WS/bin:"* ]]; then
 export PATH=$LOGICMOO_WS/bin:$PATH
fi


echo PATH=$PATH

export PATH
export LOGICMOO_WS

if which swipl >/dev/null; then
   echo swi-prolog exists 
else
#   apt-add-repository -y ppa:swi-prolog/stable
#   apt-get update
#   apt-get install swi-prolog
echo git clone https://github.com/logicmoo/swipl-devel-unstable swipl-devel-unstable
(cd swipl-devel-unstable ; ../build.unstable)
# (cd swipl-devel-unstable ; make clean ; make distclean ; ../build.unstable)
fi

# Install R + RStudio on Ubuntu 16.04

apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv E084DAB9

# Ubuntu 12.04: precise
# Ubuntu 14.04: trusty
# Ubuntu 16.04: xenial
# Basic format of next line deb https://<my.favorite.cran.mirror>/bin/linux/ubuntu <enter your ubuntu version>/
add-apt-repository 'deb https://ftp.ussg.iu.edu/CRAN/bin/linux/ubuntu xenial/'
apt-get update
apt-get install r-base
apt-get install r-base-dev

   
# Download and Install RStudio
apt-get install gdebi-core
wget https://download1.rstudio.org/rstudio-1.0.44-amd64.deb
gdebi rstudio-1.0.44-amd64.deb
rm rstudio-1.0.44-amd64.deb

add-apt-repository -y ppa:marutter/rrutter
apt-get update
apt-get install r-base r-base-dev
apt-get install r-cran-rserve r-cran-devtools 


add-apt-repository -y ppa:tsl0922/ttyd-dev
apt-get update
apt-get install ttyd


swipl -f /dev/null  -g "absolute_file_name(packs_lib,Dir),make_directory_path(Dir),pack_install('auc',[interactive(false),upgrade(true),package_directory(Dir)])" -g halt
swipl -f /dev/null  -g "absolute_file_name(packs_lib,Dir),make_directory_path(Dir),pack_install('matrix',[interactive(false),upgrade(true),package_directory(Dir)])" -g halt
swipl -f /dev/null  -g "absolute_file_name(packs_lib,Dir),make_directory_path(Dir),pack_install('cplint',[interactive(false),upgrade(true),package_directory(Dir)])" -g halt
swipl -f /dev/null  -g "absolute_file_name(packs_lib,Dir),make_directory_path(Dir),pack_install('aleph',[interactive(false),upgrade(true),package_directory(Dir)])" -g halt
apt-get install libopenmpi-dev
swipl -f /dev/null  -g "absolute_file_name(packs_lib,Dir),make_directory_path(Dir),pack_install('mpi',[interactive(false),upgrade(true),package_directory(Dir)])" -g halt
swipl -f /dev/null  -g "absolute_file_name(packs_lib,Dir),make_directory_path(Dir),pack_install('libfgs',[interactive(false),upgrade(true),package_directory(Dir)])" -g halt
swipl -f /dev/null  -g "absolute_file_name(packs_lib,Dir),make_directory_path(Dir),pack_install('xlibrary',[interactive(false),upgrade(true),package_directory(Dir)])" -g halt
swipl -f /dev/null  -g "absolute_file_name(packs_lib,Dir),make_directory_path(Dir),pack_install('assertions',[interactive(false),upgrade(true),package_directory(Dir)])" -g halt
swipl -f /dev/null  -g "absolute_file_name(packs_lib,Dir),make_directory_path(Dir),pack_install('lambda',[interactive(false),upgrade(true),package_directory(Dir)])" -g halt
swipl -f /dev/null  -g "absolute_file_name(packs_lib,Dir),make_directory_path(Dir),pack_install('rtchecks',[interactive(false),upgrade(true),package_directory(Dir)])" -g halt
swipl -f /dev/null  -g "absolute_file_name(packs_lib,Dir),make_directory_path(Dir),pack_install('xtools',[interactive(false),upgrade(true),package_directory(Dir)])" -g halt

swipl -f /dev/null  -g "absolute_file_name(packs_lib,Dir),make_directory_path(Dir),pack_install('https://github.com/JanWielemaker/rserve_client.git',[interactive(false),package_directory(Dir)])" -g halt
swipl -f /dev/null  -g "absolute_file_name(packs_lib,Dir),make_directory_path(Dir),pack_install('cplint_r',[interactive(false),package_directory(Dir)])" -g halt


######## HDT ################
curl -s http://download.drobilla.net/serd-0.26.0.tar.bz2 | tar -xj && \
  cd serd-0.26.0 && \
  ./waf configure && \
  ./waf && \
  sudo ./waf install;

sudo apt-get install libraptor2-dev
swipl -f /dev/null  -g "absolute_file_name(packs_lib,Dir),make_directory_path(Dir),pack_install('hdt',[interactive(false),upgrade(true),package_directory(Dir)])" -g halt

apt-get install libffi-dev


# Packs that are generally distributed
swipl -f $LOGICMOO_WS/.swiplrc  -g "absolute_file_name(packs_sys,Dir),make_directory_path(Dir),pack_install(logicmoo_utils,[interactive(false),upgrade(true),package_directory(Dir)])" -g halt


# Packs that are generally distributed
swipl -f $LOGICMOO_WS/.swiplrc  -g "absolute_file_name(packs_sys,Dir),make_directory_path(Dir),pack_install(prologmud,[interactive(false),upgrade(true),package_directory(Dir)])" -g halt

# Packs that are user focus
swipl -f $LOGICMOO_WS/.swiplrc  -g "absolute_file_name(packs_usr,Dir),make_directory_path(Dir),pack_install(prologmud_samples,[interactive(false),upgrade(true),package_directory(Dir)])" -g halt

   

if id "prologmud_server" >/dev/null 2>&1; then
 echo "PrologMUD Server User exists"
else
 echo "PrologMUD Server User being created"
 adduser --gecos "PrologMUD Server User" --system --home $PWD/packs_usr/prologmud_samples/prolog/prologmud_sample_games prologmud_server --disabled-password --shell /bin/bash
fi

#chmod 777 /opt/logicmoo_workspace/packs_xtra/prologmud/runtime/cache
#chmod 777 /opt/logicmoo_workspace/packs_xtra/prologmud/runtime

# 3,625 inferences, 6.003 CPU in 6.014 seconds (100% CPU, 604 Lips)
# 1,828,987,011 inferences, 316.932 CPU in 319.418 seconds (99% CPU, 5770916 Lips)
swipl -g "time(load_files(['packs_xtra/logicmoo_nlu/prolog/pldata/nldata_talk_db_pdat'],[qcompile(auto),if_needed(true)])),halt."
swipl -g "time(load_files(['packs_xtra/logicmoo_nlu/prolog/pldata/nldata_freq_pdat'],[qcompile(auto),if_needed(true)])),halt."
swipl -g "time(load_files(['packs_xtra/logicmoo_nlu/prolog/pldata/nldata_BRN_WSJ_LEXICON'],[qcompile(auto),if_needed(true)])),halt."
swipl -g "time(load_files(['packs_xtra/logicmoo_nlu/prolog/pldata/nldata_colloc_pdat'],[qcompile(auto),if_needed(true)])),halt."
swipl -g "time(load_files(['packs_xtra/logicmoo_nlu/prolog/pldata/nldata_cycl_pos0'],[qcompile(auto),if_needed(true)])),halt."
#swipl -g "time(qcompile('packs_sys/logicmoo_base/prolog/logicmoo/plarkc/logicmoo_u_cyc_kb_tinykb')),halt."
#swipl -g "time(load_files(['packs_sys/logicmoo_base/prolog/logicmoo/plarkc/logicmoo_u_cyc_kb_tinykb'],[qcompile(auto),if_needed(true)])),halt."
echo "Compiling a 1gb file this might take about 5 minutes after this it will only take 6 seconds to load"
#swipl -g "time(load_files(['packs_xtra/pldata_larkc/prolog/el_holds/el_assertions'],[qcompile(auto),if_needed(true)])),halt."

if ![ -f $STANFORD_JAR ]; then 
    echo "Downloading $STANFORD_JAR ...";
    wget http://prologmoo.com/downloads/stanford-corenlp3.5.2-ALL.jar -O $STANFORD_JAR
fi


echo "to start the MUD type: ./startMUDServer.sh"
su - prologmud_server

echo git clone https://github.com/logicmoo/swipl-devel-unstable swipl-devel-unstable
(cd swipl-devel-unstable ; ../build.unstable)
# (cd swipl-devel-unstable ; make clean ; make distclean ; ../build.unstable)

# Packs that are generally distributed
echo ./bin/swipl -f $LOGICMOO_WS/.swiplrc  -g "absolute_file_name(packs_sys,Dir),make_directory_path(Dir),pack_install(prologmud,[interactive(false),upgrade(true),package_directory(Dir)])" -g halt

# Packs that are user focus
echo ./bin/swipl -f $LOGICMOO_WS/.swiplrc  -g "absolute_file_name(packs_usr,Dir),make_directory_path(Dir),pack_install(prologmud_samples,[interactive(false),upgrade(true),package_directory(Dir)])" -g halt

# (Non)"Packs" that create the remote interface
## echo git clone --recursive https://github.com/logicmoo/swish-with-filesystem-editing packs_web/swish
## echo git clone --recursive https://github.com/logicmoo/ClioPatria-filessytem-and-clausedb packs_web/ClioPatria-filessytem-and-clausedb
## git clone --recursive https://github.com/logicmoo/plweb packs_web/plweb-realtime

# Very large packs are way beyond most peoples scope and interest (or just too random)
## echo git clone --recursive https://gitlab.logicmoo.org:8060/NomicMU/logicmoo_nlu/  packs_xtra/logicmoo_nlu/
## echo git clone --recursive https://gitlab.logicmoo.org:8060/NomicMU/logicmoo_planners/  packs_xtra/logicmoo_planners/
## echo git clone --recursive https://gitlab.logicmoo.org:8060/NomicMU/logicmoo_packages/  packs_xtra/logicmoo_packages/
## echo git clone --recursive https://gitlab.logicmoo.org:8060/NomicMU/logicmoo_experimental/  packs_xtra/logicmoo_experimental/


