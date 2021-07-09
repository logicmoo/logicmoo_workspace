#!/bin/bash

(./INSTALL-SWI.md)
stty sane



#if which swipl >/dev/null; then
#   echo swi-prolog exists 
#else
#   apt-add-repository -y ppa:swi-prolog/stable
#   apt-get update
#   apt-get install swi-prolog
#echo git clone https://github.com/logicmoo/swipl-devel-unstable swipl-devel-unstable
#(cd swipl-devel-unstable ; ../build.unstable)
# (cd swipl-devel-unstable ; make clean ; make distclean ; ../build.unstable)
#fi

# Install R + RStudio on Ubuntu 16.04

apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv E084DAB9

# Ubuntu 12.04: precise
# Ubuntu 14.04: trusty
# Ubuntu 16.04: xenial
# Basic format of next line deb https://<my.favorite.cran.mirror>/bin/linux/ubuntu <enter your ubuntu version>/
#add-apt-repository deb https://ftp.ussg.iu.edu/CRAN/bin/linux/ubuntu xenial/
#apt-get update
#apt-get install r-base
#apt-get install r-base-dev

   
# Download and Install RStudio
#apt-get install gdebi-core
#wget https://download1.rstudio.org/rstudio-1.0.44-amd64.deb
#gdebi rstudio-1.0.44-amd64.deb
#rm rstudio-1.0.44-amd64.deb

#add-apt-repository -y ppa:marutter/rrutter
#apt-get update
#apt-get install r-base r-base-dev
#apt-get install r-cran-rserve r-cran-devtools 


#add-apt-repository -y ppa:tsl0922/ttyd-dev
#apt-get update
#apt-get install ttyd

install_swi_package(){

if [[ ! -d "$1/$2" ]]; then
 if test -n "${3-}"; then
  ( cd $1 ; git clone $3 --recursive )
 else
  swipl -f /dev/null  -g "absolute_file_name(${1},Dir),make_directory_path(Dir),pack_install(${2},[interactive(false),upgrade(true),package_directory(Dir)])" -g halt
 fi
fi 
 URL=$( cd $1/$2 ; git remote -v get-url origin )
 git submodule add $URL $1/$2
}

export -f install_swi_package

install_swi_package packs_lib auc 
install_swi_package packs_lib matrix 
install_swi_package packs_lib cplint 

install_swi_package packs_lib aleph


install_swi_package packs_lib bddem
install_swi_package packs_lib lambda
install_swi_package packs_lib rserve_client	 

apt-get install libopenmpi-dev

install_swi_package packs_lib mpi 
#install_swi_package packs_lib libfgs 
install_swi_package packs_lib xlibrary 
install_swi_package packs_lib assertions 
install_swi_package packs_lib lambda 
install_swi_package packs_lib rtchecks 
install_swi_package packs_lib xtools 

install_swi_package packs_lib cplint_r

install_swi_package packs_lib phil 
install_swi_package packs_lib trill 
install_swi_package packs_lib rocksdb 

# apt-get install libffi-dev
# install_swi_package packs_lib https://github.com/JanWielemaker/rserve_client.git,[interactive(false),package_directory(Dir)])" -g halt

######## HDT ################
#curl -s http://download.drobilla.net/serd-0.26.0.tar.bz2 | tar -xj && \
#  cd serd-0.26.0 && \
#  ./waf configure && \
#  ./waf && \
#  sudo ./waf install;

#sudo apt-get install libraptor2-dev
#echo install_swi_package packs_lib hdt 



# Packs that are generally distributed
install_swi_package packs_sys logicmoo_utils 

# Packs that are user focus

install_swi_package packs_sys dictoo
install_swi_package packs_sys multimodal_dcg
install_swi_package packs_sys pfc
install_swi_package packs_sys gvar_syntax
install_swi_package packs_sys predicate_streams
install_swi_package packs_sys logicmoo_base
install_swi_package packs_sys logicmoo_cg 
install_swi_package packs_sys logicmoo_ec 
install_swi_package packs_sys logicmoo_nlu 
install_swi_package packs_sys prologmud 
install_swi_package packs_sys prologmud_samples 
install_swi_package packs_sys wam_common_lisp 
install_swi_package packs_sys lps_corner 
# install_swi_package packs_sys planner_api 

install_swi_package packs_sys logicmoo_nars https://github.com/logicmoo/logicmoo_nars


install_swi_package packs_web logicmoo_webui 
install_swi_package packs_web swish https://github.com/logicmoo/swish
install_swi_package packs_web ClioPatria https://github.com/logicmoo/ClioPatria

   

#if id "prologmud_server" >/dev/null 2>&1; then
# echo "PrologMUD Server User exists"
#else
# echo "PrologMUD Server User being created"
# adduser --gecos "PrologMUD Server User" --system --home $PWD/packs_usr/prologmud_samples/prolog/prologmud_sample_games prologmud_server --disabled-password --shell /bin/bash
#fi

#chmod 777 /opt/logicmoo_workspace/packs_xtra/prologmud/runtime/cache
#chmod 777 /opt/logicmoo_workspace/packs_xtra/prologmud/runtime

# 3,625 inferences, 6.003 CPU in 6.014 seconds (100% CPU, 604 Lips)
# 1,828,987,011 inferences, 316.932 CPU in 319.418 seconds (99% CPU, 5770916 Lips)
#swipl -g "time(load_files([packs_xtra/logicmoo_nlu/prolog/pldata/nldata_talk_db_pdat],[qcompile(auto),if_needed(true)])),halt."
#swipl -g "time(load_files([packs_xtra/logicmoo_nlu/prolog/pldata/nldata_freq_pdat],[qcompile(auto),if_needed(true)])),halt."
#swipl -g "time(load_files([packs_xtra/logicmoo_nlu/prolog/pldata/nldata_BRN_WSJ_LEXICON],[qcompile(auto),if_needed(true)])),halt."
#swipl -g "time(load_files([packs_xtra/logicmoo_nlu/prolog/pldata/nldata_colloc_pdat],[qcompile(auto),if_needed(true)])),halt."
#swipl -g "time(load_files([packs_xtra/logicmoo_nlu/prolog/pldata/nldata_cycl_pos0],[qcompile(auto),if_needed(true)])),halt."
#swipl -g "time(qcompile(packs_sys/logicmoo_base/prolog/logicmoo/plarkc/logicmoo_u_cyc_kb_tinykb)),halt."
#swipl -g "time(load_files([packs_sys/logicmoo_base/prolog/logicmoo/plarkc/logicmoo_u_cyc_kb_tinykb],[qcompile(auto),if_needed(true)])),halt."
# echo "Compiling a 1gb file this might take about 5 minutes after this it will only take 6 seconds to load"
#swipl -g "time(load_files([packs_xtra/pldata_larkc/prolog/el_holds/el_assertions],[qcompile(auto),if_needed(true)])),halt."

#if ![ -f $STANFORD_JAR ]; then 
#    echo "Downloading $STANFORD_JAR ...";
#    wget http://prologmoo.com/downloads/stanford-corenlp3.5.2-ALL.jar -O $STANFORD_JAR
#fi


echo "to start the MUD type: ./startMUDServer.sh"
#su - prologmud_server

#echo git clone https://github.com/logicmoo/swipl-devel-unstable swipl-devel-unstable
#(cd swipl-devel-unstable ; ../build.unstable)
# (cd swipl-devel-unstable ; make clean ; make distclean ; ../build.unstable)

# Packs that are generally distributed
#echo ./bin/swipl -f $LOGICMOO_WS/.swiplrc  -g "absolute_file_name(packs_sys prologmud 

# Packs that are user focus
#echo ./bin/swipl -f $LOGICMOO_WS/.swiplrc  -g "absolute_file_name(packs_usr prologmud_samples 

# (Non)"Packs" that create the remote interface
## echo git clone --recursive https://github.com/logicmoo/swish-with-filesystem-editing packs_web/swish
## echo git clone --recursive https://github.com/logicmoo/ClioPatria-filessytem-and-clausedb packs_web/ClioPatria-filessytem-and-clausedb
## git clone --recursive https://github.com/logicmoo/plweb packs_web/plweb-realtime

# Very large packs are way beyond most peoples scope and interest (or just too random)
## echo git clone --recursive https://gitlab.logicmoo.org:8060/NomicMU/logicmoo_nlu/  packs_xtra/logicmoo_nlu/
## echo git clone --recursive https://gitlab.logicmoo.org:8060/NomicMU/logicmoo_planners/  packs_xtra/logicmoo_planners/
## echo git clone --recursive https://gitlab.logicmoo.org:8060/NomicMU/logicmoo_packages/  packs_xtra/logicmoo_packages/
## echo git clone --recursive https://gitlab.logicmoo.org:8060/NomicMU/logicmoo_experimental/  packs_xtra/logicmoo_experimental/


[submodule "wikis/logicmoo_workspace.wiki"]
	path = wikis/logicmoo_workspace.wiki
	url = https://github.com/logicmoo/logicmoo_workspace.wiki
[submodule "packs_lib/auc"]
	path = packs_lib/auc
	url = https://github.com/friguzzi/auc.git
[submodule "packs_lib/matrix"]
	path = packs_lib/matrix
	url = https://github.com/friguzzi/matrix.git
[submodule "packs_lib/cplint"]
	path = packs_lib/cplint
	url = https://github.com/friguzzi/cplint.git
[submodule "packs_lib/bddem"]
	path = packs_lib/bddem
	url = https://github.com/friguzzi/bddem.git
[submodule "packs_lib/aleph"]
	path = packs_lib/aleph
	url = https://github.com/friguzzi/aleph.git
[submodule "packs_lib/rocksdb"]
	path = packs_lib/rocksdb
	url = https://github.com/JanWielemaker/rocksdb.git
[submodule "packs_lib/mpi"]
	path = packs_lib/mpi
	url = https://github.com/friguzzi/mpi.git
[submodule "packs_lib/xlibrary"]
	path = packs_lib/xlibrary
	url = https://github.com/edisonm/xlibrary.git
[submodule "packs_lib/assertions"]
	path = packs_lib/assertions
	url = https://github.com/edisonm/assertions.git
[submodule "packs_lib/rtchecks"]
	path = packs_lib/rtchecks
	url = https://github.com/edisonm/rtchecks.git
[submodule "packs_lib/xtools"]
	path = packs_lib/xtools
	url = https://github.com/edisonm/xtools.git
[submodule "packs_lib/cplint_r"]
	path = packs_lib/cplint_r
	url = https://github.com/friguzzi/cplint_r.git
[submodule "packs_lib/phil"]
	path = packs_lib/phil
	url = https://github.com/ArnaudFadja/phil.git
[submodule "packs_lib/trill"]
	path = packs_lib/trill
	url = https://github.com/rzese/trill.git
[submodule "packs_sys/logicmoo_base"]
	path = packs_sys/logicmoo_base
	url = https://github.com/logicmoo/logicmoo_base.git
[submodule "packs_sys/logicmoo_cg"]
	path = packs_sys/logicmoo_cg
	url = https://github.com/logicmoo/logicmoo_cg.git
[submodule "packs_sys/logicmoo_ec"]
	path = packs_sys/logicmoo_ec
	url = https://github.com/logicmoo/logicmoo_ec.git
[submodule "packs_sys/logicmoo_nlu"]
	path = packs_sys/logicmoo_nlu
	url = https://github.com/logicmoo/logicmoo_nlu.git
[submodule "packs_sys/prologmud"]
	path = packs_sys/prologmud
	url = https://github.com/logicmoo/prologmud.git
[submodule "packs_sys/prologmud_samples"]
	path = packs_sys/prologmud_samples
	url = https://github.com/logicmoo/prologmud_samples.git
[submodule "packs_sys/wam_common_lisp"]
	path = packs_sys/wam_common_lisp
	url = https://github.com/logicmoo/wam_common_lisp.git
[submodule "packs_sys/lps_corner"]
	path = packs_sys/lps_corner
	url = https://github.com/logicmoo/lps_corner.git
[submodule "packs_web/ClioPatria"]
	path = packs_web/ClioPatria
	url = https://github.com/logicmoo/ClioPatria
[submodule "packs_sys/logicmoo_utils"]
	path = packs_sys/logicmoo_utils
	url = https://github.com/logicmoo/logicmoo_utils.git
[submodule "packs_sys/dictoo"]
	path = packs_sys/dictoo
	url = https://github.com/logicmoo/dictoo.git
[submodule "packs_sys/gvar_syntax"]
	path = packs_sys/gvar_syntax
	url = https://github.com/logicmoo/gvar_syntax.git
[submodule "packs_sys/predicate_streams"]
	path = packs_sys/predicate_streams
	url = http://github.com/logicmoo/predicate_streams.git
[submodule "packs_sys/multimodal_dcg"]
	path = packs_sys/multimodal_dcg
	url = https://github.com/logicmoo/multimodal_dcg.git
[submodule "packs_sys/pfc"]
	path = packs_sys/pfc
	url = https://github.com/logicmoo/pfc.git
[submodule "packs_sys/logicmoo_nars"]
	path = packs_sys/logicmoo_nars
	url = https://github.com/logicmoo/logicmoo_nars
[submodule "packs_web/swish"]
	path = packs_web/swish
	url = https://github.com/logicmoo/swish
[submodule "packs_web/logicmoo_webui"]
	path = packs_web/logicmoo_webui
	url = https://github.com/logicmoo/logicmoo_webui.git
[submodule "packs_sys/body_reordering"]
	path = packs_sys/body_reordering
	url = https://github.com/logicmoo/body_reordering.git
[submodule "packs_sys/eggdrop"]
	path = packs_sys/eggdrop
	url = https://github.com/logicmoo/eggdrop.git
[submodule "packs_sys/slack_prolog"]
	path = packs_sys/slack_prolog
	url = https://github.com/swi-to-yap/slack_prolog
[submodule "packs_sys/swicli"]
	path = packs_sys/swicli
	url = https://github.com/logicmoo/swicli.git
[submodule "packs_lib/sldnfdraw"]
	path = packs_lib/sldnfdraw
	url = https://github.com/gavanelli/sldnfdraw.git
[submodule "packs_xtra/logicmoo_pldata"]
	path = packs_xtra/logicmoo_pldata
	url = https://logicmoo.org/gitlab/logicmoo/logicmoo_pldata.git
[submodule "wiki"]
	path = wiki
	url = https://logicmoo.org/gitlab/logicmoo/logicmoo_wiki.git
