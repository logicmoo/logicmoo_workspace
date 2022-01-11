#!/bin/bash

export LM_PORTS="-p 5901:5901 -p 5900:5900 -p 4018:8188 -p 4180:1800 -p 4188:8888 -p 4000-4004:4000-4004 -p 4021-4025:4021-4025 -p 4100-4125:4100-4125 -p 4090-4099:4090-4099 -p 4243:443 -p 4280:80 -p 4020:3020  -p 3020:3020 -p 4222:22 -p 4220:3020 -p 4200:5900 -p 4201:9001 -p 4290:4090 -p 6079-6081:6079-6081"
export LM_VOLUMES="-v /opt/logicmoo_workspace:/opt/logicmoo_workspace"
export EXTRA=''
export DOCKER_RUN="--privileged=true --no-healthcheck $LM_VOLUMES --rm -it ${LM_PORTS} ${EXTRA}"
export DOCKER_UP=""
export RUN_IMAGE=logicmoo/logicmoo_workspace:latest

if [ "$(hostname -d)" == "logicmoo.org" ]; then
  export DOCKER_RUN="--privileged=true --no-healthcheck $LM_VOLUMES --rm -it ${LM_PORTS} ${EXTRA}"
  DOCKER_RUN="$DOCKER_RUN --add-host logicmoo.org:10.0.0.194"
  RUN_IMAGE=logicmoo/logicmoo_server_32gb:latest
  echo "locally testing on logicmoo.org"
fi


if [ -f "/.dockerenv" ]; then
   ./StartLogicmoo.sh

   return 0 2>/dev/null
   exit 0
fi

#set +x +e

if [[ $EUID -ne 0 ]]; then
   echo "#* "
   echo -e "\e[1;31mWARNING This script should be run as root. \e[0m"
   echo "#* "
   #return 1 2>/dev/null
   #exit 1
fi

#[ ! -x git ] && apt install -y git
#[ ! -x docker ] && apt install -y docker.io
#[ ! -x screen ] && apt install -y screen

DIR0="$( cd "$( dirname "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )"
#DIR0=/opt/logicmoo_workspace
ROOT_DIRECTORY=$DIR0

if [ "${TERM}" == "screen" ]; then
   echo "#* "
   echo "Good we are already in screen"
   echo "#* "
else
   echo "#* "
   screen -list
   echo "#* "
  screen -m ${BASH_SOURCE[0]} $*
  return 0 2>/dev/null ; exit 0
fi


run=1

for arg in "$@"
do
    case $arg in
       # --help|-h)
       # show_lm_help
       # return 0 2>/dev/null ; exit 0
       # ;;
       # --no-x)
       # NOX=1
       # shift
       # ;;
        --no-env)
        LMENV=0
        shift
        ;;
        -v|-q|-x|-e)
        LMENV_ARG="${LMENV_ARG} $1"
        shift
        ;;
        -d|--wd)
        ROOT_DIRECTORY="$2"
        shift # Remove argument name from processing
        shift # Remove argument value from processing
        ;;
        --) shift
        break;;
        *)
        export run=0
        export $1=1
        shift;;
    esac
done

(
cd $DIR0

export LOGICMOO_WS=$DIR0

if [ "${LMENV}" == "1" ]; then
./logicmoo_env.sh
fi

if [ "${commit}" == "1" ]; then
   DOCKER_COMPOSE=0
   git config --local http.sslVerify false
   git config credential.helper 'cache --timeout=300000'
   git update-index --assume-unchanged prologmud_server/.bash_history
   git update-index --assume-unchanged packs_sys/eggdrop/conf/PrologMUD-freenode.chan
   git update-index --assume-unchanged packs_sys/eggdrop/conf/PrologMUD-freenode.user
   git remote add --track master github https://github.com/logicmoo/logicmoo_workspace.git 2>/dev/null ; /bin/true
   git remote add --track master gitlab https://logicmoo.org/gitlab/logicmoo/logicmoo_workspace.git 2>/dev/null ; /bin/true

   echo "Scanning changes for GIT ..."
   git status -s

   git submodule foreach 'git commit -am "Docker $(date)" ; git push origin HEAD:master ; SUBM=$(basename `pwd`) ; echo $SUBM  ; cd .. ; git add $SUBM  ; /bin/true'
   git commit -am "Docker $(date)"
   git push github master

fi



if [ "${build_fresh}" == "1" ]; then
     set +e +x
     cd $LOGICMOO_WS
     docker build -f docker/Dockerfile $EXTRA -t logicmoo/logicmoo_starter_image . 
     echo MAYBE: docker push logicmoo/logicmoo_starter_image
   cd $LOGICMOO_WS
   export build=1
fi

if [ "${push}" == "1" ]; then
     docker push logicmoo/logicmoo_starter_image      
fi

if [ "${build_2}" == "1" ]; then
   set +e +x
   cd $LOGICMOO_WS

   tmp_img=logicmoo/logicmoo_starter_image
   docker build $EXTRA -t $tmp_img . 
   docker run --name logicmoo_builder $DOCKER_RUN --entrypoint /bin/bash $tmp_img
   docker commit logicmoo_builder logicmoo/logicmoo_workspace:latest
   docker rm "$tmp_container"
   #docker build $EXTRA -t logicmoo/logicmoo_workspace .
   echo MAYBE: docker push logicmoo/logicmoo_workspace
   return 0 2>/dev/null ; exit 0
fi

if [ "${build}" == "1" ]; then
   set +e +x
   cd $LOGICMOO_WS
   docker build $EXTRA -t logicmoo/logicmoo_workspace .
   echo MAYBE: docker push logicmoo/logicmoo_workspace
fi

if [ "${push}" == "1" ]; then
   docker push logicmoo/logicmoo_workspace
fi

if [ "${build}" == "1" ]; then
     set +e +x
     cd $LOGICMOO_WS
     docker build -f docker/Dockerfile.32gb $EXTRA -t logicmoo/logicmoo_server_32gb . 
     echo MAYBE: docker push logicmoo/logicmoo_server_32gb
     cd $LOGICMOO_WS
fi


if [ "$run" == "1" ]; then
      #set +x -e
      docker kill logicmoo 2>/dev/null ; /bin/true
      sleep 1
      docker container rm logicmoo 2>/dev/null ; /bin/true
      sleep 1
      docker kill logicmoo 2>/dev/null ; /bin/true
      docker container rm logicmoo 2>/dev/null ; /bin/true
      docker kill logicmoo 2>/dev/null ; /bin/true
      docker ps
fi

if [ "$run" == "1" ]; then
   docker kill logicmoo 2>/dev/null ; /bin/true
   docker container rm logicmoo 2>/dev/null ; /bin/true
   docker kill logicmoo 2>/dev/null ; /bin/true
   docker container rm logicmoo 2>/dev/null ; /bin/true
   docker kill logicmoo 2>/dev/null ; /bin/true
   docker ps
   
   echo "docker-compose up $DOCKER_UP"
   echo "docker run $DOCKER_RUN"
   if [ "$compose" == "1" ]; then
     docker-compose up $DOCKER_UP
   else 
     docker run --name=logicmoo $DOCKER_RUN $RUN_IMAGE
    fi
fi

)
