#!/bin/bash

set +x +e


if [[ $EUID -ne 0 ]]; then
   echo "#* "
   echo -e "\e[1;31mERROR This script must be run as root. \e[0m"
   echo "#* "
   return 1 2>/dev/null
   exit 1
fi

apt install -y git screen docker docker.io

DIR0="$( cd "$( dirname "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )"
DIR0=/opt/logicmoo_workspace

if [ "${TERM}" == "screen" ]; then
   echo "#* "
   echo "Good we are already in screen"
   echo "#* "
else
   echo "#* "
   screen -list
   echo "#* "
  # screen -m ${BASH_SOURCE[0]} $*
  # return 0 2>/dev/null
  # exit 0
fi

DOCKER_COMPOSE=1

(
cd $DIR0

export LOGICMOO_WS=$DIR0

./logicmoo_env.sh

#find $LOGICMOO_WS/?*/ -type d -exec chmod 777 "{}" + 
#chmod a+w -R $LOGICMOO_WS/?*/

if [ "${2}" == "commit" ]; then
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



if [ "${1}" == "build" ]; then
   DOCKER_COMPOSE=0

   (
      set +e +x
      cd docker
      docker build $EXTRA -t logicmoo/logicmoo_starter_image . 

      if [ "${2}" == "commit" ]; then
         docker push logicmoo/logicmoo_starter_image
      else
         echo MAYBE: docker push logicmoo/logicmoo_starter_image
      fi
      
   )
   
   
   docker build $EXTRA -t logicmoo/logicmoo_workspace .

   if [ "${2}" == "commit" ]; then
      docker push logicmoo/logicmoo_workspace
   else
      echo MAYBE: docker push logicmoo/logicmoo_workspace
   fi

fi

docker kill logicmoo 2>/dev/null ; /bin/true
docker container rm logicmoo 2>/dev/null ; /bin/true
docker kill logicmoo 2>/dev/null ; /bin/true
docker container rm logicmoo 2>/dev/null ; /bin/true
docker kill logicmoo 2>/dev/null ; /bin/true
docker ps

export PORTS="4000-4199:4000-4199 -p 4243:443 -p 4280:80 -p 3020:3020 -p 4222:22 -p 4220:3020 -p 4200:5900 -p 4201:9001 -p 4290:4090 -p 6079-6081:6079-6081"

export LM_VOLUMES="-v /opt/logicmoo_workspace:/opt/logicmoo_workspace"
export DOCKER_RUN="--name logicmoo --privileged=true --no-healthcheck $LM_VOLUMES --rm -it -p $PORTS $EXTRA logicmoo/logicmoo_workspace:latest"
export DOCKER_UP=""

if [ "$(hostname -d)" == "logicmoo.org" ]; then
   DOCKER_RUN="$DOCKER_RUN --add-host logicmoo.org:10.0.0.194"
   echo "locally testing on logicmoo.org"
  # docker run --name logicmoo --privileged=true --no-healthcheck --add-host logicmoo.org:10.0.0.194 --rm -it -p 4000-4199:4000-4199 -p 4243:443 -p 4280:80 -p 3020:3020 -p 4222:22 -p 4220:3020 -p 4200:5900 -p 4201:9001 -p 4290:4090 -p 6079-6081:6079-6081  logicmoo/logicmoo_workspace:latest
  # return 0 2>/dev/null
  # exit 0
fi


echo "docker-compose up $DOCKER_UP"
echo "docker run $DOCKER_RUN"
if [ "$DOCKER_COMPOSE" == "1" ]; then
#   docker-compose up $DOCKER_UP
  docker run $DOCKER_RUN
fi

)
