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

(
cd $DIR0

export LOGICMOO_WS=$DIR0

./logicmoo_env.sh

#find $LOGICMOO_WS/?*/ -type d -exec chmod 777 "{}" + 
#chmod a+w -R $LOGICMOO_WS/?*/

if [ "${2}" == "commit" ]; then

   git remote add --track master github https://github.com/logicmoo/logicmoo_workspace.git 2>/dev/null ; /bin/true
   git remote add --track master gitlab https://logicmoo.org/gitlab/logicmoo/logicmoo_workspace.git 2>/dev/null ; /bin/true

   echo "Scanning changes for GIT ..."
   git status -s

   git submodule foreach 'git commit -am "Docker $(date)" ; git push origin HEAD:master ; SUBM=$(basename `pwd`) ; echo $SUBM  ; cd .. ; git add $SUBM  ; /bin/true'
   git commit -am "Docker $(date)"
   git push github master

fi

docker container rm logicmoo
docker kill logicmoo 2>/dev/null ; /bin/true
docker container rm logicmoo

if [ "${1}" == "build" ]; then


   docker kill logicmoo 2>/dev/null ; /bin/true

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
docker ps

export DOCKER_RUN="--name logicmoo --privileged=true --no-healthcheck -v /opt/logicmoo_workspace:/opt/logicmoo_workspace --rm -it -p 4000-4440:4000-4440 -p 4443:443 -p 3020:3020 $EXTRA logicmoo/logicmoo_workspace:latest"

echo "docker run $DOCKER_RUN"
docker run $DOCKER_RUN

)
