#!/bin/bash

# Installs with 
#   source <(curl -sS https://logicmoo.org/gitlab/logicmoo/logicmoo_workspace/-/raw/master/web_install.sh)
#   source <(curl -sS https://raw.githubusercontent.com/logicmoo/logicmoo_workspace/master/web_install.sh)

if [[ $EUID -ne 0 ]]; then
   echo ""
   echo -e "\e[1;31mERROR This script must be run as root. \e[0m"
   echo ""
   return 1 2>/dev/null
   exit 1
fi

apt install -y git screen docker docker.io

if git --version &>/dev/null; then
   echo "Found Git"; 
else
echo ""
   echo ""
   echo -e "\e[1;31mERROR Require git but it's not installed.  Aborting. \e[0m"
   echo ""
   return 1 2>/dev/null
   exit 1
fi

mkdir -p /opt
(
cd /opt
if [ ! -d "logicmoo_workspace" ]; then
  export SSLWAS=$(git config --global http.sslVerify)
  git config --global http.sslVerify false
  git clone --no-remote-submodules https://github.com/logicmoo/logicmoo_workspace.git
  git config --global http.sslVerify $SSLWAS
  ggID='1KhXSv4vq_a82ctGg74GcVBO4fArldVou'
  ggURL='https://drive.google.com/uc?export=download'
  filename="$(curl -sc /tmp/gcokie "${ggURL}&id=${ggID}" | grep -o '="uc-name.*</span>' | sed 's/.*">//;s/<.a> .*//')"
  getcode="$(awk '/_warning_/ {print $NF}' /tmp/gcokie)"
  curl -Lb /tmp/gcokie "${ggURL}&confirm=${getcode}&id=${ggID}" -o "${filename}"
fi

ls logicmoo_workspace
cd logicmoo_workspace
git pull -f
git status -s

(source ./INSTALL.md)

echo -e "\e[1;32m If Docker is installed: $LOGICMOO_WS/runFromDocker.sh
         (otherwise: source $LOGICMOO_WS/StartLogicmoo.sh)\e[0m"

)
