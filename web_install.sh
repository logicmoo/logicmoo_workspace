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
  git clone https://github.com/logicmoo/logicmoo_workspace.git
  git config --global http.sslVerify $SSLWAS
fi

ls logicmoo_workspace
cd logicmoo_workspace
git pull -f
git status -s

(source ./INSTALL.md)

echo -e "\e[1;32m If Docker is installed: /opt/logicmoo_workspace/runFromDocker.sh
         (otherwise: source /opt/logicmoo_workspace/StartLogicmoo.sh)\e[0m"

)
