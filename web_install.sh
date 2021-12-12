#!/bin/bash

# Installs with 
#   source <(curl -sS https://logicmoo.org/gitlab/logicmoo/logicmoo_workspace/-/raw/master/web_install.sh)
#   source <(curl -sS https://raw.githubusercontent.com/logicmoo/logicmoo_workspace/master/web_install.sh)

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
  git clone --no-checkout https://github.com/logicmoo/logicmoo_workspace.git
  git config --global http.sslVerify $SSLWAS
  (
  cd logicmoo_workspace
  ggID='1KhXSv4vq_a82ctGg74GcVBO4fArldVou'
  ggURL='https://drive.google.com/uc?export=download'
  filename="$(curl -sc /tmp/gcokie "${ggURL}&id=${ggID}" | grep -o '="uc-name.*</span>' | sed 's/.*">//;s/<.a> .*//')"
  getcode="$(awk '/_warning_/ {print $NF}' /tmp/gcokie)"
  curl -Lb /tmp/gcokie "${ggURL}&confirm=${getcode}&id=${ggID}" -o "${filename}"
  tar xfvz "${filename}" -C .git/modules/prologmud_server
  git checkout origin/master
  git checkout master
  git submodule update --init --recursive
  git submodule update --recursive --remote
  )
fi

# ls logicmoo_workspace
cd logicmoo_workspace

git pull -f --recursive
git status -s

source ./INSTALL.md

echo -e "\e[1;32m If Docker is installed: $LOGICMOO_WS/runFromDocker.sh
         (otherwise: source $LOGICMOO_WS/StartLogicmoo.sh)\e[0m"

)
