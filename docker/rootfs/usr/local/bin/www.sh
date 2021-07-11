#!/bin/bash

#--help # Display LOGICMOO WWW in a web browser

if [ ! -f /.dockerenv ]; then
   docker exec -it logicmoo bin/$(basename "${BASH_SOURCE[0]}") $*
   return 0 2>/dev/null
   exit 0
fi

set -o pipefail

source /opt/logicmoo_workspace/logicmoo_env.sh

URL="http://localhost"
if [$# -neq 0];  then 
URL=$*
fi

# sudo -u prologmud_server -- google-chrome $URL
sudo -u prologmud_server -- firefox $URL

return 0 2>/dev/null
exit 0
