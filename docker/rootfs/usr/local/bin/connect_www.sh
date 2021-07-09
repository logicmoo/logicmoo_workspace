#!/bin/bash
set -o pipefail

source /opt/logicmoo_workspace/logicmoo_env.sh

sudo prologmud_server -- google-chrome "http://localhost"

return 0 2>/dev/null
exit 0
