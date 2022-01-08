#!/bin/bash

/dockerstartup/vnc_startup.sh echo "Starting"
cd /home/opencog
echo "Starting jupyter"
jupyter lab --ip 0.0.0.0 --no-browser
