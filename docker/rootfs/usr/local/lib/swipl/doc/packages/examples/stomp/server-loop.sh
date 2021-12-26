#!/bin/bash
#
# Kill our stress_server.pl script every 0.5 seconds to test recovery.

trap exit SIGINT

while true; do
  timeout 0.5 swipl stress_server.pl
done
