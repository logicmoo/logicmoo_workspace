#!/bin/bash
# Laxy error handling...: exit if anything goes wrong:
set -e
LPS_SHA1=$(git log -1 --pretty=%H)
docker build --build-arg LPS_SHA1=${LPS_SHA1} -t logicalcontracts/lps.swi .
docker push logicalcontracts/lps.swi
# Probably not a good idea, tagging the image with our git hash:
#docker build --build-arg LPS_SHA1=${LPS_SHA1} -t logicalcontracts/lps.swi:${LPS_SHA1} -t logicalcontracts/lps.swi:latest .
#docker push logicalcontracts/lps.swi:${LPS_SHA1}
#docker push logicalcontracts/lps.swi:latest
