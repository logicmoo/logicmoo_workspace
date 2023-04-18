#!/bin/bash

docker build . -t logicmoo/archathon
docker push logicmoo/archathon
echo docker run -it --rm -v /etc -v logs:/var/log --mount type=bind,source="$(pwd)"/secret_data,target=/data --mount type=bind,source=/opt,target=/opt logicmoo/archathon bash

