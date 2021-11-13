@echo on
docker container rm logicmoo
docker pull logicmoo/logicmoo_workspace:latest
@SET DOCKER_RUN=docker run
@SET DOCKER_RUN=%DOCKER_RUN% --privileged=true
@SET DOCKER_RUN=%DOCKER_RUN% --rm -it
@SET DOCKER_RUN=%DOCKER_RUN% --no-healthcheck
@SET DOCKER_RUN=%DOCKER_RUN% -v %CD%:/opt/logicmoo_workspace
@SET DOCKER_RUN=%DOCKER_RUN% --name logicmoo
@SET DOCKER_RUN=%DOCKER_RUN% -p 4000-4004:4000-4004 -p 4021-4025:4021-4025 -p 4243:443 -p 4280:80 -p 4020:3020  -p 3020:3020 -p 4222:22 -p 4220:3020 -p 4200:5900 -p 4201:9001 -p 4290-4299:4090-4099 -p 6079-6081:6079-6081
@SET DOCKER_RUN=%DOCKER_RUN% logicmoo/logicmoo_workspace:latest
%DOCKER_RUN%
