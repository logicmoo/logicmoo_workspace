

SET CONTAINER=logicmoo
SET IMAGE=logicmoo/logicmoo_workspace:latest

Title "docker kill %CONTAINER%"
rem docker kill %CONTAINER%
rem docker kill %CONTAINER%
Title "docker pull %IMAGE%"
rem docker pull %IMAGE%

SET CALL=docker run --name %CONTAINER% --privileged=true --no-healthcheck --rm -it -p 4000-4199:4000-4199 -p 4243:443 -p 4280:80 -p 3020:3020 -p 4222:22 -p 4220:3020 -p 4200:5900 -p 4201:9001 -p 4290:4090 -p 6079-6081:6079-6081 %IMAGE%
Title "%CALL%"
echo %CALL%
rem start http://172.17.0.2
%CALL%
wait 10000

