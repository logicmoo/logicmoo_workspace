docker ps -a
echo docker image prune -a -f
echo docker container prune -f
echo docker image prune -a -f
echo docker ps -a
echo ./make-docker.sh --add-host logicmoo.org:10.0.0.194 $@



