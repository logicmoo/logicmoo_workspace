# LogicMOO's Parent Project 

Documentation  http://logicmoo.org/xwiki/

=========

# Install/Run  Methods


## From web-installer 
```bash
source <(curl -sS https://raw.githubusercontent.com/logicmoo/logicmoo_workspace/master/web_install.sh)
cd /opt/logicmoo_workspace
./StartLogicmoo.sh
```

## Developer's Docker 
(Copies the git repo where things can be edited for development purposes. while running in Docker)
```bash
cd /opt
git clone https://logicmoo.org/gitlab/logicmoo/logicmoo_workspace.git
cd logicmoo_workspace
./runFromDocker.sh
```

## RSync
```bash
mkdir -p /opt/logicmoo/.git
rsync -chaPlz -vv --progress rsync://logicmoo.org:12000/git /opt/logicmoo/.git
cd /opt/logicmoo/
git pull --recurse-submodule
./INSTALL.md
./StartLogicmoo.sh
```

# Running

## When `./StartLogicmoo.sh` is ran a few minutes later you will see 
```bash
MAYBE (IN OTHER TERMINAL):  docker exec -it logicmoo sudo -u prologmud_server -- screen -rx LogicmooServer
OR (Bash IN OTHER TERMINAL):  docker exec -it logicmoo bash
```  (in green every few minutes)

## Open a terminal new terminal and run
```bash
root@gitlab:/opt/logicmoo_workspace# lm
#*
#* DISPLAY=10.0.0.78:0.0
Finding/Setting LIBJVM...
#* LIBJVM=/usr/lib/jvm/java-11-openjdk-amd64/lib/server
Finding/Setting LD_LIBRARY_PATH...
#* LD_LIBRARY_PATH=/usr/lib/jvm/java-11-openjdk-amd64/lib/server:/usr/local/lib

 Executes one of the following commands in the LOGICMOO docker.

   lm [--no-x] [--no-env] [-v|-q] [--wd <path>] [--] <cmd> [<args>]

   ansi                  # Opens LOGICMOO Main Termninal
   emacs                 # Opens IDE in a web browser
   bfly                  # Opens LOGICMOO Main Termninal in web brower
   telnet                # Connect via Telnet
   www                   # Display LOGICMOO WWW in a web browser
   cls                   # Clear the SCREEN

```



# Douglas' pastebin 
Ignore the reset of this
```bash
docker kill logicmoo
docker exec -it $(docker ps -n 1 -q) bash
docker network create -d macvlan --subnet=10.0.0.0/24 --gateway=10.0.0.1 -o parent=eth0 pub_net
docker run ./runFromDocker.sh --network="logicmoo_workspace_prologmud_vlan"
docker kill $(docker ps -a -q)
docker image prune --all -f
docker rmi logicmoo/logicmoo_starter_image:latest
```

## Old Way to Install (will now damage system if ran outside of docker)
```bash
cd /opt
git clone --recursive https://logicmoo.org/gitlab/logicmoo/logicmoo_workspace.git
cd logicmoo_workspace
./INSTALL.md
./StartLogicmoo.sh
```


## Docker without installing (poor choice as it has to download and build everything from docker which can take an hour and fail)
```bash
docker run -it --name logicmoo --privileged=true -p 4000-4440:4000-4440 -p 4443:443 -p 3020:3020 logicmoo/logicmoo_workspace:latest
```

OLD Doc urls
Initial Docs https://github.com/logicmoo/logicmoo_workspace/wiki

As well as https://docs.google.com/document/d/1fkOxnmI1LqxadvZuCRS-fGIEweIKyPn6AVGp5Yjse1I/edit



