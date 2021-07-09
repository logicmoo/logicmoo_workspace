# LogicMOO's Parent Project 

Documentation  http://logicmoo.org/xwiki/

=========

# Install/Run  Methods


## From web-installer 
```bash
source <(curl -sS https://raw.githubusercontent.com/logicmoo/logicmoo_workspace/master/web_install.sh)
cd /opt/logicmoo_workspace
bash ./StartLogicmoo.sh
```

## Developer's Docker 
(Copies the git repo where things can be edited for development purposes. while running in Docker)
```bash
cd /opt
git clone https://logicmoo.org/gitlab/logicmoo/logicmoo_workspace.git
cd logicmoo_workspace
bash ./runFromDocker.sh -d  # the -d is for running detatched
```

## Old Way (might work with older versions, but it can very much damage/alter your system)
```bash
cd /opt
git clone --recursive https://logicmoo.org/gitlab/logicmoo/logicmoo_workspace.git
cd logicmoo_workspace
source ./INSTALL.md
./StartLogicmoo.sh

```

```
rsync -chaPz -vv --progress --stats /opt/logicmoo_workspace jacobpdq@logicmoo.org:/opt/logicmoo_workspace
```

# Douglas' pastebin
```
docker kill logicmoo
docker exec -it $(docker ps -n 1 -q) bash
docker network create -d macvlan --subnet=10.0.0.0/24 --gateway=10.0.0.1 -o parent=eth0 pub_net
docker run ./runFromDocker.sh --network="logicmoo_workspace_prologmud_vlan"
docker kill $(docker ps -a -q)
docker image prune --all -f
docker rmi logicmoo/logicmoo_starter_image:latest
```

## Docker without installing (poor choice as it has to download and build everything from docker which can take an hour and fail)
```bash
docker run -it --name logicmoo --privileged=true -p 4000-4440:4000-4440 -p 4443:443 -p 3020:3020 logicmoo/logicmoo_workspace:latest
```

OLD Doc urls
Initial Docs https://github.com/logicmoo/logicmoo_workspace/wiki

As well as https://docs.google.com/document/d/1fkOxnmI1LqxadvZuCRS-fGIEweIKyPn6AVGp5Yjse1I/edit





