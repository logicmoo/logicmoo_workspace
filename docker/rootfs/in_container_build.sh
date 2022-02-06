#!/bin/bash

set +x

#set +e

if [ -f "/tmp/is_google_collab" ]; then
 if [ "${TERM}" == "screen" ]; then
   echo "#* "
   echo "Good we are already in screen"
   echo "#* "
 else
   echo "#* "
   screen -list
   echo "#* "
  screen -m ${BASH_SOURCE[0]} $*
  return 0 2>/dev/null ; exit 0
 fi
fi


if [ -n "$VNC_PASSWORD" ]; then
    echo -n "$VNC_PASSWORD" > /.password1
    x11vnc -storepasswd $(cat /.password1) /.password2
    chmod 400 /.password*
    sed -i 's/^command=x11vnc.*/& -rfbauth \/.password2/' /etc/supervisor/conf.d/supervisord.conf
    export VNC_PASSWORD=
fi

if [ -n "$X11VNC_ARGS" ]; then
    sed -i "s/^command=x11vnc.*/& ${X11VNC_ARGS}/" /etc/supervisor/conf.d/supervisord.conf
fi

if [ -n "$OPENBOX_ARGS" ]; then
    sed -i "s#^command=/usr/bin/openbox\$#& ${OPENBOX_ARGS}#" /etc/supervisor/conf.d/supervisord.conf
fi

if [ -n "$RESOLUTION" ]; then
    sed -i "s/1024x768/$RESOLUTION/" /usr/local/bin/xvfb.sh
fi

USER=${USER:-root}
HOME=/root
if [ "$USER" != "root" ]; then
    echo "* enable custom user: $USER"
    useradd --create-home --shell /bin/bash --user-group --groups adm,sudo $USER
    if [ -z "$PASSWORD" ]; then
        echo "  set default password to \"ubuntu\""
        PASSWORD=ubuntu
    fi
    HOME=/home/$USER
    echo "$USER:$PASSWORD" | chpasswd
    cp -r /root/{.config,.gtkrc-2.0,.asoundrc} ${HOME}
    chown -R $USER:$USER ${HOME}
    [ -d "/dev/snd" ] && chgrp -R adm /dev/snd
fi
sed -i -e "s|%USER%|$USER|" -e "s|%HOME%|$HOME|" /etc/supervisor/conf.d/supervisord.conf

# home folder
if [ ! -x "$HOME/.config/pcmanfm/LXDE/" ]; then
    mkdir -p $HOME/.config/pcmanfm/LXDE/
    ln -sf /usr/local/share/doro-lxde-wallpapers/desktop-items-0.conf $HOME/.config/pcmanfm/LXDE/
    chown -R $USER:$USER $HOME
fi

# nginx workers
sed -i 's|worker_processes .*|worker_processes 1;|' /etc/nginx/nginx.conf

# nginx ssl
if [ -n "$SSL_PORT" ] && [ -e "/etc/nginx/ssl/nginx.key" ]; then
    echo "* enable SSL"
	sed -i 's|#_SSL_PORT_#\(.*\)443\(.*\)|\1'$SSL_PORT'\2|' /etc/nginx/sites-enabled/default
	sed -i 's|#_SSL_PORT_#||' /etc/nginx/sites-enabled/default
fi

# nginx http base authentication
if [ -n "$HTTP_PASSWORD" ]; then
    echo "* enable HTTP base authentication"
    htpasswd -bc /etc/nginx/.htpasswd $USER $HTTP_PASSWORD
	sed -i 's|#_HTTP_PASSWORD_#||' /etc/nginx/sites-enabled/default
fi

# dynamic prefix path renaming
if [ -n "$RELATIVE_URL_ROOT" ]; then
    echo "* enable RELATIVE_URL_ROOT: $RELATIVE_URL_ROOT"
	sed -i 's|#_RELATIVE_URL_ROOT_||' /etc/nginx/sites-enabled/default
	sed -i 's|_RELATIVE_URL_ROOT_|'$RELATIVE_URL_ROOT'|' /etc/nginx/sites-enabled/default
fi
	
# clearup
PASSWORD=
HTTP_PASSWORD=

cp /dockerstartup/generate_container_user /generate_container_user

export DEBIAN_FRONTEND=noninteractive
export LOGICMOO_WS=/opt/logicmoo_workspace
export DO_PULL=0

if [ ! -d $LOGICMOO_WS/ ]; then
if grep -qs "$LOGICMOO_WS" /proc/mounts; then
     echo "$LOGICMOO_WS already mounted."
     DO_PULL=0
else
   echo "$LOGICMOO_WS is not mounted."
   export SHARED_SERVER=$(route -n | awk '/UG[ \t]/{print $2}')
   #export DISPLAY=$SHARED_SERVER:0.0
   
   if ping -c 1 -W 1 "$SHARED_SERVER"; then
      echo "$SHARED_SERVER is UP"
      service rpcbind start
      service nfs-common start
      mkdir -p $LOGICMOO_WS
      echo "trying to mount..." 
      mount $SHARED_SERVER:$LOGICMOO_WS $LOGICMOO_WS
      if [ $? -eq 0 ]; then
         echo "Success mount $SHARED_SERVER:$LOGICMOO_WS $LOGICMOO_WS !"
         DO_PULL=0
        else
         echo "Something went wrong with the mount..."
         mount $SHARED_SERVER:$LOGICMOO_WS $LOGICMOO_WS -v ; /bin/true
         rmdir $LOGICMOO_WS
      fi
   else
      echo "$SHARED_SERVER is not local"
   fi

fi
fi


# check out our repo
if [[ ! -d $LOGICMOO_WS/.git ]]
then
 cd /opt
 git config --global http.sslVerify false
 GIT_CLONE="clone --recurse-submodules https://github.com/logicmoo/logicmoo_workspace"
 echo git $GIT_CLONE
 git $GIT_CLONE
 find $LOGICMOO_WS/ -type d -exec chmod 777 {} +
 DO_PULL=0
fi

cd $LOGICMOO_WS
if [ $DO_PULL -gt 0 ]; then 
   echo "git checkout master ."
   git checkout master .
else 
   echo "Skipping pull"
fi

rsync -ra $LOGICMOO_WS/docker/rootfs/. /.

bash -x /mounted_fs_build.sh


find /opt/logicmoo_workspace/packs_???/ -maxdepth 3 -name setup.py -execdir bash -c "pip install --target /usr/local/lib/python3.10/dist-packages --no-deps --no-compile -e ." \;
mv /home/opencog/.local/lib/python3.10 /home/opencog/.local/lib/python3.10-orig
ln -s /usr/local/lib/python3.10 /home/opencog/.local/lib/python3.10


echo "Starting . $LOGICMOO_WS/INSTALL.md"
cd $LOGICMOO_WS
bash -x $LOGICMOO_WS/INSTALL.md


mkdir -p /home/ubuntu
mkdir -p /home/opencog

cp ~prologmud_server/.guile ~root/.guile

if [ ! -d "/root.wrong" ]; then
   mv /root /root.wrong
   mv /home/opencog /home/opencog.wrong
   mv /home/ubuntu /home/ubuntu.wrong
   ln -s ~prologmud_server /home/opencog
   ln -s ~prologmud_server /root
   ln -s ~prologmud_server /home/ubuntu
fi

if [ ! -d "/etc/apache2/conf-available.dist" ]; then
   mv /etc/apache2/conf-available /etc/apache2/conf-available.dist
   rm -f /etc/apache2/conf-available
   ln -s $LOGICMOO_WS/docker/rootfs/etc/apache2/conf-available /etc/apache2/conf-available 
fi

if [ ! -d "/etc/apache2/sites-enabled.dist" ]; then
   mv /etc/apache2/sites-enabled /etc/apache2/sites-enabled.dist
   rm -f /etc/apache2/sites-enabled
   ln -s $LOGICMOO_WS/docker/rootfs/etc/apache2/sites-enabled /etc/apache2/sites-enabled
fi

if [ ! -d "/etc/apache2/ssl.dist" ]; then
   mv /etc/apache2/ssl /etc/apache2/ssl.dist
   rm -f /etc/apache2/ssl
   ln -s $LOGICMOO_WS/docker/rootfs/etc/apache2/ssl /etc/apache2/ssl
if

if [ ! -d "/etc/supervisor.dist" ]; then
   mv /etc/supervisor /etc/supervisor.dist
   rm -f /etc/supervisor
   ln -s $LOGICMOO_WS/docker/rootfs/etc/supervisor/ /etc/supervisor
fi

if [ ! -d "/root.dist" ]; then
   mv /root /root.dist
   rm -f /root
   ln -s $LOGICMOO_WS/prologmud_server /root
fi

chown -R prologmud_server:www-data /root

if [ ! -d "/usr/local/lib/swipl.dist" ]; then
   mv /usr/local/lib/swipl /usr/local/lib/swipl.dist
   rm -f /usr/local/lib/swipl
   ln -s $LOGICMOO_WS/docker/rootfs/usr/local/lib/swipl /usr/local/lib/swipl
fi



rm -rf /opt/logicmoo_workspace/docker/rootfs/usr/local/lib/python3.6
find $LOGICMOO_WS/docker/rootfs/ -type f -printf "ln -sf '%p' '/%P' 2>/dev/null \n" | xargs -I{} bash -c "{}"
find $LOGICMOO_WS/docker/rootfs/usr/local/bin -type f -exec bash -c " ln -svf {} /usr/local/bin/ " \;


ln -sf `which swipl` /usr/bin/swipl

#chmod a+w -R /tmp/


if [ -f "/usr/share/emacs/26.3" ]; then
   mv /usr/share/emacs/26.3 /usr/share/emacs/26.3.dead
   ln -s  /usr/local/share/emacs/28.0.50/ /usr/share/emacs/26.1
   ln -s  /usr/local/share/emacs/28.0.50/ /usr/share/emacs/26.3
fi



(
pip install jswipl 
cd /usr/share/jupyter/kernels
mkdir -p jswipl && cd jswipl
wget https://raw.githubusercontent.com/targodan/jupyter-swi-prolog/master/kernel.json
cat kernel.json
)

set +x

