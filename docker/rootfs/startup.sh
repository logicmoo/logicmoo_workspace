#!/bin/bash

set +x
#set +e

export DEBIAN_FRONTEND=noninteractive
apt update
apt-get install -y --allow-unauthenticated \
  nginx-common nginx nginx-core  libnginx-mod-http-geoip libnginx-mod-http-image-filter \
  libnginx-mod-http-xslt-filter libnginx-mod-mail libnginx-mod-stream \
  supervisor apache2 nmap x11-apps vim eggdrop default-jdk default-jre \
  iproute2 libgd3 libgeoip1 libmnl0 libwebp6 libxslt1.1 \
 \
 python3-gevent \
 python3-flask-api \
 iputils-ping \
 iputils-arping \
 nfs-kernel-server \
 nfs-common \
 rpcbind \
 telnet \
 traceroute \
 inotify-tools \
 ant \
 swig \
 flex \
 libllvm8 \
 lsb-release \
 tzdata \
 gosu \
 zlib1g-dev \
 zlib1g \
 zip \
 yarn \
 xvfb \
 xtrans-dev \
 xterm \
 xorg-sgml-doctools \
 xfonts-base \
 xdotool \
 xauth \
 x11vnc \
 x11-utils \
 x11proto-xinerama-dev \
 x11proto-xext-dev \
 x11proto-dev \
 x11proto-core-dev \
 wget \
 vim \
 uuid-dev \
 unzip \
 unixodbc-dev \
 unixodbc \
 unattended-upgrades \
 tightvncserver \
 texlive-extra-utils \
 tdsodbc \
 supervisor \
 sudo \
 software-properties-common


export LOGICMOO_WS=/opt/logicmoo_workspace
export DO_PULL=0

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

# check out our repo
if [[ ! -d $LOGICMOO_WS/.git ]]
then
 cd /opt
 echo "clone --depth 1 https://github.com/logicmoo/logicmoo_workspace"
 git config --global http.sslVerify false
 git clone --depth 1 https://github.com/logicmoo/logicmoo_workspace
 find $LOGICMOO_WS/ -type d -exec chmod 777 {} +
 chmod a+w -R $LOGICMOO_WS/
fi

cd $LOGICMOO_WS
if [ $DO_PULL -gt 0 ]; then 
   echo "git checkout master ."
   git checkout master .
else 
   echo "Skipping pull"
fi

chmod a+w -R /tmp/

rsync -ra $LOGICMOO_WS/docker/rootfs/. /.


echo "Starting . $LOGICMOO_WS/INSTALL.md"
cd $LOGICMOO_WS
bash -x $LOGICMOO_WS/INSTALL.md

mv /etc/apache2/conf-available /etc/apache2/conf-available.dist
ln -s $LOGICMOO_WS/docker/rootfs/etc/apache2/conf-available /etc/apache2/conf-available 
mv /etc/apache2/sites-enabled /etc/apache2/sites-enabled.dist
ln -s $LOGICMOO_WS/docker/rootfs/etc/apache2/sites-enabled /etc/apache2/sites-enabled
mv /etc/apache2/ssl /etc/apache2/ssl.dist
ln -s $LOGICMOO_WS/docker/rootfs/etc/apache2/ssl /etc/apache2/ssl
mv /etc/supervisor /etc/supervisor.dist
ln -s $LOGICMOO_WS/docker/rootfs/etc/supervisor /etc/supervisor
mv /root /root.dist
ln -s $LOGICMOO_WS/packs_sys/prologmud_samples/prolog/prologmud_sample_games/ /root
chown -R prologmud_server:www-data /root

mv /usr/local/lib/swipl /usr/local/lib/swipl.dist
ln -s $LOGICMOO_WS/docker/rootfs/usr/local/lib/swipl /usr/local/lib/swipl

find $LOGICMOO_WS/docker/rootfs/usr/local/bin -type f -exec bash -c " ln -svf {} /usr/local/bin/ " \;


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
    sed -i "s#^command=/usr/bin/openbox.*#& ${OPENBOX_ARGS}#" /etc/supervisor/conf.d/supervisord.conf
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
    cp -r /root/{.gtkrc-2.0,.asoundrc} ${HOME}
    [ -d "/dev/snd" ] && chgrp -R adm /dev/snd
fi
sed -i -e "s|%USER%|$USER|" -e "s|%HOME%|$HOME|" /etc/supervisor/conf.d/supervisord.conf

# home folder
mkdir -p $HOME/.config/pcmanfm/LXDE/
ln -sf /usr/local/share/doro-lxde-wallpapers/desktop-items-0.conf $HOME/.config/pcmanfm/LXDE/
# chown -R $USER:$USER $HOME

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

exec /bin/tini -w -vv -- /usr/bin/supervisord -n -c /etc/supervisor/supervisord.conf

