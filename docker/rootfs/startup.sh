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


cp /dockerstartup/generate_container_user /generate_container_user

export DEBIAN_FRONTEND=noninteractive


if [ -d "/usr/local/lib/python3.8" ]; then
\cp -a /usr/local/lib/python3.10/?* /usr/local/lib/python3.8/
mv /usr/local/lib/python3.10 /usr/local/lib/python3.10-orig
mv /usr/local/lib/python3.8 /usr/local/lib/python3.10
\cp -a /home/opencog/.local/lib/python3.10/?* /usr/local/lib/python3.10
fi

mv /home/opencog/.local/lib/python3.10 /home/opencog/.local/lib/python3.10-orig
ln -s /usr/local/lib/python3.10 /home/opencog/.local/lib/python3.10

curl -sS https://bootstrap.pypa.io/get-pip.py | /usr/bin/python3.10
python -m pip install pip -U
pip3 uninstall -y html5lib pyzmq zmq gevent greenlet spacy nltk nbconvert jupyter jupyterlab requests six gevent.websocket
pip3 install html5lib pyzmq zmq gevent greenlet spacy nltk nbconvert jupyter jupyterlab requests six gevent.websocket

#find /opt/logicmoo_workspace/packs_sys/logicmoo_agi -maxdepth 3 -name setup.py -execdir pip install -e . \;
find /opt/logicmoo_workspace/packs_lib -maxdepth 2 -name setup.py -execdir pip install -e . \;

pip3 install nbnovnc
jupyter serverextension enable  --py --sys-prefix nbnovnc
jupyter nbextension     install --py --sys-prefix nbnovnc
jupyter nbextension     enable  --py --sys-prefix nbnovnc

(
sudo ln -s /usr/lib/x86_64-linux-gnu /usr/lib64
ln -s /usr/lib/x86_64-linux-gnu/libguile-3.0.so /usr/local/lib/libguile-3.0.so
cd /opt/logicmoo_workspace/packs_sys/logicmoo_opencog
#find -maxdepth 2  -name requirements-dev.txt -execdir bash -c "pip install -r requirements-dev.txt" \;
#find -maxdepth 2  -name requirements.txt -execdir bash -c "pip install -r requirements.txt" \;
#find -maxdepth 3 -name setup.py -execdir bash -c "pip install -e ." \;
)



echo apt-get --allow-unauthenticated --no-install-recommends --no-upgrade -y install \
        build-essential cmake ninja-build pkg-config \
        ncurses-dev libreadline-dev libedit-dev \
        libgoogle-perftools-dev \
        libunwind-dev \
        libgmp-dev \
        libssl-dev \
        unixodbc-dev \
        zlib1g-dev libarchive-dev \
        libossp-uuid-dev \
        libxext-dev libice-dev libjpeg-dev libxinerama-dev libxft-dev \
        libxpm-dev libxt-dev \
        libdb-dev  libraptor2-dev \
        libpcre3-dev \
        libyaml-dev \
        default-jdk junit4 libserd-dev libserd-0-0 \
     libgnutls28-dev libidn11-dev libkrb5-dev librtmp-dev libssh2-1-dev

echo apt-get --allow-unauthenticated --no-install-recommends --no-upgrade -y install \
	dbus-x11 ant apache2 apt build-essential chromium-browser \
	cmake default-jdk default-jre eggdrop flex gosu inotify-tools iproute2 iputils-arping iputils-ping junit4 \
	libarchive-dev libboost-all-dev libdb-dev libedit-dev libgd3 libgeoip1 libgmp-dev \
	libgnutls28-dev libgoogle-perftools-dev guile-3.0-dev \
	graphviz libboost1.71-doc libboost-contract1.71-dev autoconf-archive \
	libmpfrc++-dev libntl-dev xsltproc doxygen docbook-xml docbook-xsl fop \
	libcanberra-gtk0 libcanberra-pulse libldap2-dev git fuse rng-tools geoip-bin \
	libmpfr-dev openssh-server keychain libpam-ssh monkeysphere ssh-askpass ed \
	ant-optional ssl-cert fakeroot libalgorithm-merge-perl libfl-dev \
	libsaxon-java less manpages manpages-dev libcanberra-gtk-module \
	libfile-fcntllock-perl liblocale-gettext-perl icc-profiles-free \
	geoip-database libgts-bin javascript-common libtool libsocket6-perl \
	libcoarrays-openmpi-dev libpam-tmpdir libpng-tools raptor2-utils \
  cron netcat-openbsd netcat socat ssh-import-id rtkit \
	file libatk-wrapper-java-jni fonts-dejavu-extra ncurses-term \
	lmodern dvisvgm ghostscript libfile-homedir-perl liblog-log4perl-perl libyaml-tiny-perl ruby \
	libice-dev libidn11-dev libjpeg-dev libkrb5-dev libllvm8 libmnl0 libnginx-mod-http-geoip libnginx-mod-http-image-filter \
	libnginx-mod-http-xslt-filter libnginx-mod-mail libnginx-mod-stream libossp-uuid-dev libpcre3-dev \
	libraptor2-dev libraptor2-dev libreadline-dev librtmp-dev libserd-0-0 libserd-dev libssh2-1-dev \
	libssl-dev libtcmalloc-minimal4 libunwind-dev libwebp6 libxext-dev libxft-dev libxinerama-dev libxpm-dev \
	libxslt1.1 libxt-dev libyaml-dev libzip-dev libzip-ocaml-dev lsb-release ncurses-dev \
	nfs-common nfs-kernel-server nginx nginx-common nginx-core ninja-build nmap pkg-config psmisc \
	python3-flask-api python3-gevent python3-pip rlwrap rpcbind rsync screen \
	software-properties-common sudo supervisor swig tdsodbc telnet texlive-extra-utils \
	tightvncserver tini traceroute tzdata unattended-upgrades unixodbc unixodbc-dev unzip \
	uuid-dev vim wget x11-apps x11-utils x11proto-core-dev x11proto-dev x11proto-xext-dev \
	x11proto-xinerama-dev x11vnc xauth xdotool xfonts-base xorg-sgml-doctools xterm xtrans-dev xvfb \
	yarn zip zlib1g zlib1g-dev
	
export LOGICMOO_WS=/opt/logicmoo_workspace
export DO_PULL=0

if [[ ! -d $LOGICMOO_WS/ ]]; then
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


echo "Starting . $LOGICMOO_WS/INSTALL.md"
cd $LOGICMOO_WS
bash -x $LOGICMOO_WS/INSTALL.md


mv /etc/apache2/conf-available /etc/apache2/conf-available.dist
rm -f /etc/apache2/conf-available
ln -s $LOGICMOO_WS/docker/rootfs/etc/apache2/conf-available /etc/apache2/conf-available 

mv /etc/apache2/sites-enabled /etc/apache2/sites-enabled.dist
rm -f /etc/apache2/sites-enabled
ln -s $LOGICMOO_WS/docker/rootfs/etc/apache2/sites-enabled /etc/apache2/sites-enabled

mv /etc/apache2/ssl /etc/apache2/ssl.dist
rm -f /etc/apache2/ssl
ln -s $LOGICMOO_WS/docker/rootfs/etc/apache2/ssl /etc/apache2/ssl

mv /etc/supervisor /etc/supervisor.dist
rm -f /etc/supervisor
ln -s $LOGICMOO_WS/docker/rootfs/etc/supervisor/ /etc/supervisor

mv /root /root.dist
rm -f /root
ln -s $LOGICMOO_WS/prologmud_server /root

chown -R prologmud_server:www-data /root

mv /usr/local/lib/swipl /usr/local/lib/swipl.dist
rm -f /usr/local/lib/swipl
ln -s $LOGICMOO_WS/docker/rootfs/usr/local/lib/swipl /usr/local/lib/swipl

rm -rf /opt/logicmoo_workspace/docker/rootfs/usr/local/lib/python3.6
find $LOGICMOO_WS/docker/rootfs/ -type f -printf "ln -sf '%p' '/%P' 2>/dev/null \n" | xargs -I{} bash -c "{}"
find $LOGICMOO_WS/docker/rootfs/usr/local/bin -type f -exec bash -c " ln -svf {} /usr/local/bin/ " \;


ln -sf `which swipl` /usr/bin/swipl

#chmod a+w -R /tmp/


if [[ -f /usr/share/emacs/26.3 ]]; then
   mv /usr/share/emacs/26.3 /usr/share/emacs/26.3.dead
   ln -s  /usr/local/share/emacs/28.0.50/ /usr/share/emacs/26.1
   ln -s  /usr/local/share/emacs/28.0.50/ /usr/share/emacs/26.3
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


mkdir -p /home/ubuntu
mkdir -p /home/opencog

mv /home/ubuntu /home/ubuntu.wrong
rm -f /home/ubuntu
ln -s /root /home/ubuntu

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


set +x

exec tini -w -vv -- /usr/bin/supervisord -n -c /etc/supervisor/supervisord.conf

