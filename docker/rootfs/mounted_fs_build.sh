#!/usr/bin/bash

set -x

cd /opt/logicmoo_workspace/packs_sys/logicmoo_opencog

apt install -y libzmq-* lxpanel pcmanfm novnc openbox


#rm -rf /usr/share/guile /usr/local/include/guile /usr/local/lib/guile/ /usr/local/share/guile /usr/local/bin/guile /usr/bin/guile /usr/include/guile /usr/lib/x86_64-linux-gnu/guile
apt install -y guile-3.0-dev
#apt install -y guile-3* --reinstall

if [ -f "/never"]; then


cd /opt/logicmoo_workspace/packs_sys/logicmoo_opencog/fibers
./autogen.sh
autoreconf -i
./configure~
make
make install


cd /opt/logicmoo_workspace/packs_sys/logicmoo_opencog/guile-persist
autoreconf -i
./configure
make
make install
cp -a /guile/?* /usr/local/lib/guile/ ; /bin/true
cp -a /guile/?* /usr/lib/x86_64-linux-gnu/guile/ ; /bin/true

cd /opt/logicmoo_workspace/packs_sys/logicmoo_opencog/guile-syntax-parse
autoreconf -i
./configure
make
make install


cd /opt/logicmoo_workspace/packs_sys/logicmoo_opencog/guile-log
autoreconf -i
./configure
make
make install


cd /opt/logicmoo_workspace/packs_sys/logicmoo_opencog/guile-json
autoreconf -ifv
./configure
make
make install


cd /opt/logicmoo_workspace/packs_sys/logicmoo_opencog/guile-simple-zmq
autoreconf -ifv
./configure
make
make install

mkdir -p /usr/share/jupyter/kernels/guile
ln -s /opt/logicmoo_workspace/packs_sys/logicmoo_opencog/guile-kernel/ /usr/share/jupyter/kernels/guile/guile-kernel
cp /opt/logicmoo_workspace/packs_sys/logicmoo_opencog/guile-kernel/src/kernel.json /usr/share/jupyter/kernels/guile/kernel.json


#unalias cp
if [ ! -d "/usr/lib/guile" ]; then
   if [ ! -f "/usr/lib/guile" ]; then
      ln -s /usr/local/lib/guile/ /usr/lib/guile 
   fi
fi

if [ ! -d "/usr/lib/guile.dist" ]; then    
   if [ -d "/usr/lib/guile" ]; then
      cp -a /usr/lib/guile/?* /usr/lib/x86_64-linux-gnu/guile/  ; /bin/true
      mv /usr/lib/guile /usr/lib/guile.dist ; /bin/true
      ln -s /usr/local/lib/guile/ /usr/lib/guile ; /bin/true
   fi
fi


if [ ! -d "/usr/share/guile.dist" ]; then
   cp -a /usr/share/guile/?* /usr/local/share/guile/ ; /bin/true
   mv /usr/share/guile /usr/share/guile.dist ; /bin/true
   ln -s /usr/local/share/guile/ /usr/share/guile  ; /bin/true
fi

if [ ! -d "/usr/share/guile.dist" ]; then
   cp -a /usr/share/guile/?* /usr/local/share/guile/ ; /bin/true
   mv /usr/share/guile /usr/share/guile.dist ; /bin/true
   ln -s /usr/local/share/guile/ /usr/share/guile  ; /bin/true
fi

if [ ! -d "/usr/local/lib/guile.dist" ]; then
   cp -a /usr/local/lib/guile/?* /usr/lib/x86_64-linux-gnu/guile/  ; /bin/true
   mv /usr/local/lib/guile /usr/local/lib/guile.dist  ; /bin/true
   ln -s /usr/lib/x86_64-linux-gnu/guile /usr/local/lib/guile  ; /bin/true
fi

if [ -d "/guile" ]; then
   cp -a /guile/?* /usr/local/lib/guile/ ; /bin/true
   cp -a /guile/?* /usr/lib/x86_64-linux-gnu/guile/ ; /bin/true
   mv /guile /guile.dist ; /bin/true
fi 
if [ ! -f "/guile" ]; then
   ln -s /usr/lib/x86_64-linux-gnu/guile /guile
fi

fi

(
pip install jswipl 
cd /usr/share/jupyter/kernels
mkdir -p jswipl && cd jswipl
wget https://raw.githubusercontent.com/targodan/jupyter-swi-prolog/master/kernel.json
cat kernel.json
)


if [ ! -d "/root.dist" ]; then
   mkdir -p /home/ubuntu
   mkdir -p /home/opencog
   mv /root /root.dist
   mv /home/opencog /home/opencog.dist
   mv /home/ubuntu /home/ubuntu.dist
   ln -s ~prologmud_server /home/opencog
   ln -s ~prologmud_server /root
   ln -s ~prologmud_server /home/ubuntu
#cp ~prologmud_server/.guile ~root/.guile
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
fi

if [ ! -d "/etc/supervisor.dist" ]; then
   mv /etc/supervisor /etc/supervisor.dist
   rm -f /etc/supervisor
   ln -s $LOGICMOO_WS/docker/rootfs/etc/supervisor/ /etc/supervisor
fi

chown -R prologmud_server:www-data /root

if [ ! -d "/usr/local/lib/swipl.dist" ]; then
   mv /usr/local/lib/swipl /usr/local/lib/swipl.dist
   rm -f /usr/local/lib/swipl
   ln -s $LOGICMOO_WS/docker/rootfs/usr/local/lib/swipl /usr/local/lib/swipl
fi

if [ ! -d "/home/opencog/.local/lib/python3.10.dist" ]; then
   if [ -d "/home/opencog/.local/lib/python3.10" ]; then
      mv /home/opencog/.local/lib/python3.10 /home/opencog/.local/lib/python3.10.dist
      ln -s /usr/local/lib/python3.10 /home/opencog/.local/lib/python3.10
      cp -a /home/opencog/.local/lib/python3.10.dist/?* /home/opencog/.local/lib/python3.10
   fi
fi

rm -rf /opt/logicmoo_workspace/docker/rootfs/usr/local/lib/python3.6
find $LOGICMOO_WS/docker/rootfs/ -type f -printf "ln -sf '%p' '/%P' 2>/dev/null \n" | xargs -I{} bash -c "{}"
find $LOGICMOO_WS/docker/rootfs/usr/local/bin -type f -exec bash -c " ln -svf {} /usr/local/bin/ " \;

echo "Starting . $LOGICMOO_WS/INSTALL.md"
cd $LOGICMOO_WS
bash -x $LOGICMOO_WS/INSTALL.md

ln -sf `which swipl` /usr/bin/swipl

#chmod a+w -R /tmp/


if [ -f "/usr/share/emacs/26.3" ]; then
 if [ ! -f "/usr/share/emacs/26.3.dead" ]; then
   mv /usr/share/emacs/26.3 /usr/share/emacs/26.3.dead
   ln -s  /usr/local/share/emacs/28.0.50/ /usr/share/emacs/26.1
   ln -s  /usr/local/share/emacs/28.0.50/ /usr/share/emacs/26.3
 fi
fi

find /opt/logicmoo_workspace/packs_???/ -maxdepth 3 -name setup.py -execdir bash -c "pip install --target /usr/local/lib/python3.10/dist-packages --no-deps --no-compile -e ." \;

