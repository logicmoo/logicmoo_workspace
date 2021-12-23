# Built with arch: amd64 flavor: lxde image: ubuntu:20.04
#
################################################################################
# base system
################################################################################

function MAINTAINER {
 echo SKIPPING: $*
}
function RUN {
 echo RUNing: $*
 $*
}

#FROM centminmod/docker-ubuntu-vnc-desktop
#FROM dickhub/ubuntu-xfce-vnc
#FROM ct2034/vnc-ros-kinetic-full
#FROM tiryoh/ros2-desktop-vnc
#FROM ros:noetic-desktop-full
#FROM tiryoh/ros-desktop-vnc:noetic
#MAINTAINER FROM dorowu/ubuntu-desktop-lxde-vnc:bionic
#LABEL maintainer="Tiryoh<tiryoh@gmail.com>"
#FROM aicampbell/vnc-ubuntu18-xfce
##EXPOSE 80


#USER root
#LABEL maintainer = "logicmoo@gmail.com"

export DEBIAN_FRONTEND=noninteractive

#
# from root@debian10:/opt/logicmoo_workspace
# we run..
#
# docker build -t logicmoo/logicmoo_starter_image:latest --no-cache --add-host=logicmoo.org:10.0.0.90 - < ./Dockerfile.distro
#

export DEBIAN_FRONTEND=noninteractive
#export --security-opt seccomp:unconfined
# who/where
export LANG=C.UTF-8
export LANGUAGE=C.UTF-8
export LC_ALL=C.UTF-8
export LOGICMOO_USER=prologmud_server
export LOGICMOO_WS=/opt/logicmoo_workspace
export LOGICMOO_GAMES=$LOGICMOO_WS/prologmud_server


mkdir -p /usr/share/man/man1 \
 && apt update \
 && apt install -y apt-utils curl \
 && apt-key adv --keyserver keyserver.ubuntu.com --recv-keys 4F4EA0AAE5267A6C \
 && sh -c 'echo "deb [trusted=yes] http://packages.ros.org/ros/ubuntu $(lsb_release -sc) main" > /etc/apt/sources.list.d/ros-latest.list' \
 && curl -s https://raw.githubusercontent.com/ros/rosdistro/master/ros.asc | sudo apt-key add - \
 && apt-get update \
 && apt-get upgrade -y

# apt-get install -y locales -qq && locale-gen en_AU \&& locale-gen en_AU.UTF-8 \ && dpkg-reconfigure locales \ && locale-gen C.UTF-8 \ && dpkg-reconfigure locales

apt-get install -y --allow-unauthenticated \
  nginx-common nginx nginx-core  libnginx-mod-http-geoip libnginx-mod-http-image-filter \
  libnginx-mod-http-xslt-filter libnginx-mod-mail libnginx-mod-stream \
  supervisor apache2 nmap x11-apps vim eggdrop default-jdk default-jre \
  iproute2 libgd3 libgeoip1 libmnl0 libwebp6 libxslt1.1 \
  build-essential git autoconf texinfo libgnutls28-dev libxml2-dev libncurses5-dev libjansson-dev \
  libxpm-dev libjpeg-dev libpng-dev libgif-dev libtiff-dev libjson*-dev libxf*-dev libwebkit2gtk-4.0-dev \
 python-is-python3 \
 python-dev-is-python3 \
 python3-gevent \
 python3-flask-api \
 python3-flask \
 python3-gevent-websocket \
 python3-novnc \
 python3-flask-sockets \
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
 software-properties-common \
 screen \
 rsync \
 rlwrap \
 r-cran-xtable \
 r-cran-tikzdevice \
 r-cran-testit \
 r-cran-sourcetools \
 r-cran-shiny \
 r-cran-rserve \
 r-cran-highr \
 r-cran-filehash \
 r-cran-devtools \
 r-cran-bit64 \
 r-cran-base64enc \
 r-base-dev \
 r-base \
 qt5-default \
 python-setuptools \
 python3-pip \
 python3-dev \
 python-pip-whl \
 pwgen \
 psmisc \
 po-debconf \
 pkg-config \
 patch \
 pandoc-data \
 pandoc \
 openssh-server \
 openjdk-11-jdk \
 odbc-postgresql \
 odbcinst1debian2 \
 odbcinst \
 nodejs-doc \
 ninja-build \
 nginx-core \
 net-tools \
 netbase \
 ncurses-term \
 ncurses-dev \
 ncdu \
 nano \
 mysql-common \
 mesa-utils \
 mariadb-common \
 make \
 mailutils-common \
 mailutils \
 m4 \
 m17n-db \
 lynx \
 lxde \
 lwm \
 lsof \
 linux-libc-dev \
 libyaml-dev \
 libyaml-0-2 \
 libxt-dev \
 libxt6 \
 libxrender-dev \
 libxpm-dev \
 libxpm4 \
 libxml2-dev \
 libxinerama-dev \
 libxinerama1 \
 libxft-dev \
 libxft2 \
 libxext-dev \
 libxext6 \
 libxdmcp-dev \
 libxcb1-dev \
 libxau-dev \
 libx11-dev \
 libx11-6 \
 libwebpmux3 \
 libuv1-dev \
 libunwind-dev \
 libudunits2-dev \
 libubsan1 \
 libtsan0 \
 libtool \
 libtinfo5 \
 libtiff-dev \
 libtcmalloc-minimal4 \
 libstdc++-8-dev \
 libssl-dev \
 libssl1.1 \
 libssh-dev \
 libsqlite3-0 \
 libsm-dev \
 libsm6 \
 libsigsegv2 \
 libserd-dev \
 libserd-0-0 \
 librhash0 \
 libreoffice \
 libreadline-dev \
 libraptor2-0 \
 libpthread-stubs0-dev \
 libpng-dev \
 libpcrecpp0v5 \
 libpcre3-dev \
 libpcre32-3 \
 libpcre3 \
 libpcre16-3 \
 libossp-uuid-dev \
 libossp-uuid16 \
 libopenmpi-dev \
 libodbc1 \
 libnet-nslookup-perl \
 libncurses-dev \
 libmpx2 \
 liblzma-dev \
 liblua5.1-0 \
 libltdl-dev \
 libltdl7 \
 liblsan0 \
 libjs-twitter-bootstrap-datepicker \
 libjs-sifter.js \
 libjs-prettify \
 libjsoncpp1 \
 libjs-microplugin.js \
 libjs-json \
 libjs-jquery-ui \
 libjs-jquery-selectize.js \
 libjs-jquery-datatables \
 libjs-jquery \
 libjs-is-typedarray \
 libjs-inherits \
 libjs-highlight.js \
 libjs-es5-shim \
 libjs-d3 \
 libjpeg-turbo-progs \
 libjpeg-dev \
 libjpeg62 \
 libitm1 \
 libice-dev \
 libice6 \
 libheif1 \
 libgsasl7 \
 libgoogle-perftools-dev \
 libgnutls-dane0 \
 libgmpxx4ldbl \
 libgmp-dev \
 libgmp10 \
 libgl1-mesa-dri \
 libgeos++-dev \
 libgeos-c1v5 \
 libgcc-8-dev \
 libgc1c2 \
 libfreetype6-dev \
 libfreetype6 \
 libfontconfig1-dev \
 libfontconfig1 \
 libfile-stripnondeterminism-perl \
 libfftw3-double3 \
 libffi-dev \
 libexpat1-dev \
 libedit-dev \
 libedit2 \
 libdpkg-perl \
 libde265-0 \
 libdb-dev \
 libdb5.3-dev \
 libdb5.3 \
 libcurl4-openssl-dev \
 libc-dev-bin \
 libcc1-0 \
 libc-ares2 \
 libcairo-dev \
 libc6-dev \
 libc6 \
 libbsd-dev \
 libbinutils \
 libatk-wrapper-java-jni \
 libatk-wrapper-java \
 libasan5 \
 libarchive-zip-perl \
 libarchive-dev \
 libarchive13 \
 libapache2-mod-wsgi \
 less \
 junit4 \
 junit \
 intltool-debian \
 install-info \
 init \
 gyp \
 gtk2-engines-pixbuf \
 gtk2-engines-murrine \
 gsfonts guile-2.2-libs imagemagick-6-common \
 ghostscript \
 gnupg \
 gnome-themes-standard \
 gitweb \
 git-lfs \
 gitk \
 git-gui \
 git \
 gettext \
 gdebi-core \
 gdbserver \
 gdb \
 gcc-8 \
 gcc \
 g++-8 \
 g++ \
 file \
 exim4-base \
 exim4-config \
 exim4-daemon-light \
 #emacs-gtk \
 emacsen-common \
 emacs \
 elpa-ediprolog \
 eggdrop \
 dwz \
 dpkg-dev \
 dh-strip-nondeterminism \
 dh-autoreconf \
 debhelper \
 dctrl-tools \
 dbus-x11 \
 curl \
 cron \
 cmake-data \
 cmake \
 ca-certificates \
 build-essential \
 binutils-x86-64-linux-gnu \
 binutils-common \
 binutils \
 bash-completion \
 bash \
 autotools-dev \
 autopoint \
 automake \
 autoconf \
 arc-theme \
 apt-utils \
 libapache2-mod-php libapache2-mod-proxy-uwsgi \
 apache2-utils \
 apache2-suexec-pristine \
 apache2-suexec-custom \
 apache2-ssl-dev \
 apache2-doc \
 apache2-dev \
 apache2-data \
 apache2-bin \
 apache2 \
 analog
# && apt install ros-noetic-desktop-full 

apt remove -y swi-prolog-*

mkdir -p /var/lock/apache2 /var/run/apache2 /var/run/sshd /var/log/supervisor

cp /etc/ssh/sshd_config /etc/ssh/sshd_config.original
chmod a-w /etc/ssh/sshd_config.original
echo 'root:ubuntu' | chpasswd
sed -i 's/PermitRootLogin without-password/PermitRootLogin yes/' /etc/ssh/sshd_config
#COPY supervisord.conf /etc/supervisor/conf.d/supervisord.conf
##EXPOSE 22 80
#CMD [“/usr/bin/supervisord”]
export USER=ubuntu
RUN useradd --create-home --home-dir /home/ubuntu --shell /bin/bash --user-group --groups adm,sudo ubuntu && \
    echo ubuntu:ubuntu | chpasswd && \
    echo "ubuntu ALL=(ALL) NOPASSWD:ALL" >> /etc/sudoers

export USER=root
export HOME=/root




###########################
# EMACS LSP SUPPORT BEGIN #
###########################



export TINI_VERSION=v0.19.0
wget https://github.com/krallin/tini/releases/download/${TINI_VERSION}/tini -o /bin/tini
chmod +x /bin/tini
#RUN chmod +x run.sh
#RUN chmod 777 docker-entrypoint.sh

apt-get update && apt-get install -y software-properties-common curl gnupg2 && \
  curl -fsSL https://apt.releases.hashicorp.com/gpg | apt-key add - && \
  apt-add-repository "deb [arch=amd64] https://apt.releases.hashicorp.com $(lsb_release -cs) main" && \
  apt-get update && apt-get install -y \
  vault bash && \
  setcap cap_ipc_lock= /usr/bin/vault


apt-get install -y -q --no-install-recommends libapache2-mod-wsgi ; /bin/true 
apt-get install -y -q --no-install-recommends libapache2-mod-proxy-uwsgi  ; /bin/true 
apt-get install -y -q --no-install-recommends iputils-ping net-tools 
apt-get install -y -q --no-install-recommends \
 php7.4-mysql php7.4-xml php7.4-xmlrpc php7.4-curl \
 php7.4-gd php7.4-imagick php7.4-cli php7.4-dev php7.4-imap php7.4-mbstring \
 php7.4-opcache php7.4-soap php7.4-zip php7.4-intl


# install our Butterfly websockets (telnet server over httpd)
pip install --upgrade pip ; python3 -m pip install --upgrade pip
python3 -m pip uninstall setuptools ; pip install setuptools
 python3 -m pip install --upgrade setuptools wheel
 python3 -m pip install tornado asyncio
 python3 -m pip install butterfly
 python3 -m pip install butterfly[themes] # If you want to use themes
 python3 -m pip install butterfly[systemd] # If you want to use systemd 
 cd /etc/systemd/system
 curl -O https://raw.githubusercontent.com/paradoxxxzero/butterfly/master/butterfly.service
 curl -O https://raw.githubusercontent.com/paradoxxxzero/butterfly/master/butterfly.socket
 systemctl enable butterfly.socket
 systemctl start butterfly.socket




# expose our used ports
#EXPOSE 22
#LSP (right?)
#EXPOSE 8123 5007 6001 5555 8543
#EXPOSE 4080
##EXPOSE 443 
#EXPOSE 4443
#EXPOSE 3020 4020
#EXPOSE 111 2049
##EXPOSE 139 445

# Phase three in case we forgot any above
 #python3-gevent-websocket \
 #python3-novnc \
 #python3-flask-sockets \



#LABEL maintainer = "logicmoo@gmail.com"
################################################################################
# merge
################################################################################
#FROM system
#LABEL maintainer="fcwu.tw@gmail.com"
##EXPOSE 80
#EXPOSE 4180
#EXPOSE 4022
#WORKDIR /root
export HOME=/root \
    SHELL=/bin/bash
# HEALTHCHECK --interval=60s --timeout=10s CMD curl --fail http://127.0.0.1:6079/api/health

#COPY rootfs /

echo "Set disable_coredump false" >> /etc/sudo.conf
# RUN useradd --create-home --home-dir /home/ubuntu --shell /bin/bash --user-group --groups adm,sudo ubuntu && \
echo ubuntu:ubuntu | chpasswd 
echo "ubuntu ALL=(ALL) NOPASSWD:ALL" >> /etc/sudoers

export USER=ubuntu

wget -O /tmp/google-chrome-stable_current_amd64.deb https://dl.google.com/linux/direct/google-chrome-stable_current_amd64.deb \
 && apt update && apt install -y /tmp/google-chrome-stable_current_amd64.deb \
 && apt autoclean -y \
 && echo apt autoremove -y

apt-get update \
    && export DEBIAN_FRONTEND=noninteractive \
    && apt-get -y install --no-install-recommends apt-utils dialog 2>&1

apt update \
  && apt install -y npm \
  && npm install -g typescript

apt update \
  && apt install -y mono-complete mono-4.0-gac mono-tools-devel
  

#ENTRYPOINT ["/startup_logicmoo.sh"]
#ENTRYPOINT ["/startup.sh"]
#ENTRYPOINT ["/bin/sh", "-ec", "while :; do echo '.'; sleep 600 ; done"]



