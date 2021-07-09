# Built with arch: amd64 flavor: lxde image: ubuntu:20.04
#
################################################################################
# base system
################################################################################

#FROM centminmod/docker-ubuntu-vnc-desktop
#FROM dickhub/ubuntu-xfce-vnc
#FROM ct2034/vnc-ros-kinetic-full
FROM tiryoh/ros2-desktop-vnc
MAINTAINER FROM dorowu/ubuntu-desktop-lxde-vnc:bionic
LABEL maintainer="Tiryoh<tiryoh@gmail.com>"
#FROM aicampbell/vnc-ubuntu18-xfce
EXPOSE 80

#
# from root@debian10:/opt/logicmoo_workspace
# we run..
#
# docker build -t logicmoo/logicmoo_starter_image:latest --no-cache --add-host=logicmoo.org:10.0.0.90 - < ./Dockerfile.distro
#

USER root
LABEL maintainer = "logicmoo@gmail.com"

ARG DEBIAN_FRONTEND=noninteractive
ARG --security-opt seccomp:unconfined


RUN mkdir -p /usr/share/man/man1 \
 && apt-key adv --keyserver keyserver.ubuntu.com --recv-keys 4F4EA0AAE5267A6C \
 && apt-get update -qq && apt-get install -y locales -qq \
 && locale-gen en_AU \
 && locale-gen en_AU.UTF-8 \
 && dpkg-reconfigure locales \
 # && locale-gen C.UTF-8 \
 && dpkg-reconfigure locales \
 && apt-get install -y --no-install-recommends --allow-unauthenticated \
 \
 lsb-release \
 tzdata \
 gosu \
 zlib1g-dev \
 zlib1g \
 zip \
 yarn \
 #xvnc4viewer \
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
 # tini \
 texlive-extra-utils \
 tdsodbc \
 supervisor \
 sudo \
 software-properties-common \
 screen \
 rsync \
 rlwrap \
 r-cran-xtable \
 #r-cran-xfun \
 #r-cran-tinytex \
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
 python-virtualenv \
 python-setuptools \
 python-dev \
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
 # openjdk-8-jdk \
 openjdk-11-jdk \
 odbc-postgresql \
 odbcinst1debian2 \
 odbcinst \
 nodejs-doc \
 ninja-build \
 #nginx-full \
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
 #libx265-165
 libx11-dev \
 libx11-6 \
 libwebpmux3 \
 libuv1-dev \
 libunwind-dev \
 #libunbound8 \
 libudunits2-dev \
 libubsan1 \
 libtsan0 \
 libtool \
 #libtinfo6 \
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
 libreadline7 \
 #libraptor2-dev \
 libraptor2-0 \
 libpthread-stubs0-dev \
 libpng-dev \
 libpcrecpp0v5 \
 libpcre3-dev \
 libpcre32-3 \
 libpcre3 \
 libpcre16-3 \
 #libotf0 \
 libossp-uuid-dev \
 libossp-uuid16 \
 libopenmpi-dev \
 libodbc1 \
 #libntlm0 \
 #libnode-dev \
 #libnode64 \
 libnet-nslookup-perl \
 libncurses-dev \
 #libncurses6 \
 libmpx2 \
 #libmariadb3 \
 libmailutils5 \
 #libmagickwand-6.q16-6
 #libmagickcore-6.q16-6 \
 #libm17n-0 \
 liblzma-dev \
 liblua5.1-0 \
 libltdl-dev \
 libltdl7 \
 liblsan0 \
 #liblqr-1-0 \
 #libkyotocabinet16v5 \
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
 #libjpeg62-turbo \
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
 #libcurl4-gnutls-dev \
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
 apache2-utils \
 apache2-suexec-pristine \
 apache2-suexec-custom \
 apache2-ssl-dev \
 apache2-doc \
 apache2-dev \
 apache2-data \
 apache2-bin \
 apache2 \
 analog \
 \
 # odd deps we missed for SWI-Prolog (world build version) \
 # libcurl4-openssl-dev libgazebo9-dev libignition-fuel-tools1-dev \
 # ros-dashing-desktop ros-dashing-gazebo-dev ros-dashing-gazebo-plugins \
 # ros-dashing-gazebo-ros ros-dashing-gazebo-ros-pkgs \
 # ros-dashing-libcurl-vendor ros-dashing-resource-retriever \
 # ros-dashing-rviz-common ros-dashing-rviz-default-plugins \
 # ros-dashing-rviz-rendering ros-dashing-rviz2 \
 && echo apt autoclean -y \
 && echo apt autoremove -y \
 && rm -rf /var/lib/apt/lists/* 

RUN mkdir -p /var/lock/apache2 /var/run/apache2 /var/run/sshd /var/log/supervisor
#RUN cp /etc/ssh/sshd_config /etc/ssh/sshd_config.original
#RUN chmod a-w /etc/ssh/sshd_config.original
RUN echo 'root:ubuntu' | chpasswd
RUN sed -i 's/PermitRootLogin without-password/PermitRootLogin yes/' /etc/ssh/sshd_config
#COPY supervisord.conf /etc/supervisor/conf.d/supervisord.conf
EXPOSE 22 80
#CMD [“/usr/bin/supervisord”]
ENV USER ubuntu
MAINTAINER RUN useradd --create-home --home-dir /home/ubuntu --shell /bin/bash --user-group --groups adm,sudo ubuntu && \
    echo ubuntu:ubuntu | chpasswd && \
    echo "ubuntu ALL=(ALL) NOPASSWD:ALL" >> /etc/sudoers
COPY rootfs/ros-eloquent-desktop.sh /ros-eloquent-desktop.sh
RUN mkdir -p /tmp/ros_setup_scripts_ubuntu && mv /ros-eloquent-desktop.sh /tmp/ros_setup_scripts_ubuntu/ && \
    ( gosu ubuntu /tmp/ros_setup_scripts_ubuntu/ros-eloquent-desktop.sh ; /bin/true) && \
    rm -rf /var/lib/apt/lists/*

ENV USER=root
ENV HOME=/root

RUN curl -s https://raw.githubusercontent.com/ros/rosdistro/master/ros.asc | sudo apt-key add - \
 && curl -sSL https://raw.githubusercontent.com/ros/rosdistro/master/ros.key -o /usr/share/keyrings/ros-archive-keyring.gpg \
 && curl http://repo.ros2.org/repos.key | sudo apt-key add -


RUN apt-get update -qq && apt-get install -y \
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
 && echo apt autoclean -y \
 && echo apt autoremove -y \
 && rm -rf /var/lib/apt/lists/* 




###########################
# EMACS LSP SUPPORT BEGIN #
###########################


FROM ubuntu:18.04 AS ccls
RUN apt-get update \
	# && apt-get upgrade -y \
	&& apt-get install -y build-essential cmake clang libclang-dev zlib1g-dev git wget \
	&& git clone --depth=1 --recursive https://github.com/MaskRay/ccls \
	&& cd ccls \
	&& wget -c http://releases.llvm.org/8.0.0/clang+llvm-8.0.0-x86_64-linux-gnu-ubuntu-18.04.tar.xz \
	&& tar xf clang+llvm-8.0.0-x86_64-linux-gnu-ubuntu-18.04.tar.xz \
	&& cmake -H. -BRelease -DCMAKE_BUILD_TYPE=Release -DCMAKE_PREFIX_PATH=$PWD/clang+llvm-8.0.0-x86_64-linux-gnu-ubuntu-18.04 \
	&& cmake --build Release \
	 && echo apt autoclean -y \
	 && echo apt autoremove -y \
	 && rm -rf /var/lib/apt/lists/* 

FROM ubuntu:18.04 AS go
RUN apt-get update -y \
	&& apt-get install -y wget \
	&& export LATEST_VERSION=`wget -qO- https://golang.org/dl | grep -oE go[0-9]+\.[0-9]+\.[0-9]+\.linux-amd64\.tar\.gz | head -n 1` \
	&& wget -c https://dl.google.com/go/$LATEST_VERSION \
	&& tar -xzf $LATEST_VERSION

# C-Family
COPY --from=ccls /ccls /ccls
RUN ln -s /ccls/Release/ccls /usr/bin/ccls \
	&& ln -s /ccls/clang+llvm-8.0.0-x86_64-linux-gnu-ubuntu-18.04/bin/clangd /usr/bin/clangd

# Go
MAINTAINER COPY --from=go /go /go
MAINTAINER ENV PATH "${PATH}:/go/bin:/root/go/bin"
MAINTAINER RUN /go/bin/go get -u golang.org/x/tools/gopls


# Python
RUN apt-get install -y python3-pip \
	&& pip3 install 'python-language-server[all]' \
	 && echo apt autoclean -y \
	 && echo apt autoremove -y \
	 && rm -rf /var/lib/apt/lists/* 


# required for apt upgrade (which is requested in EMACS LSP DOCKER)
MAINTAINER RUN apt-get update \
        ; mv /etc/apache2 /etc/apache2.logicmoo \
	; mv /var/www /var/www.logicmoo \
	; apt purge -y apache2 gitweb \
	; apt install -y apache2 gitweb \
      # ; apt install -y gir1.2-harfbuzz-0.0 libgraphite2-dev libharfbuzz-dev libharfbuzz-gobject0 libicu-le-hb-dev libicu-le-hb0 libiculx60 \
      	; apt-get upgrade -y \
	; mv /etc/apache2 /etc/apache2.dead \
	; mv /var/www /var/www.logicmoo.dead \
	; mv /etc/apache2.logicmoo /etc/apache2  \
	; mv /var/www.logicmoo /var/www ; /bin/true


# Emacs PreReqs (added this when Emacs was ~28.0.50)
RUN apt-get update \
       # && apt-get upgrade -y \
	&& apt-get install -y build-essential git autoconf texinfo libgnutls28-dev libxml2-dev libncurses5-dev libjansson-dev \
                              libjpeg-dev libpng-dev libgif-dev libtiff-dev libjson*-dev libxf*-dev
# Emacs
MAINTAINER RUN git clone --depth=1 git://git.sv.gnu.org/emacs.git /emacs \
	&& cd /emacs && ./autogen.sh \
	&& ./configure --with-modules --with-json \
	&& make -j4 && make install


#########################
# EMACS LSP SUPPORT END #
#########################

# who/where
ENV LANG C.UTF-8
ENV LANGUAGE C.UTF-8
ENV LC_ALL C.UTF-8
ENV LOGICMOO_USER prologmud_server
ENV LOGICMOO_WS /opt/logicmoo_workspace
ENV LOGICMOO_GAMES $LOGICMOO_WS/packs_sys/prologmud_samples/prolog/prologmud_sample_games


RUN apt-get update \
 && apt-get install -y -q --no-install-recommends tini screen nginx iputils-ping net-tools

# Phase two in case we forgot any above
MAINTAINER RUN echo add-apt-repository -y ppa:ondrej/php \
 && apt-get update \
# && apt-get install -y -q --no-install-recommends libapache2-mod-php 
# && apt-get install -y -q --no-install-recommends libapache2-mod-php7.4  ; /bin/true \
  && apt-get install -y -q --no-install-recommends libapache2-mod-wsgi ; /bin/true \
  && apt-get install -y -q --no-install-recommends libapache2-mod-proxy-uwsgi  ; /bin/true \
  && apt-get install -y -q --no-install-recommends iputils-ping net-tools \
# && apt-get install -y -q --no-install-recommends php7.4-dev php7.4-common php7.4 \
  && apt-get install -y -q --no-install-recommends \
 php7.4-mysql php7.4-xml php7.4-xmlrpc php7.4-curl \
 php7.4-gd php7.4-imagick php7.4-cli php7.4-dev php7.4-imap php7.4-mbstring \
 php7.4-opcache php7.4-soap php7.4-zip php7.4-intl  \
 # && echo apt autoclean -y \
 # && echo apt autoremove -y \
 # && rm -rf /var/lib/apt/lists/*


# install our Butterfly websockets (telnet server over httpd)
RUN \
 pip install --upgrade pip ; python3 -m pip install --upgrade pip \
 # && python3 -m pip uninstall setuptools ; pip install setuptools \
 && python3 -m pip install --upgrade setuptools wheel \
 && python3 -m pip install tornado asyncio \
 && python3 -m pip install butterfly \
 && python3 -m pip install butterfly[themes] # If you want to use themes \
 && python3 -m pip install butterfly[systemd] # If you want to use systemd \
 && RUN cd /etc/systemd/system \
 && curl -O https://raw.githubusercontent.com/paradoxxxzero/butterfly/master/butterfly.service \
 && curl -O https://raw.githubusercontent.com/paradoxxxzero/butterfly/master/butterfly.socket \
 && echo Maybe remember to: systemctl enable butterfly.socket \
 && echo Maybe remember to: systemctl start butterfly.socket


# Emacs
MAINTAINER RUN git clone --depth=1 git://git.sv.gnu.org/emacs.git /emacs \
	&& cd /emacs && ./autogen.sh \
	&& ./configure --with-modules --with-json \
	&& make -j4 && make install


# expose our used ports
EXPOSE 22
#LSP (right?)
EXPOSE 8123 5007 6001 5555 8543
EXPOSE 80 4080
EXPOSE 443 4443
EXPOSE 3020 4020
EXPOSE 111 2049
EXPOSE 139 445

# Phase three in case we forgot any above
 #python3-gevent-websocket \
 #python3-novnc \
 #python3-flask-sockets \



LABEL maintainer = "logicmoo@gmail.com"
################################################################################
# merge
################################################################################
#FROM system
LABEL maintainer="fcwu.tw@gmail.com"
EXPOSE 80
EXPOSE 4180
EXPOSE 4022
WORKDIR /root
ENV HOME=/root \
    SHELL=/bin/bash
HEALTHCHECK --interval=60s --timeout=10s CMD curl --fail http://127.0.0.1:6079/api/health


COPY rootfs /

ENTRYPOINT ["/startup_logicmoo.sh"]
ENTRYPOINT ["/startup.sh"]
