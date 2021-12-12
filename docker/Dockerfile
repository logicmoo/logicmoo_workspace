FROM dorowu/ubuntu-desktop-lxde-vnc
LABEL maintainer = "logicmoo@gmail.com"
ARG DEBIAN_FRONTEND=noninteractive
ENV DEBIAN_FRONTEND noninteractive
ARG --security-opt seccomp:unconfined

RUN mkdir -p /usr/share/man/man1/
RUN apt-get update && apt-get install -y --no-upgrade --allow-unauthenticated \
  nginx-common nginx nginx-core  libnginx-mod-http-geoip libnginx-mod-http-image-filter \
  libnginx-mod-http-xslt-filter libnginx-mod-mail libnginx-mod-stream \
  supervisor apache2 nmap x11-apps vim eggdrop default-jdk default-jre \
  iproute2 libgd3 libgeoip1 libmnl0 libwebp6 libxslt1.1 \
  libtcmalloc-minimal4 \
  psmisc \
 python3-gevent \
 python3-flask-api \
 iputils-ping \
 iputils-arping \
 nfs-kernel-server \
 python3-pip \
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
 #xvnc4viewer \
 xterm \
 wget \
 vim \
 uuid-dev \
 unzip \
 unixodbc-dev \
 unixodbc \
 #unattended-upgrades \
 tightvncserver \
 chromium-browser \
 # tini \
 texlive-extra-utils \
 tdsodbc \
 sudo \
 #software-properties-common \
 screen \
 rsync \
 rlwrap \
 libraptor2-dev			 \
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
	libdb-dev \
	libpcre3-dev \
	libyaml-dev \
	default-jdk junit4

