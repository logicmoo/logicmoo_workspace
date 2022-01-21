FROM synthillect/opencog-jupyter:latest
USER root
#FROM dorowu/ubuntu-desktop-lxde-vnc:latest
LABEL maintainer = "logicmoo@gmail.com"
ARG --security-opt seccomp:unconfined
ARG DEBIAN_FRONTEND noninteractive
ENV DEBIAN_FRONTEND noninteractive

#COPY --from=dorowu/ubuntu-desktop-lxde-vnc:latest / /
#COPY --from=opencog/cogutil:latest / /
#COPY --from=opencog/opencog-deps:latest / /
#COPY --from=opencog/postgres:latest / /
#COPY --from=opencog/relex:latest / /
#COPY --from=opencog/moses:latest / /
#COPY --from=synthillect/atomspace:latest / /
#COPY --from=synthillect/opencog-dev:latest / /
#COPY --from=synthillect/opencog-jupyter:latest / /
#COPY --from=synthillect/opencog-minerl:latest / /

RUN mkdir -p /usr/share/man/man1/
RUN apt-get update && apt-get --allow-unauthenticated --no-install-recommends --no-upgrade -y install \
	dbus-x11 ant apache2 apt build-essential chromium-browser \
	cmake default-jdk default-jre eggdrop flex gosu inotify-tools iproute2 iputils-arping iputils-ping junit4 \
	libarchive-dev libboost-all-dev libdb-dev libedit-dev libgd3 libgeoip1 libgmp-dev \
	libgnutls28-dev libgoogle-perftools-dev \
	graphviz libboost1.71-doc libboost-contract1.71-dev autoconf-archive \
	libmpfrc++-dev libntl-dev xsltproc doxygen docbook-xml docbook-xsl fop \
	libcanberra-gtk0 libcanberra-pulse libldap2-dev git fuse rng-tools geoip-bin \
	libmpfr-dev openssh-server keychain libpam-ssh monkeysphere ssh-askpass ed \
	ant-optional ssl-cert fakeroot libalgorithm-merge-perl libfl-dev \
	libsaxon-java less manpages manpages-dev libcanberra-gtk-module \
	libfile-fcntllock-perl liblocale-gettext-perl icc-profiles-free \
	geoip-database libgts-bin javascript-common libtool libsocket6-perl \
	libcoarrays-openmpi-dev libpam-tmpdir libpng-tools raptor2-utils \
  libgc-dev libgc1c2 autopoint cron netcat-openbsd netcat socat ssh-import-id rtkit \
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
	yarn zip zlib1g zlib1g-dev libtinfo5 libtinfo-dev libgraphviz-dev libjava-gnome-jni libzmq-* \
	yarn zip zlib1g zlib1g-dev libtinfo5 libtinfo-dev libgraphviz-dev libjava-gnome-jni libzmq-* \
        build-essential zlib1g-dev libncurses5-dev libgdbm-dev libnss3-dev libssl-dev libreadline-dev libffi-dev libsqlite3-dev wget libbz2-dev \
        rlwrap dbus-x11 firefox net-tools libzmq-* guile-3.0-dev npm libtinfo5 libtinfo6

#openjdk-8-jdk  

RUN apt-get -y update \
 && apt-get install -y --no-upgrade --allow-unauthenticated --no-install-recommends npm \
 && npm install -g n \
 && n stable

# Jupyter
EXPOSE 1800
# SSHD
EXPOSE 22    
# Apache HTTP 
#EXPOSE 80    
# Apache SSL 
#EXPOSE 443 4443
# Ngynx
EXPOSE 4801   
# Butterfly Logins
EXPOSE 4180
# SWISH
EXPOSE 3020
# Eggdrop
EXPOSE 3334
# MUD Plain Text (HTTPS/HTTP/TELNET)
EXPOSE 14100  4100  4000   
# MUD with Debug (HTTPS/HTTP/TELNET)
EXPOSE 14101  4101  4001   
# MUD with Graphics (HTTPS/HTTP/TELNET)
EXPOSE 14102  4102  4002  
# WAM-CL REPL (HTTPS/HTTP/TELNET)
EXPOSE 14103  4103  4003  
# NOMIC MU (HTTPS/HTTP/TELNET)
EXPOSE 14104  4104  4004   
#  Shared SWIPL ?-  (HTTPS/HTTP/TELNET)
EXPOSE 14123  4123  4023  
# Non-Shared SWIPL ?- (HTTPS/HTTP/TELNET)
EXPOSE 14125  4125  4025   

EXPOSE 4090 4091


MAINTAINER RUN if [ ! -z "$LOGICMOO_EXTRAS" ]; \
 then \
  curl -O http://mirror.umd.edu/eclipse/technology/epp/downloads/release/2020-06/R/eclipse-java-2020-06-R-linux-gtk-x86_64.tar.gz \
  && tar -zxvf eclipse-java-2020-06-R-linux-gtk-x86_64.tar.gz -C /usr/ \
  && ln -s /usr/eclipse/eclipse /usr/bin/eclipse \
  && rm -f eclipse-java-2020-06-R-linux-gtk-x86_64.tar.gz \
 fi


RUN a2dismod mpm_event \
 && a2enmod macro access_compat alias auth_basic authn_core authn_file authz_core authz_host authz_user autoindex deflate dir env \
 filter headers http2 mime mpm_prefork negotiation  php7.4 proxy proxy_ajp proxy_balancer proxy_connect proxy_express \
 proxy_fcgi proxy_fdpass proxy_ftp proxy_hcheck proxy_html proxy_http proxy_http2 proxy_scgi proxy_uwsgi proxy_wstunnel reqtimeout \
 rewrite setenvif slotmem_shm socache_shmcb ssl status xml2enc ; /bin/true \
 \
# confirm our webconfig works (or it exits docker build) \
 && service apache2 start && service apache2 status && service apache2 stop

# who/where
ENV LOGICMOO_WS /opt/logicmoo_workspace
ENV LOGICMOO_USER prologmud_server
ENV LOGICMOO_GAMES $LOGICMOO_WS/prologmud_server

ENV PATH "${LOGICMOO_WS}/bin:${PATH}"
ENV WNDB $LOGICMOO_WS/packs_sys/logicmoo_nlu/data/WNprolog-3.0/prolog


MAINTAINER 1 RUN curl -fsSL https://cli.github.com/packages/githubcli-archive-keyring.gpg | sudo gpg --dearmor -o /usr/share/keyrings/githubcli-archive-keyring.gpg \
 && echo "deb [arch=$(dpkg --print-architecture) signed-by=/usr/share/keyrings/githubcli-archive-keyring.gpg] https://cli.github.com/packages stable main" | sudo tee /etc/apt/sources.list.d/github-cli.list > /dev/null \
 && apt update \
 && apt install -y gh

COPY packs_sys/logicmoo_nlu/requirements.txt /tmp/requirements.txt

#COPY --from=python:3.8-slim /usr/local/lib/python3.8/distutils /usr/lib/python3.8/distutils
#COPY --from=python:3.9-slim /usr/local/lib/python3.9/distutils /usr/lib/python3.9/distutils
MAINTAINER 1 COPY --from=python:3.10-slim /usr/local/lib/python3.10/distutils /usr/lib/python3.10/distutils
#COPY --from=python:3.10-slim /usr/local/lib/python3.10/setuptools /usr/lib/python3.10/setuptools
MAINTAINER 1 COPY --from=python:3.10-slim /usr/local/lib/python3.10 /usr/lib/python3.10

RUN  curl -sS https://bootstrap.pypa.io/get-pip.py | /usr/bin/python3.10

#MAINTAINER 1 RUN \cp -a /usr/local/lib/python3.10/* /usr/local/lib/python3.8/ && rm -rf /usr/local/lib/python3.10 \
# && mv /usr/local/lib/python3.8 /usr/local/lib/python3.10 && ln -s /usr/local/lib/python3.10 /usr/local/lib/python3.8


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


# Python NLP Stuff
EXPOSE 4095 4096 4097 4098 4099
EXPOSE 5901

#COPY packs_sys/logicmoo_nlu/requirements.txt /tmp/requirements.txt
RUN apt remove -y python3-pygments python3-h5py python3-packaging python3-nbconvert python3-gevent python3-greenlet  ; /bin/true
RUN pip3 uninstall -y Pygments pygments ; /bin/true
RUN pip3 uninstall -y html5lib pyzmq zmq gevent greenlet spacy nltk nbconvert jupyter jupyterlab requests six gevent.websocket  ; /bin/true
RUN pip3 install --verbose filelock==3.4 importlib-metadata==4.4  ; /bin/true
RUN pip3 install html5lib pyzmq zmq gevent greenlet spacy nltk nbconvert jupyter jupyterlab requests six gevent.websocket  ; /bin/true

RUN pip3 install nbnovnc \
 && jupyter serverextension enable  --py --sys-prefix nbnovnc \
 && jupyter nbextension     install --py --sys-prefix nbnovnc \
 && jupyter nbextension     enable  --py --sys-prefix nbnovnc

COPY docker/rootfs /
EXPOSE 8880

