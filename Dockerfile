FROM logicmoo/logicmoo_starter_image

USER root
LABEL maintainer = "logicmoo@gmail.com"
ARG DEBIAN_FRONTEND=noninteractive
ENV DEBIAN_FRONTEND noninteractive
ARG --security-opt seccomp:unconfined
LABEL maintainer = "logicmoo@gmail.com"
ARG DEBIAN_FRONTEND=noninteractive
ENV DEBIAN_FRONTEND noninteractive
ARG --security-opt seccomp:unconfined

COPY docker/rootfs /

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


ENV HOME /root

COPY docker/rootfs /

RUN apt-get update && apt-get install -y --allow-unauthenticated \
  nginx-common nginx nginx-core  libnginx-mod-http-geoip libnginx-mod-http-image-filter \
  libnginx-mod-http-xslt-filter libnginx-mod-mail libnginx-mod-stream \
  supervisor apache2 nmap x11-apps vim eggdrop default-jdk default-jre \
  iproute2 libgd3 libgeoip1 libmnl0 libwebp6 libxslt1.1 \
 \
 python3-gevent \
 python3-pip \
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
 tini


RUN apt update \
 && apt-get install -y \
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
        default-jdk junit4 libserd-dev libserd-0-0
        

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

RUN apt-get update && apt-get install -y --allow-unauthenticated \
  libtinfo5 libtinfo6

RUN curl -fsSL https://cli.github.com/packages/githubcli-archive-keyring.gpg | sudo gpg --dearmor -o /usr/share/keyrings/githubcli-archive-keyring.gpg \
 && echo "deb [arch=$(dpkg --print-architecture) signed-by=/usr/share/keyrings/githubcli-archive-keyring.gpg] https://cli.github.com/packages stable main" | sudo tee /etc/apt/sources.list.d/github-cli.list > /dev/null \
 && apt update \
 && apt install -y gh

COPY packs_sys/logicmoo_nlu/requirements.txt /tmp/requirements.txt

#RUN  curl -sS https://bootstrap.pypa.io/get-pip.py | /usr/bin/python3.10

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

RUN apt update \
 && apt-get install -y \
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
	python3-pip \
        default-jdk junit4 libserd-dev libserd-0-0

MAINTAINER RUN curl -fsSL https://cli.github.com/packages/githubcli-archive-keyring.gpg | sudo gpg --dearmor -o /usr/share/keyrings/githubcli-archive-keyring.gpg \
 && echo "deb [arch=$(dpkg --print-architecture) signed-by=/usr/share/keyrings/githubcli-archive-keyring.gpg] https://cli.github.com/packages stable main" | sudo tee /etc/apt/sources.list.d/github-cli.list > /dev/null \
 && apt update \
 && apt install -y gh

# Python NLP Stuff
EXPOSE 4095 4096 4097 4098 4099

EXPOSE 5901

RUN pip3 uninstall -y  nbconvert Pygments pygments ; /bin/true
RUN apt remove -y python3-pygments python3-h5py python3-packaging python3-nbconvert # python3-requests # python3-six 
RUN apt remove -y python3-gevent python3-greenlet
COPY packs_sys/logicmoo_nlu/requirements.txt /tmp/requirements.txt
#RUN pip3 install --verbose -r /tmp/requirements.txt
RUN pip3 install --verbose filelock==3.4 importlib-metadata==4.4
RUN pip3 install --verbose gevent greenlet spacy nltk nbconvert jupyter jupyterlab requests six gevent.websocket

#RUN cd /opt/logicmoo_workspace/packs_web/butterfly && pip install -e .


#RUN /startup.sh Dockerfile
#CMD $LOGICMOO_WS/StartLogicmoo.sh
ENTRYPOINT ["/startup_logicmoo.sh"]
#ENTRYPOINT ["/startup.sh"]

