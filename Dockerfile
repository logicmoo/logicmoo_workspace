FROM logicmoo/logicmoo_starter_image
USER root

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
        openjdk-8-jdk  build-essential zlib1g-dev libncurses5-dev libgdbm-dev libnss3-dev libssl-dev libreadline-dev libffi-dev libsqlite3-dev wget libbz2-dev \
        rlwrap dbus-x11 firefox net-tools ; /bin/true


COPY docker/rootfs /
ENV PYTHONUNBUFFERED 1
RUN mkdir /code
WORKDIR /code
RUN sed -i "s/buster/testing/" /etc/apt/sources.list && \
    sed -n -i "/security/!p" /etc/apt/sources.list
RUN cat /etc/apt/sources.list
RUN pip install jupyter
RUN apt-get update && apt-get install -y \ 
    guile-3.0 guile-3.0-dev automake gcc libunwind-dev build-essential
RUN wget https://github.com/zeromq/libzmq/releases/download/v4.3.4/zeromq-4.3.4.tar.gz && \
    tar xvf zeromq-4.3.4.tar.gz && cd /code/zeromq-4.3.4/ && \
    ./autogen.sh && ./configure CXXFLAGS='-Wno-error -Wno-error=stringop-truncation' && make && make install
RUN wget http://download.savannah.gnu.org/releases/guile-json/guile-json-3.2.0.tar.gz && \
    tar xvf guile-json-3.2.0.tar.gz && cd guile-json-3.2.0 && \
    ./configure --prefix=/usr && make && make install
# guile-zimple-zmq
RUN git clone https://github.com/jerry40/guile-simple-zmq.git && \
    cd ./guile-simple-zmq && \
    autoreconf --verbose --install --force && \
    ./configure --prefix=/usr && make && make install
#
RUN mkdir -p /usr/local/share/jupyter/kernels/guile
WORKDIR /usr/local/share/jupyter/kernels/guile
RUN git clone https://github.com/jerry40/guile-kernel.git
RUN cp ./guile-kernel/src/kernel.json .
#EXPOSE 8888
RUN groupadd -r jupyter_users ; /bin/true
RUN useradd --no-log-init -m -r -g jupyter_users opencog ; /bin/true
WORKDIR /home/opencog
#USER root
RUN /in_container_build.sh
#CMD $LOGICMOO_WS/StartLogicmoo.sh
#USER opencog
ENTRYPOINT ["/startup_logicmoo.sh"]
#ENTRYPOINT ["/startup.sh"]
RUN sleep 1000000


