FROM logicmoo/logicmoo_starter_image

USER root
LABEL maintainer = "logicmoo@gmail.com"

# SSHD
EXPOSE 22    
# Apache HTTP 
EXPOSE 80    
# Apache SSL 
EXPOSE 443 4443
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

FROM ubuntu:18.04 AS ccls
RUN apt-get update \
	&& apt-get upgrade -y \
	&& apt-get install -y build-essential cmake clang libclang-dev zlib1g-dev git wget \
	&& git clone --depth=1 --recursive https://github.com/MaskRay/ccls \
	&& cd ccls \
	&& wget -c http://releases.llvm.org/8.0.0/clang+llvm-8.0.0-x86_64-linux-gnu-ubuntu-18.04.tar.xz \
	&& tar xf clang+llvm-8.0.0-x86_64-linux-gnu-ubuntu-18.04.tar.xz \
	&& cmake -H. -BRelease -DCMAKE_BUILD_TYPE=Release -DCMAKE_PREFIX_PATH=$PWD/clang+llvm-8.0.0-x86_64-linux-gnu-ubuntu-18.04 \
	&& cmake --build Release 

FROM ubuntu:18.04 AS go
RUN apt-get update \
	&& apt-get upgrade -y \
	&& apt-get install -y wget \
	&& export LATEST_VERSION=`wget -qO- https://golang.org/dl | grep -oE go[0-9]+\.[0-9]+\.[0-9]+\.linux-amd64\.tar\.gz | head -n 1` \
	&& wget -c https://dl.google.com/go/$LATEST_VERSION \
	&& tar -xzf $LATEST_VERSION

FROM ubuntu:18.04
# General
RUN apt-get update \
	&& apt-get upgrade -y  \
	&& apt-get install -y git

# C-Family
COPY --from=ccls /ccls /ccls
RUN ln -s /ccls/Release/ccls /usr/bin/ccls \
	&& ln -s /ccls/clang+llvm-8.0.0-x86_64-linux-gnu-ubuntu-18.04/bin/clangd /usr/bin/clangd

# Go
COPY --from=go /go /go
ENV PATH "${PATH}:/go/bin:/root/go/bin"
RUN /go/bin/go get -u golang.org/x/tools/gopls

# NPM installed language servers
RUN apt-get install -y npm \
	&& npm i -g \
	bash-language-server \
	vscode-css-languageserver-bin \
	vscode-html-languageserver-bin \
	dockerfile-language-server-nodejs \
	typescript-language-server \
	typescript

# Python
RUN apt-get install -y python3-pip \
	&& pip3 install 'python-language-server[all]'


# Emacs
RUN apt-get update \
	&& apt-get upgrade -y \
	&& apt-get install -y build-essential git autoconf texinfo libgnutls28-dev libxml2-dev libncurses5-dev libjansson-dev \
	&& git clone --depth=1 git://git.sv.gnu.org/emacs.git /emacs \
	&& cd /emacs && ./autogen.sh \
	&& ./configure --with-modules --with-json \
	&& make -j4 && make install

ENV HOME /root



COPY docker/rootfs /

RUN echo enable some apache mods \
 && a2dismod mpm_event \
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
ENV LOGICMOO_GAMES $LOGICMOO_WS/packs_sys/prologmud_samples/prolog/prologmud_sample_games

ENV PATH "${LOGICMOO_WS}/bin:${PATH}"
ENV WNDB $LOGICMOO_WS/packs_sys/logicmoo_nlu/data/WNprolog-3.0/prolog

MAINTAINER RUN cd $LOGICMOO_WS && set -x \
 && git checkout . \
 && git submodule update --init . \
 && cd $LOGICMOO_WS/packs_sys/logicmoo_nlu/ext/pldata && swipl -g "time(qcompile(wn_iface)),halt." \
 && cd $LOGICMOO_WS/packs_sys/logicmoo_nlu/ext/pldata && swipl -g "time(qcompile(tt0_00022_cycl)),halt." \
 \
 && cd $LOGICMOO_WS/packs_xtra/logicmoo_pldata \
 && git checkout . \
 && git checkout master \
 && cd $LOGICMOO_WS/packs_xtra/logicmoo_pldata/ext/plkb0988 \
 && swipl -g "time(qcompile(plkb0988_kb)),halt." \
 && git status \
 && git add -f plkb0988_kb.qlf \
 && cd $LOGICMOO_WS/packs_xtra/ \
 && git add -f . \
 && git commit -am "plkb0988-$(date)" \
 && cd $LOGICMOO_WS/packs_xtra/ \
 && git add logicmoo_pldata \
 && git commit -am "logicmoo_pldata-$(date)" \
 && rm -rf $LOGICMOO_WS/packs_xtra/logicmoo_pldata/*/

#CMD $LOGICMOO_WS/StartLogicmoo.sh

