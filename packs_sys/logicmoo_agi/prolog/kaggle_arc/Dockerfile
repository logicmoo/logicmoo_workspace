# Use basic Python 3 image as launching point
FROM ubuntu:bionic

RUN apt-get update \
    && apt-get install -y build-essential python3 python3-pip vim unzip

ENV LC_ALL=C.UTF-8
RUN pip3 install kaggle
COPY kaggle.json /root/.kaggle/
RUN chmod 600 /root/.kaggle/kaggle.json

# Add script
# ADD tutorial_code/tutorial_script.py /
# Install dependencies
# RUN pip install numpy
RUN apt update 
RUN mkdir /data
RUN apt-get update -y
RUN apt-get update && apt-get install -y software-properties-common
RUN apt install -y gpg
# RUN LC_ALL=C.UTF-8 add-apt-repository -y ppa:ondrej/php

RUN apt-get install -y unzip apt apt-utils # libpd-dev libcurl14-gnutls-dev 
# RUN docker-php-ext-install gd dev zip curl mongodb mbstring xml fpm
RUN sh -c "echo 'deb http://ppa.launchpad.net/papirus/papirus/ubuntu xenial main' > /etc/apt/sources.list.d/arc-kde-ppa.list"
RUN apt-key adv --recv-keys --keyserver keyserver.ubuntu.com E58A9D36647CAE7F
RUN apt-get update -y
RUN apt-get install software-properties-common -y
RUN LC_ALL=C.UTF-8 apt-add-repository -y ppa:swi-prolog/devel
RUN apt install -y swi-prolog swi-prolog-java libtcmalloc-minimal4
#RUN apt build-dep -y swi-prolog swi-prolog-java libtcmalloc-minimal4 
RUN apt install -y \
        build-essential cmake ninja-build pkg-config \
        ncurses-dev libreadline-dev libedit-dev \
        libgoogle-perftools-dev \
        libgmp-dev \
        libssl-dev \
        unixodbc-dev \
        zlib1g-dev libarchive-dev \
        libossp-uuid-dev \
        libxext-dev libice-dev libjpeg-dev libxinerama-dev libxft-dev \
        libxpm-dev libxt-dev \
        libdb-dev \
        libpcre2-dev \
        libyaml-dev \
        default-jdk junit4

ENV KAGGLEDIR=/kaggle/input/abstraction-and-reasoning-challenge
RUN mkdir -p $KAGGLEDIR
RUN cd $KAGGLEDIR && \
    kaggle competitions download -c abstraction-and-reasoning-challenge && \
    unzip abstraction-and-reasoning-challenge.zip

ENV SRCDIR=/
ENV SRCDIR=/usr/local/src/arc-1st

RUN mkdir -p $SRCDIR
WORKDIR $SRCDIR
RUN kaggle datasets download -d icecuber/arc-solution-source-files && \
    unzip arc-solution-source-files.zip
RUN pip3 install psutil

ENTRYPOINT /bin/bash

RUN mkdir -p /run/sshd

RUN apt install -y --no-install-recommends git make openssh-client sudo openssh-server g++ cmake make libpcap-dev cmake make gcc g++ flex libfl-dev bison libpcap-dev libssl-dev python3 python3-dev swig zlib1g-dev

RUN useradd -rm -d /home/ubuntu -s /bin/bash -g root -G sudo -u 1000 test 
RUN  echo 'test:test' | chpasswd
RUN service ssh start
EXPOSE 22 
RUN apt install -y --no-install-recommends libserd-dev libopenmpi-dev psutils time

ADD ./bin/logicmoo_arcathon_runner $SRCDIR
ADD ./data/ $SRCDIR/data/
ADD ./tutorial_code/ $SRCDIR
ADD ./ARC-solution $SRCDIR

RUN rm -rf ./obj/
RUN rm -f run
RUN make -j
RUN make -j count_tasks

RUN swipl -g "pack_install(predicate_streams,[interactive(false),global(true)])" -t halt
RUN swipl -g "pack_install(logicmoo_utils,[interactive(false),global(true)])" -t halt
RUN swipl -g "pack_install(dictoo,[interactive(false),global(true)])" -t halt
RUN swipl -g "pack_install(logicmoo_webui,[interactive(false),global(true)])" -t halt
RUN swipl -g "pack_install(phil,[interactive(false),global(true)])" -t halt
RUN swipl -g "pack_install(aleph,[interactive(false),global(true)])" -t halt
RUN swipl -g "pack_install(mpi,[interactive(false),global(true)])" -t halt
RUN swipl -g "pack_install(liftcover,[interactive(false),global(true)])" -t halt


# Execute the script
CMD ["/usr/sbin/sshd","-D"]
CMD ["./kaggle_arc_exec", "-g attempt_arcathon_in_docker"]
CMD ["python", "./tutorial_script.py"]
