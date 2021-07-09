#!/bin/bash
if [[ $EUID -ne 0 ]]; then
   echo "#* "
   echo -e "\e[1;31mERROR This script must be run as root. \e[0m"
   echo "#* "
   return 1 2>/dev/null
   exit 1
fi


DIR0="$( cd "$( dirname "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )"

echo DIR0=$DIR0
(

cd $DIR0

export LOGICMOO_WS=$DIR0

./logicmoo_env.sh
echo LOGICMOO_WS=$LOGICMOO_WS


DIR="$LOGICMOO_WS/lib/deps_installed"

if [ -d "$DIR" ]; then

    echo "#* "
    echo "#* GOOD: Logicmoo Deps are hopefully installed"
    echo "#*   (if there was a problem with them rm -rf ${DIR} and restart this script)"
    echo "#* "
    return 0 2>/dev/null
    exit 0

fi

 chmod +x *.md
 chmod +x *.sh
 #apt-get -y install python-software-properties
 #apt-get -y install software-properties-common 
 # apt-get build-dep swi-prolog
 # libcurl4-gnutls-dev
 # libcurl4-openssl-dev
 #add-apt-repository ppa:c2d4u.team/c2d4u4.0+ -y
return 0 2>/dev/null
exit 0












 apt-get update
 apt-get install -y libnet-nslookup-perl 
 apt-get install -y sudo lsof nano vim build-essential cmake ninja-build gdb
 apt-get install -y eggdrop
 mkdir -p /usr/share/man/man1
 apt-add-repository -y 'deb http://security.debian.org/debian-security stretch/updates main'
 apt-get update
 apt-get install -y openjdk-11-jdk
 apt-get install -y openjdk-8-jdk
 #
 # curl -s "https://get.sdkman.io" | bash
 # source "$HOME/.sdkman/bin/sdkman-init.sh"
 # sdk install java 8.0.275.hs-adpt
 #
 # wget -qO - https://adoptopenjdk.jfrog.io/adoptopenjdk/api/gpg/key/public | sudo apt-key add -
 # sudo add-apt-repository --yes https://adoptopenjdk.jfrog.io/adoptopenjdk/deb/
 # sudo apt-get update && apt install -y adoptopenjdk-8-hotspot
 #

 apt-get install -y --no-install-recommends --allow-unauthenticated \
    supervisor openssh-server pwgen sudo net-tools rsync \
        lxde x11vnc xvfb \
        gtk2-engines-murrine libreoffice \
    python-pip python-dev mesa-utils libgl1-mesa-dri \
        gnome-themes-standard gtk2-engines-pixbuf gtk2-engines-murrine arc-theme \
        dbus-x11 x11-utils 

 # second line is BiocManager requirements
 apt-get install -y \
    libxml2-dev libudunits2-dev libgeos++-dev libtiff-dev libopenmpi-dev \
    git curl wget \
    r-base r-base-dev r-cran-bit64 r-cran-rserve r-cran-devtools libffi-dev gdebi-core r-cran-bit64 \
    libssl-dev libcairo-dev \
    ncurses-dev libreadline-dev libedit-dev \
    libgoogle-perftools-dev libunwind-dev libgmp-dev libssl-dev \
    unixodbc-dev  zlib1g-dev libarchive-dev  libossp-uuid-dev \
    libxext-dev libice-dev libjpeg-dev libxinerama-dev libxft-dev \
    libxpm-dev libxt-dev libdb-dev libpcre3-dev libyaml-dev \
    junit4 libssh-dev qt5-default \
    libraptor2-dev 

 apt-get install -y \
    autoconf automake autopoint autotools-dev binutils binutils-common binutils-x86-64-linux-gnu build-essential cmake cmake-data dctrl-tools debhelper \
    dh-autoreconf dh-strip-nondeterminism dpkg-dev dwz g++ g++-8 gcc gcc-8 gettext intltool-debian junit libarchive-dev libarchive-zip-perl libasan5 \
    libbinutils libbsd-dev libc-dev-bin libc6-dev libcc1-0 libdb-dev libdb5.3-dev libdpkg-perl libedit-dev libexpat1-dev libfile-stripnondeterminism-perl libfontconfig1-dev \
    libfreetype6-dev libgcc-8-dev libgmp-dev libgmpxx4ldbl libice-dev libitm1 libjpeg-dev libjsoncpp1 liblsan0 libltdl-dev liblzma-dev libmpx2 \
    libncurses-dev libodbc1 libossp-uuid-dev libossp-uuid16 libpcre16-3 libpcre3-dev libpcre32-3 libpcrecpp0v5 libpng-dev libpthread-stubs0-dev libreadline-dev librhash0 \
    libsigsegv2 libsm-dev libssl-dev libstdc++-8-dev libtool libtsan0 libubsan1 libunwind-dev libx11-dev libxau-dev libxcb1-dev libxdmcp-dev libxext-dev libxft-dev \
    libxinerama-dev libxpm-dev libxrender-dev libxt-dev libyaml-dev linux-libc-dev m4 make odbcinst odbcinst1debian2 patch pkg-config \
    po-debconf unixodbc-dev uuid-dev x11proto-core-dev x11proto-dev x11proto-xext-dev x11proto-xinerama-dev xorg-sgml-doctools xtrans-dev zlib1g-dev \
    texlive-extra-utils gdb libserd-dev libjpeg-turbo-progs libjpeg62 yarn python-dev python-pip python-virtualenv rlwrap psmisc \
    analog gyp libatk-wrapper-java libatk-wrapper-java-jni libc-ares2 libjs-d3 libjs-es5-shim libjs-highlight.js libjs-inherits libjs-is-typedarray libjs-jquery-datatables \
  libjs-jquery-selectize.js libjs-jquery-ui libjs-json libjs-microplugin.js libjs-prettify libjs-sifter.js libjs-twitter-bootstrap-datepicker liblua5.1-0 libnode-dev \
    libnode64 libuv1-dev \
    nodejs-doc pandoc pandoc-data r-cran-base64enc r-cran-filehash r-cran-highr r-cran-shiny r-cran-sourcetools \
  r-cran-testit r-cran-tikzdevice r-cran-tinytex r-cran-xfun r-cran-xtable

  apt install -y libjpeg62-turbo > /dev/null 2>&1
  apt install -y libjpeg62-dev > /dev/null 2>&1
  apt install -y libjpeg62-turbo-dev > /dev/null 2>&1
  apt install -y python3-pip python-pip
  pip uninstall setuptools
  pip install setuptools
  # pip install --upgrade setuptools 
  python -m pip install --upgrade pip setuptools wheel
  python3 -m pip install --upgrade pip setuptools wheel
  pip install butterfly
  pip install butterfly[themes]  # If you want to use themes
  pip install butterfly[systemd]  # If you want to use systemd

 ( cd /etc/systemd/system
     curl -O https://raw.githubusercontent.com/paradoxxxzero/butterfly/master/butterfly.service
     curl -O https://raw.githubusercontent.com/paradoxxxzero/butterfly/master/butterfly.socket
     systemctl enable butterfly.socket
     systemctl start butterfly.socket
  )
  (cd /tmp
 # wget http://archive.ubuntu.com/ubuntu/pool/main/n/ncurses/libncurses6_6.2-0ubuntu2_amd64.deb http://archive.ubuntu.com/ubuntu/pool/main/n/ncurses/libtinfo6_6.2-0ubuntu2_amd64.deb
 # dpkg -i /tmp/libtinfo6_6.2-0ubuntu2_amd64.deb /tmp/libncurses6_6.2-0ubuntu2_amd64.deb 
  )
  pip install --upgrade pip
  pip install tornado asyncio butterfly
 wget http://mirror.umd.edu/eclipse/technology/epp/downloads/release/2020-06/R/eclipse-java-2020-06-R-linux-gtk-x86_64.tar.gz
 sudo tar -zxvf eclipse-java-2020-06-R-linux-gtk-x86_64.tar.gz -C /usr/
 #Symlink eclipse executable to /usr/bin path so that users on the machine can able to use Eclipse.
 sudo ln -s /usr/eclipse/eclipse /usr/bin/eclipse

 echo "#* Maybe: apt install openjdk-11-jdk openjdk-11-jdk-headless"

  mkdir "${DIR}"
)


