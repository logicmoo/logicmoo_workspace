#!/bin/bash


pathadd() {
  for p in "$@"
  do
    pathadd1 $p
  done
}

pathadd1() {
  for p2 in ${1//:/ }
  do
    pathadd2 $p2
  done
}


pathadd2 ()
{  
   export DCATTER=${DCATTER/"$1:"/} #delete any instances at the beginning
       if [ -n "$1" ] && [ -d "$1" ] ; then 
          export DCATTER="$1:$DCATTER" #prepend to beginning
       fi
}

export SWI_HOME_DIR=/usr/lib/swi-prolog

if [ -z "$SWI_HOME_DIR" ]; then export SWI_HOME_DIR=/usr/lib/swi-prolog ;  fi
if [ -z "$COGBOT_DEV_DIR" ]; then export COGBOT_DEV_DIR=/opt/opensim4opencog ;  fi
if [ -z "$COGBOT_DIR" ]; then export COGBOT_DIR=$COGBOT_DEV_DIR/bin ;  fi
if [ -z "$MONO_PATH" ]; then export MONO_PATH=.:/usr/lib/ikvm:/usr/lib64/mono/4.0:/usr/lib/mono/4.0:$LD_LIBRARY_PATH   ;  fi
if [ -z "$JAVA_HOME" ]; then export JAVA_HOME=/usr/lib/jvm/default-java ;  fi

export DCATTER=
pathadd $PATH
pathadd /root/catkin_ws/devel/bin:/opt/ros/indigo/bin:/root/ros_catkin_ws/install_isolated/bin
pathadd $JAVA_HOME/jre/bin
pathadd $JAVA_HOME/bin
pathadd /usr/lib/qt-3.3/bin
pathadd $COGBOT_DEV_DIR/bin:/usr/lib/mozart/bin
pathadd /root/.nix-profile/bin
pathadd /nix/bin:/usr/kerberos/sbin:/usr/kerberos/bin:/opt/JProbe_7.0.3/bin:/jet6.4-eval/bin
pathadd $JAVA_HOME/bin
pathadd /usr/local/ec2-api-tools-1.3-19403/bin
pathadd /usr/lib64/ccache
pathadd /opt/acl/acl81.64
pathadd /usr/local/lib/acl80.64
pathadd /usr/local/sbin /usr/local/bin /usr/sbin /usr/bin /sbin /bin /usr/games /usr/local/games
pathadd /usr/local/maven-2.0/bin
pathadd /opt/slickedit/bin
pathadd /root/bin
pathadd $LD_LIBRARY_PATH:.:$COGBOT_DIR:$SWI_HOME_DIR/lib/amd64:$JAVA_HOME/lib/amd64/jli:$JAVA_HOME/lib/amd64:$JAVA_HOME/lib/amd64/server:/usr/lib64:/usr/local/lib64:/usr/lib:/usr/local/lib:$SWI_HOME_DIR/lib/x86_64-linux
pathadd $PATH
pathadd /usr/bin
pathadd $MONO_PATH
export PATH="$DCATTER"
export LD_LIBRARY_PATH="$DCATTER"
export MONO_PATH="$DCATTER"





