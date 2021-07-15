#!/bin/bash

# What the crazy large dev tree might look like 
export GITRC="--recursive"

if [ $# -eq 0 ] 
 then
    export MUDPACK=pack
 else
    export MUDPACK="$1"
fi

./clone_externals.sh $MUDPACK

# very fun complex stuff
git clone $GITRC https://github.com/logicmoo/MUD_PDDL $MUDPACK/MUD_PDDL
git clone $GITRC https://github.com/logicmoo/MUD_DeepParsing $MUDPACK/MUD_DeepParsing
git clone $GITRC https://github.com/logicmoo/MUD_ScriptEngines $MUDPACK/MUD_ScriptEngines


# even more fun complex stuff
git clone $GITRC https://github.com/logicmoo/MUD_KnowRob $MUDPACK/MUD_KnowRob
git clone $GITRC -b indigo-devel --single-branch https://github.com/logicmoo/knowrob_addons.git $MUDPACK/MUD_KnowRob/knowrob_addons
git clone $GITRC -b indigo-devel --single-branch https://github.com/logicmoo/knowrob.git $MUDPACK/MUD_KnowRob/knowrob
git clone $GITRC https://github.com/logicmoo/iai_maps.git $MUDPACK/MUD_KnowRob/iai_maps
git clone $GITRC https://github.com/logicmoo/DLog $MUDPACK/MUD_DLog

# up and comming (not usefull yet)
git clone $GITRC https://github.com/logicmoo/MUD_WebTHEA $MUDPACK/MUD_WebTHEA

git clone $GITRC https://github.com/logicmoo/MUD_DB_LeanCOR $MUDPACK/MUD_DB_LeanCOR


# svn co http://roboticssrv.wtb.tue.nl/svn/ros/user/loyvanbeek/  $MUDPACK/loyvanbeek
# svn co http://km-rdf.googlecode.com/svn/trunk/ $MUDPACK/km_rdf
exit 0



# some updating stuff 
            

 
git clone https://github.com/logicmoo/iai_maps.git 

cd knowrob
git merge upstream/indigo-devel
git checkout indigo-devel
git pull https://github.com/knowrob/knowrob.git indigo-devel
git pull https://github.com/hatguy/knowrob.git indigo-devel
git pull https://github.com/zyfang/knowrob.git indigo-devel
git pull https://github.com/logicmoo/knowrob.git indigo-devel

cd ../knowrob_addons
git merge upstream/indigo-devel
git checkout indigo-devel
git pull https://github.com/knowrob/knowrob_addons.git indigo-devel
git pull https://github.com/hatguy/knowrob_addons.git indigo-devel
git pull https://github.com/zyfang/knowrob_addons.git indigo-devel



