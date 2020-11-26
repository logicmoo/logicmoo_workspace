#!/bin/sh

ROOT=`pwd`

# delete old gfc and gfr files 
rm -f *.gfc *.gfr */*.gfc */*.gfr */*/*.gfc */*/*.gfr */*/*/*.gfc */*/*/*.gfr

# compile deLux grammars:
GRPATH=GF_GoDiS/Domain/deLux
APPPATH=delux

cd ${GRPATH}

gf < compile_sys_domain_lamps.gfscript
gf < compile_usr_domain_lamps.gfscript

cd ${ROOT}

cp ${GRPATH}/*.cfgm ${APPPATH}/Grammars
cp ${GRPATH}/*.gfcm ${APPPATH}/Grammars
cp ${GRPATH}/*.grammar ${APPPATH}/Grammars
cp ${GRPATH}/*.properties ${APPPATH}/Grammars

# compile djgodis grammars:
GRPATH=GF_GoDiS/Domain/DJGoDiS
APPPATH=djgodis

cd ${GRPATH}

gf < compile_sys_domain_player.gfscript
gf < compile_usr_domain_player.gfscript

cd  ${ROOT}

cp ${GRPATH}/*.cfgm ${APPPATH}/Grammars
cp ${GRPATH}/*.gfcm ${APPPATH}/Grammars
cp ${GRPATH}/*.grammar ${APPPATH}/Grammars
cp ${GRPATH}/*.properties ${APPPATH}/Grammars

