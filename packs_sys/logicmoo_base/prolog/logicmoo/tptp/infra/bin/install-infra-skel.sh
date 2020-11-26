#!/bin/sh

USERNAME=${1:-${USER}}
SKELDIR=${HOME}/.infra

if [[ -a ${SKELDIR} ]]
then
echo ${SKELDIR} already exists - Exiting
exit 1
fi

DIR=${INFRADIR:-${HOME}/infra}
umask 0077
mkdir -v ${SKELDIR}
mkdir -v ${SKELDIR}/tmp
cp -v ${DIR}/lib/skel/infra/config.rdf ${SKELDIR}
echo Creating Infra user ${USERNAME}
if htpasswd -c ${SKELDIR}/passwd ${USERNAME}
then
  echo Password data written to ${SKELDIR}/passwd
else
  exit 1
fi
