

cp buildenv.sh /opt/logicmoo_workspace/packs_lib/rocksdb/
cd /opt/logicmoo_workspace/packs_lib/rocksdb/
. buildenv.sh ; make ; make install


cp buildenv.sh /opt/logicmoo_workspace/packs_lib/phil/
cd /opt/logicmoo_workspace/packs_lib/phil/
. buildenv.sh ; make ; make install

cp  buildenv.sh /opt/logicmoo_workspace/packs_lib/bddem/
cd /opt/logicmoo_workspace/packs_lib/bddem/
. buildenv.sh ; make ; make install


cp  buildenv.sh /opt/logicmoo_workspace/packs_web/swish/pack/rserve_client/
cd /opt/logicmoo_workspace/packs_web/swish/pack/rserve_client/
. buildenv.sh ; make ; make install

cp  buildenv.sh /opt/logicmoo_workspace/packs_web/swish/pack/hdt/
cd /opt/logicmoo_workspace/packs_web/swish/pack/hdt/
. buildenv.sh ; make ; make install

cp  buildenv.sh /opt/logicmoo_workspace/packs_web/swish/pack/libssh/
cd /opt/logicmoo_workspace/packs_web/swish/pack/libssh/
. buildenv.sh ; make ; make install




