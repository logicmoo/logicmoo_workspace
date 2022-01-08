# FUSE filesystem over Google Drive

Complete guide to go to hell and back. &copy; [@Lion](https://d1u5p3l4wpay3k.cloudfront.net/dota2_gamepedia/4/4a/Lion_respawn_01.mp3) :sound:

Install **google-drive-ocamlfuse** on **Debian Jessie** and later.

## Preinstall

Source: https://github.com/astrada/google-drive-ocamlfuse/wiki/How-to-install-from-source-on-Debian-Jessie

```shell
su
apt-get install software-properties-common sudo ssh
exit
```

Continue with sudo:

```shell
sudo apt-get install opam ocaml make fuse camlp4-extra build-essential pkg-config
```

## Fuse

Create fuse group (if not already present):

```shell
sudo groupadd fuse
```

Debian has a special user group to allow fuse access. Log out, and log back in after this to make change effective:

> Replace `*username*` with your real username

```shell
sudo usermod -a -G fuse *username*
sudo chown root:fuse /dev/fuse
sudo chmod 660 /dev/fuse
```

## Install packages 

```shell
opam init
opam update
opam install depext
eval `opam config env`
opam depext google-drive-ocamlfuse
opam install google-drive-ocamlfuse
. /home/*username*/.opam/opam-init/init.sh > /dev/null 2> /dev/null || true
```

## Reboot

Yes, do it.

```shell
sudo reboot
```

## Authorize your account

Source: https://github.com/astrada/google-drive-ocamlfuse/wiki/Headless-Usage-&-Authorization

* Sign in to your Google account and create a project: https://console.cloud.google.com/
* Click "API Manager" then "Library" in the left-hand pane (will take you to https://console.cloud.google.com/apis/library). Click on "Drive API", then "ENABLE API".
* Click "Credentials" in the left hand pane, then click on the button "Create Credentials" (OAuth client ID)
    * Choose "Other"
    * Choose any product name, e.g "My OCAMLDrive".
    * Click "Create". You will get a Client ID, a Client Secret.
* Authorization: Back in your headless server, run google-drive-ocamlfuse for the first time. I used labels (in this document, I use the label "me") because I plan on using multiple accounts. However you can also run it without the -label parameter and it will use a default name for the label called "default". You will need the Client ID and secret you got from google above.

From version 0.5.3, you should use the -headless option:

> Replace `*yourClientID*` and `*yourSecret*` with your real credentials

```shell
google-drive-ocamlfuse -headless -label me -id *yourClientID*.apps.googleusercontent.com -secret *yourSecret* 
```

Example output:

```shell
Please, open the following URL in a web browser: https://accounts.google.com/o/oauth2/auth?client_id=##yourClientID##.apps.googleusercontent.com&redirect_uri=urn%3Aietf%3Awg%3Aoauth%3A2.0%3Aoob&scope=https%3A%2F%2Fwww.googleapis.com%2Fauth%2Fdrive&response_type=code&access_type=offline&approval_prompt=force
Then, you should open the URL above in a graphical web browser, get the verification code from Google and put it here:
Please enter the verification code: 
```

That's it. You should be ready to mount.

> Replace `*username*` with your real username

```shell
mkdir /home/*username*/gdrive
```

Try mount:

```shell
google-drive-ocamlfuse -label me /home/*username*/gdrive
```

## Mount on startup using systemd

You need mount script and service configuration

### Mount script

Source/tips: https://linuxconfig.org/how-to-automatically-execute-shell-script-at-startup-boot-on-systemd-linux

```shell
cd /usr/local/bin
nano mount-gdrive.sh
```

Type script content:

> Replace `*username*` with your real username

```shell
#!/bin/sh

su *username* -l -c "google-drive-ocamlfuse -label me /home/*username*/gdrive"
```

Fix permission:

```shell
chmod 755 mount-gdrive.sh
```

## Systemd service

Go to systemd directory:

```shell
cd /etc/systemd/system
nano gdrive.service
```

Type in file content:

```ini
[Unit]
After=networking.service

[Service]
ExecStart=/usr/local/bin/mount-gdrive.sh

[Install]
WantedBy=default.target
```

Configure service:

```shell
chmod 664 /etc/systemd/system/gdrive.service
systemctl daemon-reload
systemctl enable gdrive.service
```

### Testing before reboot [optional]

Test your configuration:

```shell
systemctl start gdrive.service
```

GG WP :video_game: :clap: :feelsgood: :smiling_imp: :coffee:

/opt/logicmoo_workspace/packs_web_waz/logicmoo_webui
/opt/logicmoo_workspace/packs_web_waz/swish
/opt/logicmoo_workspace/packs_web_waz/ClioPatria
/opt/logicmoo_workspace/prologmud_server
origin	https://logicmoo.org:2082/gitlab/logicmoo/prologmud_server.git/ (fetch)
origin	https://logicmoo.org:2082/gitlab/logicmoo/prologmud_server.git/ (push)
/opt/logicmoo_workspace/packs_sys/logicmoo_lps_save2
/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/ext/dAceRules/defeasible-rules
origin	ifigit@git.informatik.uni-leipzig.de:strass/defeasible-rules.git (fetch)
origin	ifigit@git.informatik.uni-leipzig.de:strass/defeasible-rules.git (push)
/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/ext/chat80/FOIL_Prolog
origin	https://github.com/pashok3d/FOIL_Prolog (fetch)
origin	https://github.com/pashok3d/FOIL_Prolog (push)
/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/ext/link-grammar
origin	https://github.com/opencog/link-grammar (fetch)
origin	https://github.com/opencog/link-grammar (push)
/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/ext/dApe
origin	ifigit@git.informatik.uni-leipzig.de:strass/APE.git (fetch)
origin	ifigit@git.informatik.uni-leipzig.de:strass/APE.git (push)
/opt/logicmoo_workspace/packs_sys/logicmoo_ec
/opt/logicmoo_workspace/packs_sys/logicmoo_base/prolog/logicmoo/tptp/plcop
origin	https://github.com/zsoltzombori/plcop (fetch)
origin	https://github.com/zsoltzombori/plcop (push)
/opt/logicmoo_workspace/packs_sys/logicmoo_base/prolog/logicmoo/tptp/plcop/pyswip
origin	https://github.com/yuce/pyswip.git (fetch)
origin	https://github.com/yuce/pyswip.git (push)
/opt/logicmoo_workspace/packs_sys/logicmoo_base/prolog/logicmoo/tptp/plcop/hashtbl
origin	https://github.com/gergo-/hashtbl.git (fetch)
origin	https://github.com/gergo-/hashtbl.git (push)
/opt/logicmoo_workspace/packs_sys/prologmud_samples
/opt/logicmoo_workspace/packs_sys/unused~/discordia-slash
origin	https://github.com/GitSparTV/discordia-slash (fetch)
origin	https://github.com/GitSparTV/discordia-slash (push)
/opt/logicmoo_workspace/packs_sys/unused~/Discordia
origin	https://github.com/SinisterRectus/Discordia (fetch)
origin	https://github.com/SinisterRectus/Discordia (push)
/opt/logicmoo_workspace/packs_sys/predicate_streams
/opt/logicmoo_workspace/packs_sys/instant_prolog_docs
/opt/logicmoo_workspace/packs_sys/wam_common_lisp
/opt/logicmoo_workspace/packs_sys/opencog/atomspace
origin	https://github.com/opencog/atomspace (fetch)
origin	https://github.com/opencog/atomspace (push)
/opt/logicmoo_workspace/packs_sys/opencog/pln
origin	https://github.com/ngeiswei/pln (fetch)
origin	https://github.com/ngeiswei/pln (push)
/opt/logicmoo_workspace/packs_sys/opencog/guile
origin	https://git.sv.gnu.org/git/guile.git (fetch)
origin	https://git.sv.gnu.org/git/guile.git (push)
/opt/logicmoo_workspace/packs_sys/opencog/cogutil
origin	https://github.com/opencog/cogutil (fetch)
origin	https://github.com/opencog/cogutil (push)
/opt/logicmoo_workspace/packs_sys/opencog/nbdev
origin	https://github.com/fastai/nbdev (fetch)
origin	https://github.com/fastai/nbdev (push)
/opt/logicmoo_workspace/packs_sys/opencog/cogprotolab
origin	https://github.com/opencog/cogprotolab (fetch)
origin	https://github.com/opencog/cogprotolab (push)
/opt/logicmoo_workspace/packs_sys/opencog/guile-persist
origin	https://gitlab.com/tampe/guile-persist (fetch)
origin	https://gitlab.com/tampe/guile-persist (push)
/opt/logicmoo_workspace/packs_sys/opencog/malmo
origin	https://github.com/microsoft/malmo (fetch)
origin	https://github.com/microsoft/malmo (push)
/opt/logicmoo_workspace/packs_sys/opencog/guile-syntax-parse
origin	https://gitlab.com/guile-syntax-parse/guile-syntax-parse.git (fetch)
origin	https://gitlab.com/guile-syntax-parse/guile-syntax-parse.git (push)
/opt/logicmoo_workspace/packs_sys/opencog/ure
origin	https://github.com/opencog/ure (fetch)
origin	https://github.com/opencog/ure (push)
/opt/logicmoo_workspace/packs_sys/opencog/rocca
origin	https://github.com/ngeiswei/rocca (fetch)
origin	https://github.com/ngeiswei/rocca (push)
/opt/logicmoo_workspace/packs_sys/opencog/QuProlog10.1
origin	https://github.com/DouglasRMiles/QuProlog (fetch)
origin	https://github.com/DouglasRMiles/QuProlog (push)
/opt/logicmoo_workspace/packs_sys/opencog/bdw-gc-logical-mod
origin	https://gitlab.com/bdw-gc-logical-mod/bdw-gc-logical-mod (fetch)
origin	https://gitlab.com/bdw-gc-logical-mod/bdw-gc-logical-mod (push)
/opt/logicmoo_workspace/packs_sys/opencog/asmoses
origin	https://github.com/ngeiswei/asmoses (fetch)
origin	https://github.com/ngeiswei/asmoses (push)
/opt/logicmoo_workspace/packs_sys/opencog/QuProlog
origin	https://github.com/DouglasRMiles/QuProlog (fetch)
origin	https://github.com/DouglasRMiles/QuProlog (push)
/opt/logicmoo_workspace/packs_sys/opencog/guile-log
origin	https://gitlab.com/gule-log/guile-log (fetch)
origin	https://gitlab.com/gule-log/guile-log (push)
/opt/logicmoo_workspace/packs_sys/opencog/spacetime
origin	https://github.com/opencog/spacetime (fetch)
origin	https://github.com/opencog/spacetime (push)
/opt/logicmoo_workspace/packs_sys/opencog/miner
origin	https://github.com/opencog/miner (fetch)
origin	https://github.com/opencog/miner (push)
/opt/logicmoo_workspace/packs_sys/logicmoo_opencog/atomspace
origin	https://github.com/opencog/atomspace (fetch)
origin	https://github.com/opencog/atomspace (push)
/opt/logicmoo_workspace/packs_sys/logicmoo_opencog/pln
origin	https://github.com/ngeiswei/pln (fetch)
origin	https://github.com/ngeiswei/pln (push)
/opt/logicmoo_workspace/packs_sys/logicmoo_opencog/guile
origin	https://git.sv.gnu.org/git/guile.git (fetch)
origin	https://git.sv.gnu.org/git/guile.git (push)
/opt/logicmoo_workspace/packs_sys/logicmoo_opencog/cogutil
origin	https://github.com/opencog/cogutil (fetch)
origin	https://github.com/opencog/cogutil (push)
/opt/logicmoo_workspace/packs_sys/logicmoo_opencog/nbdev
origin	https://github.com/fastai/nbdev (fetch)
origin	https://github.com/fastai/nbdev (push)
/opt/logicmoo_workspace/packs_sys/logicmoo_opencog/cogprotolab
origin	https://github.com/opencog/cogprotolab (fetch)
origin	https://github.com/opencog/cogprotolab (push)
/opt/logicmoo_workspace/packs_sys/logicmoo_opencog/guile-persist
origin	https://gitlab.com/tampe/guile-persist (fetch)
origin	https://gitlab.com/tampe/guile-persist (push)
/opt/logicmoo_workspace/packs_sys/logicmoo_opencog/malmo
origin	https://github.com/microsoft/malmo (fetch)
origin	https://github.com/microsoft/malmo (push)
/opt/logicmoo_workspace/packs_sys/logicmoo_opencog/guile-syntax-parse
origin	https://gitlab.com/guile-syntax-parse/guile-syntax-parse.git (fetch)
origin	https://gitlab.com/guile-syntax-parse/guile-syntax-parse.git (push)
/opt/logicmoo_workspace/packs_sys/logicmoo_opencog/ure
origin	https://github.com/opencog/ure (fetch)
origin	https://github.com/opencog/ure (push)
/opt/logicmoo_workspace/packs_sys/logicmoo_opencog/rocca
origin	https://github.com/ngeiswei/rocca (fetch)
origin	https://github.com/ngeiswei/rocca (push)
/opt/logicmoo_workspace/packs_sys/logicmoo_opencog/QuProlog10.1
origin	https://github.com/DouglasRMiles/QuProlog (fetch)
origin	https://github.com/DouglasRMiles/QuProlog (push)
/opt/logicmoo_workspace/packs_sys/logicmoo_opencog/bdw-gc-logical-mod
origin	https://gitlab.com/bdw-gc-logical-mod/bdw-gc-logical-mod (fetch)
origin	https://gitlab.com/bdw-gc-logical-mod/bdw-gc-logical-mod (push)
/opt/logicmoo_workspace/packs_sys/logicmoo_opencog/asmoses
origin	https://github.com/ngeiswei/asmoses (fetch)
origin	https://github.com/ngeiswei/asmoses (push)
/opt/logicmoo_workspace/packs_sys/logicmoo_opencog/QuProlog
origin	https://github.com/DouglasRMiles/QuProlog (fetch)
origin	https://github.com/DouglasRMiles/QuProlog (push)
/opt/logicmoo_workspace/packs_sys/logicmoo_opencog/guile-log
origin	https://gitlab.com/gule-log/guile-log (fetch)
origin	https://gitlab.com/gule-log/guile-log (push)
/opt/logicmoo_workspace/packs_sys/logicmoo_opencog/spacetime
origin	https://github.com/opencog/spacetime (fetch)
origin	https://github.com/opencog/spacetime (push)
/opt/logicmoo_workspace/packs_sys/logicmoo_opencog/miner
origin	https://github.com/opencog/miner (fetch)
origin	https://github.com/opencog/miner (push)
/opt/logicmoo_workspace/packs_sys/logicmoo_agi/prolog/theory-toolbox-2
origin	https://github.com/JeanChristopheRohner/theory-toolbox-2 (fetch)
origin	https://github.com/JeanChristopheRohner/theory-toolbox-2 (push)
/opt/logicmoo_workspace/packs_sys/logicmoo_agi/prolog/malmo
origin	https://github.com/Microsoft/malmo.git (fetch)
origin	https://github.com/Microsoft/malmo.git (push)
/opt/logicmoo_workspace/packs_sys/logicmoo_agi/prolog/rl/numpy
origin	https://github.com/numpy/numpy.git (fetch)
origin	https://github.com/numpy/numpy.git (push)
/opt/logicmoo_workspace/packs_sys/logicmoo_agi/prolog/rl/clingo
origin	https://github.com/potassco/clingo (fetch)
origin	https://github.com/potassco/clingo (push)
/opt/logicmoo_workspace/packs_sys/logicmoo_agi/prolog/rl/venv/src/vgdl
origin	https://github.com/rubenvereecken/py-vgdl/ (fetch)
origin	https://github.com/rubenvereecken/py-vgdl/ (push)
/opt/logicmoo_workspace/packs_sys/logicmoo_agi/prolog/theory-toolbox
origin	https://github.com/JeanChristopheRohner/theory-toolbox (fetch)
origin	https://github.com/JeanChristopheRohner/theory-toolbox (push)
/opt/logicmoo_workspace/packs_sys/dictoo
/opt/logicmoo_workspace/packs_sys/pfc
/opt/logicmoo_workspace/packs_sys/eggdrop
/opt/logicmoo_workspace/packs_sys/logicmoo_lps
/opt/logicmoo_workspace/packs_sys/prologmud
/opt/logicmoo_workspace/packs_sys/logicmoo_nars/OON/opennars
origin	https://github.com/opennars/opennars.git (fetch)
origin	https://github.com/opennars/opennars.git (push)
/opt/logicmoo_workspace/packs_sys/logicmoo_nars/OON/opennars-parent
origin	https://github.com/opennars/opennars-parent.git (fetch)
origin	https://github.com/opennars/opennars-parent.git (push)
/opt/logicmoo_workspace/packs_sys/logicmoo_nars/OON/OpenNARS-for-Applications
origin	https://github.com/opennars/OpenNARS-for-Applications (fetch)
origin	https://github.com/opennars/OpenNARS-for-Applications (push)
/opt/logicmoo_workspace/packs_sys/logicmoo_nars/OON/opennars-lab
origin	https://github.com/opennars/opennars-lab.git (fetch)
origin	https://github.com/opennars/opennars-lab.git (push)
/opt/logicmoo_workspace/packs_sys/logicmoo_nars/OON/opennars-gui
origin	https://github.com/opennars/opennars-gui.git (fetch)
origin	https://github.com/opennars/opennars-gui.git (push)
/opt/logicmoo_workspace/packs_sys/logicmoo_nars/OON/opennars-applications
origin	https://github.com/opennars/opennars-applications.git (fetch)
origin	https://github.com/opennars/opennars-applications.git (push)
/opt/logicmoo_workspace/packs_sys/swicli
/opt/logicmoo_workspace/packs_sys/sigma_ace
/opt/logicmoo_workspace/packs_sys/logicmoo_cg
/opt/logicmoo_workspace
github	https://github.com/logicmoo/logicmoo_workspace.git (fetch)
github	https://github.com/logicmoo/logicmoo_workspace.git (push)
gitlab	https://logicmoo.org/gitlab/logicmoo/logicmoo_workspace.git (fetch)
gitlab	https://logicmoo.org/gitlab/logicmoo/logicmoo_workspace.git (push)
origin	https://logicmoo.org/gitlab/logicmoo/logicmoo_workspace (fetch)
origin	https://logicmoo.org/gitlab/logicmoo/logicmoo_workspace (push)
symbolic-rl	/symbolic-rl (fetch)
symbolic-rl	/symbolic-rl (push)
/opt/logicmoo_workspace/packs_web/logicmoo_webui
/opt/logicmoo_workspace/packs_web/swish
/opt/logicmoo_workspace/packs_web/neuro
origin	https://github.com/shawncplus/neuro (fetch)
origin	https://github.com/shawncplus/neuro (push)
/opt/logicmoo_workspace/packs_web/ClioPatria
/opt/logicmoo_workspace/packs_lib/sCASP
origin	https://github.com/JanWielemaker/sCASP.git (fetch)
origin	https://github.com/JanWielemaker/sCASP.git (push)
/opt/logicmoo_workspace/packs_lib/rocksdb/rocksdb
origin	https://github.com/facebook/rocksdb (fetch)
origin	https://github.com/facebook/rocksdb (push)
/opt/logicmoo_workspace/packs_lib/prologterms-py
origin	https://github.com/cmungall/prologterms-py (fetch)
origin	https://github.com/cmungall/prologterms-py (push)
/opt/logicmoo_workspace/packs_lib/PythonPengines
origin	https://github.com/ian-andrich/PythonPengines (fetch)
origin	https://github.com/ian-andrich/PythonPengines (push)
/opt/logicmoo_workspace/TextWorld
origin	https://github.com/microsoft/TextWorld (fetch)
origin	https://github.com/microsoft/TextWorld (push)
/opt/logicmoo_workspace/swipl-devel/packages/archive
origin	https://github.com/SWI-Prolog/packages-archive.git (fetch)
origin	https://github.com/SWI-Prolog/packages-archive.git (push)
/opt/logicmoo_workspace/swipl-devel/packages/utf8proc
origin	https://github.com/SWI-Prolog/packages-utf8proc.git (fetch)
origin	https://github.com/SWI-Prolog/packages-utf8proc.git (push)
/opt/logicmoo_workspace/swipl-devel/packages/jpl
origin	https://github.com/SWI-Prolog/packages-jpl.git (fetch)
origin	https://github.com/SWI-Prolog/packages-jpl.git (push)
/opt/logicmoo_workspace/swipl-devel/packages/cpp
origin	https://github.com/SWI-Prolog/packages-cpp.git (fetch)
origin	https://github.com/SWI-Prolog/packages-cpp.git (push)
/opt/logicmoo_workspace/swipl-devel/packages/yaml
origin	https://github.com/SWI-Prolog/packages-yaml.git (fetch)
origin	https://github.com/SWI-Prolog/packages-yaml.git (push)
/opt/logicmoo_workspace/swipl-devel/packages/pldoc
origin	https://github.com/SWI-Prolog/packages-pldoc.git (fetch)
origin	https://github.com/SWI-Prolog/packages-pldoc.git (push)
/opt/logicmoo_workspace/swipl-devel/packages/paxos
origin	https://github.com/SWI-Prolog/packages-paxos.git (fetch)
origin	https://github.com/SWI-Prolog/packages-paxos.git (push)
/opt/logicmoo_workspace/swipl-devel/packages/pengines
origin	https://github.com/SWI-Prolog/packages-pengines.git (fetch)
origin	https://github.com/SWI-Prolog/packages-pengines.git (push)
/opt/logicmoo_workspace/swipl-devel/packages/tipc
origin	https://github.com/SWI-Prolog/contrib-tipc.git (fetch)
origin	https://github.com/SWI-Prolog/contrib-tipc.git (push)
/opt/logicmoo_workspace/swipl-devel/packages/mqi
origin	https://github.com/SWI-Prolog/packages-mqi.git (fetch)
origin	https://github.com/SWI-Prolog/packages-mqi.git (push)
/opt/logicmoo_workspace/swipl-devel/packages/language_server
origin	https://github.com/SWI-Prolog/packages-language_server.git (fetch)
origin	https://github.com/SWI-Prolog/packages-language_server.git (push)
/opt/logicmoo_workspace/swipl-devel/packages/plunit
origin	https://github.com/SWI-Prolog/packages-plunit.git (fetch)
origin	https://github.com/SWI-Prolog/packages-plunit.git (push)
/opt/logicmoo_workspace/swipl-devel/packages/sgml
origin	https://github.com/SWI-Prolog/packages-sgml.git (fetch)
origin	https://github.com/SWI-Prolog/packages-sgml.git (push)
/opt/logicmoo_workspace/swipl-devel/packages/readline
origin	https://github.com/SWI-Prolog/packages-readline.git (fetch)
origin	https://github.com/SWI-Prolog/packages-readline.git (push)
/opt/logicmoo_workspace/swipl-devel/packages/RDF
origin	https://github.com/SWI-Prolog/packages-RDF.git (fetch)
origin	https://github.com/SWI-Prolog/packages-RDF.git (push)
/opt/logicmoo_workspace/swipl-devel/packages/cppproxy
origin	https://github.com/SWI-Prolog/packages-cppproxy.git (fetch)
origin	https://github.com/SWI-Prolog/packages-cppproxy.git (push)
/opt/logicmoo_workspace/swipl-devel/packages/PDT
origin	https://github.com/SWI-Prolog/packages-PDT.git (fetch)
origin	https://github.com/SWI-Prolog/packages-PDT.git (push)
/opt/logicmoo_workspace/swipl-devel/packages/nlp
origin	https://github.com/SWI-Prolog/packages-nlp.git (fetch)
origin	https://github.com/SWI-Prolog/packages-nlp.git (push)
/opt/logicmoo_workspace/swipl-devel/packages/clib
origin	https://github.com/SWI-Prolog/packages-clib.git (fetch)
origin	https://github.com/SWI-Prolog/packages-clib.git (push)
/opt/logicmoo_workspace/swipl-devel/packages/ltx2htm
origin	https://github.com/SWI-Prolog/packages-ltx2htm.git (fetch)
origin	https://github.com/SWI-Prolog/packages-ltx2htm.git (push)
/opt/logicmoo_workspace/swipl-devel/packages/semweb
origin	https://github.com/SWI-Prolog/packages-semweb.git (fetch)
origin	https://github.com/SWI-Prolog/packages-semweb.git (push)
/opt/logicmoo_workspace/swipl-devel/packages/protobufs
origin	https://github.com/SWI-Prolog/contrib-protobufs.git (fetch)
origin	https://github.com/SWI-Prolog/contrib-protobufs.git (push)
/opt/logicmoo_workspace/swipl-devel/packages/cql
origin	https://github.com/SWI-Prolog/packages-cql.git (fetch)
origin	https://github.com/SWI-Prolog/packages-cql.git (push)
/opt/logicmoo_workspace/swipl-devel/packages/zlib
origin	https://github.com/SWI-Prolog/packages-zlib.git (fetch)
origin	https://github.com/SWI-Prolog/packages-zlib.git (push)
/opt/logicmoo_workspace/swipl-devel/packages/clpqr
origin	https://github.com/SWI-Prolog/packages-clpqr.git (fetch)
origin	https://github.com/SWI-Prolog/packages-clpqr.git (push)
/opt/logicmoo_workspace/swipl-devel/packages/inclpr
origin	https://github.com/SWI-Prolog/packages-inclpr.git (fetch)
origin	https://github.com/SWI-Prolog/packages-inclpr.git (push)
/opt/logicmoo_workspace/swipl-devel/packages/redis
origin	https://github.com/SWI-Prolog/packages-redis.git (fetch)
origin	https://github.com/SWI-Prolog/packages-redis.git (push)
/opt/logicmoo_workspace/swipl-devel/packages/stomp
origin	https://github.com/SWI-Prolog/packages-stomp.git (fetch)
origin	https://github.com/SWI-Prolog/packages-stomp.git (push)
/opt/logicmoo_workspace/swipl-devel/packages/pcre
origin	https://github.com/SWI-Prolog/packages-pcre.git (fetch)
origin	https://github.com/SWI-Prolog/packages-pcre.git (push)
/opt/logicmoo_workspace/swipl-devel/packages/bdb
origin	https://github.com/SWI-Prolog/packages-bdb.git (fetch)
origin	https://github.com/SWI-Prolog/packages-bdb.git (push)
/opt/logicmoo_workspace/swipl-devel/packages/windows
origin	https://github.com/SWI-Prolog/packages-windows.git (fetch)
origin	https://github.com/SWI-Prolog/packages-windows.git (push)
/opt/logicmoo_workspace/swipl-devel/packages/swipl-win
origin	https://github.com/SWI-Prolog/packages-swipl-win.git (fetch)
origin	https://github.com/SWI-Prolog/packages-swipl-win.git (push)
/opt/logicmoo_workspace/swipl-devel/packages/libedit
origin	https://github.com/SWI-Prolog/packages-libedit.git (fetch)
origin	https://github.com/SWI-Prolog/packages-libedit.git (push)
/opt/logicmoo_workspace/swipl-devel/packages/ssl
origin	https://github.com/SWI-Prolog/packages-ssl.git (fetch)
origin	https://github.com/SWI-Prolog/packages-ssl.git (push)
/opt/logicmoo_workspace/swipl-devel/packages/http
origin	https://github.com/SWI-Prolog/packages-http.git (fetch)
origin	https://github.com/SWI-Prolog/packages-http.git (push)
/opt/logicmoo_workspace/swipl-devel/packages/odbc
origin	https://github.com/SWI-Prolog/packages-odbc.git (fetch)
origin	https://github.com/SWI-Prolog/packages-odbc.git (push)
/opt/logicmoo_workspace/swipl-devel/packages/xpce
origin	https://github.com/SWI-Prolog/packages-xpce.git (fetch)
origin	https://github.com/SWI-Prolog/packages-xpce.git (push)
/opt/logicmoo_workspace/swipl-devel/packages/table
origin	https://github.com/SWI-Prolog/packages-table.git (fetch)
origin	https://github.com/SWI-Prolog/packages-table.git (push)
/opt/logicmoo_workspace/swipl-devel/packages/chr
origin	https://github.com/SWI-Prolog/packages-chr.git (fetch)
origin	https://github.com/SWI-Prolog/packages-chr.git (push)
/opt/logicmoo_workspace/swipl-devel
origin	https://github.com/SWI-Prolog/swipl-devel.git (fetch)
origin	https://github.com/SWI-Prolog/swipl-devel.git (push)
/opt/logicmoo_workspace/swipl-devel/bench
origin	https://github.com/SWI-Prolog/bench.git (fetch)
origin	https://github.com/SWI-Prolog/bench.git (push)
/opt/logicmoo_workspace/swipl-devel/debian
origin	https://github.com/SWI-Prolog/distro-debian.git (fetch)
origin	https://github.com/SWI-Prolog/distro-debian.git (push)
