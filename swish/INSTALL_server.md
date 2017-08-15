# How to install LPS as a SWISH cloud server #
The following applies to Linux Ubuntu, and is a collage of specific instructions for each part for convenience as of March 2017; refer to the original URLs below for more details and updates.

A less Unix-challenged soul (than yours truly) should have no difficulty adapting this into a single Shell script or dockerfile.
## Setup Ubuntu Virtual machine ##
Pls see instructions from your cloud provider.

* E.g. AWS, Ubuntu Server 16.04 LTS, t2.micro 8 Gb SSD
* define and attach IAM role to instance, allowing full cloudWatch access + AmazonEC2FullAccess (probably overkill!)
* Security group with open ssh and http ports
* Prepare commands ssh and sftp commands

## SWI Prolog with pre requisites ##
Please refer to [SWI Prolog's site](http://www.swi-prolog.org/build/unix.html)

```
cd ~
sudo apt-get update
sudo apt-get upgrade
sudo apt-get install unzip
sudo apt-get install \
        build-essential autoconf curl chrpath pkg-config \
        ncurses-dev libreadline-dev libunwind-dev \
        libgmp-dev \
        libssl-dev \
        unixodbc-dev \
        zlib1g-dev libarchive-dev \
        libossp-uuid-dev \
        libxext-dev libice-dev libjpeg-dev libxinerama-dev libxft-dev \
        libxpm-dev libxt-dev \
        libdb-dev \
        openjdk-8-jdk junit
# Optional (doc), consider avoiding as it requires about 3Gb:
sudo apt-get install \
        texlive-latex-extra \
        texlive-fonts-extra \
        texlive-fonts-extra-doc \
        texlive-fonts-recommended \
        texlive-fonts-recommended-doc
git clone https://github.com/SWI-Prolog/swipl-devel.git
cd swipl-devel
./prepare
(confirm with yes) 
./configure
make
sudo make install
cd packages
./configure
make
sudo make install

```

## SWISH with pre requisites ##
Please refer to the [SWISH README](https://github.com/SWI-Prolog/swish/blob/master/README.md) file.

```
cd ~
sudo apt-get install nodejs
sudo apt install nodejs-legacy
sudo apt install npm
sudo npm install -g bower

cd ~
git clone https://github.com/SWI-Prolog/swish.git

cd swish
git submodule update --init
bower install
#as of now, this seems to be missing from the previous step and must be performed too:
#bower install jquery-ui

make src
```
Ideally minify swish referring to its documentation, or at least comment line swish/web/js/swish.js:47:
```
urlArgs: "ts="+new Date().getTime(),	/* prevent caching during development */
```
## LPS site ##
```
cd ~
git clone https://bitbucket.org/lpsmasters/lps_corner.git
cd lps_corner/swish
mkdir data
chmod a+w data    # ...or something finer

cd ~/lps_corner/swish/web/lps
./INSTALL.sh  # install bower components
# You might want to TEST now:
# cd ~/lps_corner/swish
# sudo swipl user_module_file.pl ../../swish/daemon.pl --port=80 --no-fork --user=www-data --workers=16
# ... and open a browser upon your virtual machine address

# Google Analytics (optional):
# with sftp, put googleAnalyticsKey file (containing only the key) into lps_corner/swish

# Setup operation as service (Using upstart)
sudo apt install upstart
sudo reboot
sudo apt-get install upstart-sysv
sudo reboot
cd /etc/init
sudo ln -s ~/lps_corner/swish/upstart/lps.conf .

sudo start lps
# then use 
# sudo stop lps
# sudo restart lps
```
For HTTP (SWI/SWISH) logging: give write permission to www-data in ~/swish 

### Variant for systemd (instead of Upstart) ###
Replace the final sequence above with the following:

```
# Setup operation as service (Using systemd, e.g. Ubuntu 17.0)
cd /lib/systemd/system
sudo ln -s ~/lps_corner/swish/systemd/lps.service .
sudo systemctl enable lps.service
sudo systemctl daemon-reload
sudo systemctl restart lps.service

# then use
# sudo systemctl stop lps.service
# systemctl status lps.service

```

## Site alarms with Amazon Cloud Watch ##
At https://console.aws.amazon.com/cloudwatch:

* setup CPU utilization alarm
* attach IAM role to instance, allowing full cloudWatch access + AmazonEC2FullAccess (probably overkill!)

For disk space and RAM monitoring (cf. [AWS instructions](http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/mon-scripts.html)): 

```
sudo apt-get install libwww-perl libdatetime-perl
curl http://aws-cloudwatch.s3.amazonaws.com/downloads/CloudWatchMonitoringScripts-1.2.1.zip -O
unzip CloudWatchMonitoringScripts-1.2.1.zip
rm CloudWatchMonitoringScripts-1.2.1.zip
cd aws-scripts-mon
crontab -e
# add this and save:
*/5 * * * * ~/aws-scripts-mon/mon-put-instance-data.pl --mem-util --disk-space-util --disk-path=/ --from-cron
```
Then define alarms and email notifications at the Cloud Watch console.

