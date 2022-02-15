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


