# Running the server from Linux upstart

[Upstart](http://upstart.ubuntu.com/) is Ubuntu's default service
manager. One of the goodies it provides is the possibility to restart
the server should it crash. Adding a HTTP service that is started using
library(http/http_unix_daemon) is quite simple. Here is some example
code, which assumes a file `daemon.pl` that starts the server. The
server is started on port 80, which is a good setup if you start the
server in a virtualized environment such as [Linux
containers](https://linuxcontainers.org/).

  ==
  # demo - SWI-Prolog demo server
  #
  # The SWI-Prologdemo server

  description	"SWI-Prolog demo server"

  start on runlevel [2345]
  stop on runlevel [!2345]

  respawn
  respawn limit 5 60
  umask 022

  console log
  chdir /home/swipl/src/demo

  script
    export LANG=en_US.utf8
    ./daemon.pl --no-fork --port=80 --user=www-data --pidfile=/var/run/swipl.pid --workers=16 --syslog=swipl
  end script
  ==


