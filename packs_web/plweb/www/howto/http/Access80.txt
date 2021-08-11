---+ Making the server accessible at port 80

Typically, you want to deploy your webserver at the default HTTP port:
80.  There are several ways to achieve this.  Note that this situation
isn't unique to Prolog and you can find similar solutions for deploying
e.g., _Tomcat_ servers on the web.


---++ If there is no other web-server running on the machine

This used to be rare, but given today's good and cheap virtualization it
becomes a real option.  The library(http/http_unix_daemon) provides the
infrastructure, including a =|/etc/init.d|= script for Debian for starting
a server using root privileges, opening a reserved port, registering the
service in =|/var/run|=, talking to the Unix syslog daemon and finally
dropping privileges to a given user and group.

---++ Using Apache

Assuming you have Apache running on the server, the simplest way is to
use Apache's _Proxy_ facilities.  There are two options: with virtual
host and without.

---+++ Using a virtual host

For this, you need to bind the target hostname to the intended server,
which requires control over the DNS services of your domain.  Once you
have that, add a host configuration file that looks like the one we
use for http://www.swi-prolog.org

==
<VirtualHost *:80 >
ServerName www.swi-prolog.org
ErrorLog logs/prolog-error_log
TransferLog logs/prolog-access_log

<Location "/">
Order allow,deny
Allow from all
</Location>

ProxyPass / http://localhost:3040/ retry=0
</VirtualHost>
==

Note that =localhost= is used if Apache and the Prolog server run on
the same host.  You can easily use apache as proxy for Prolog servers
running on other hosts by entering the appropriate hostname.


---+++ Without a virtual host

If you want Prolog to serve only part of a website, you have to
organize the server such that all the locations that it serves have a
common root.  There are two options for that.  The setting http:prefix
rebases the entire server to serve addresses below the given root.  How
you manage this setting depends on the setup.  One way is to call

    ==
    :- set_setting_default(http:prefix, '/myserver').
    ==

Alternatively, if you are using library(http/http_path), you can define
the location of a path-alias that is the root of your server.

Once all functionality is available under, say, =|/myserver|=, you
can proxy this hierarchy using this rule in an Apache config file:

    ==
    ProxyPass /myserver/ http://localhost:3040/myserver/ retry=0
    ==

@see [Can I replace a LAMP stack with SWI-Prolog?](</FAQ/PrologLAMP.txt>)
