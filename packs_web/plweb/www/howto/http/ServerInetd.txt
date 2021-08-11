---+ Running the server from Unix (x)inetd

If you are using a Unix system,  one   of  the options for deploying the
SWI-Prolog based server is to use  (x)inetd.   This  starts a server for
every client. The server will try   to use Keep-Alive connections, which
can greatly enhance the performance  if   multiple  requests  need to be
made. If Keep-Alive  is  not  used,  this   is  very  similar  to  using
SWI-Prolog as a GCI script. See library(cgi) for details.

If you have applications that take long to  load, you want to do session
management, etc., we advice to use the multi-threaded server.


---++ To set it all up

 1. Create a server directory:  In this example,  $PLBASE
    is the installation directory of SWI-Prolog.  This
    variable is set using the first command.

    ==
    % eval `swipl --dump-runtime-variables`
    % sudo mkdir -p /srv/www/html/srv_demo
    % cd !$
    % sudo cp $PLBASE/doc/packages/examples/http/demo_body.pl .
    % sudo cp $PLBASE/doc/packages/examples/http/demo_inetd .
    ==

 2. Create an (x)inetd entry

    * If you are using *xinetd*:

      ==
      % sudo tee /etc/xinetd.d/demo_swi_srv << EOF
      service demo_swi_srv
      {
      port                    = 8080
      socket_type             = stream
      protocol                = tcp
      wait                    = no
      user                    = nobody
      server                  = /srv/www/html/srv_demo/demo_inetd
      log_on_failure  += USERID
      log_on_success  += PID HOST EXIT
      }
      EOF
      ==

    * If you are using *inetd*:

      ==
      % sudo tee -a /etc/inetd.conf << EOF
      demo_swi_srv stream tcp nowait nobody /srv/www/html/srv_demo/demo_inetd
      EOF
      ==

 3. Add association to the services file. This can be done by adding the
    following line to =|/etc/services|=

    ==
    demo_swi_srv   8080/tcp
    ==

 4. Restart (x)inetd

    ==
    % sudo /etc/init.d/xinetd reload
    ==

    or

    ==
    % sudo /etc/init.d/inetd reload
    ==

 5. Point your browser to http://localhost:8080 and try

    - http://localhost:8080/upload
    - http://localhost:8080/foreign
    - http://localhost:8080/error

@author Nicos Angelopoulos
