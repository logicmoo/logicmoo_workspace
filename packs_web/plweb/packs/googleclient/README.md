# SWI-Prolog code for "Login with Google"

This pack deals with "Login With   Google"  using [OpenID Connect (OAuth
2.0                                                                  for
Login)](https://developers.google.com/accounts/docs/OpenIDConnect).  The
code is partially based on
[pl-oauth2client-for-google](https://github.com/xpxaxsxi/pl-oauth2client-for-google)

To run the code

  1. Follow these
  [steps](https://developers.google.com/accounts/docs/OpenIDConnect) to
  create a Google project and get

     - A client ID
     - A client secret
     - Register a redirect url.  To test from localhost, this should be
       `http://localhost:3040/oauth2/auth_redirect`

  2. Edit `demo/test.pl`, fill in the client id and client secret. Start
  Prolog with `demo/test.pl` and run the command below to start the
  server.

     ```{prolog}
     ?- server.
     ```

  3. Go to `http://localhost:3040` and hit the button.  After logging in
  this should show a Prolog dict holding the Google profile information.

# Requirements

Requires SWI-Prolog 7.1.30 or later (to   be  released, use version from
[GIT](https://github.com/SWI-Prolog/swipl-devel) or [nightly  builds for
Windows](http://www.swi-prolog.org/download/daily/bin/)


