# SMTP pack for SWI-Prolog

Provides sending mail using the (E)SMTP protocol. This library currently
supports good old SMTP, encrypted and authorized ESMTP. Both SSL/TLS and
STARTTLS encryption is  supported.  Authorization   is  supported  using
PLAIN and LOGIN methods.

Installation (from within SWI-Prolog):

  ```
  ?- pack_install(smtp).
  ```
