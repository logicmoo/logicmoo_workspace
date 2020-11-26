# User profile database

This library deals with  maintaining   user  profiles  and session-based
cookies to support web servics.  The overall picture is that we have

  1. Libraries to establish the user identity using mostly HTTP
     authentication or _federated_ authentication using e.g., *oauth*.
  2. This library to maintain information about a user.
  3. An _authorization_ library that reasons about the user profile
     to decide wether an identified user has access to a particular
     resource.

The  profile  library  provides  a  stable    API  between  the  various
components. The main library  (user_profile)   is  merely  a dispatching
library that connects to  a  backend   implementation.  This  allows for
backends ranging from a simple backed  up Prolog database to distributed
databases.

## Status

Work in progress.  Currently developed in the context of SWISH.
