# Issues with library(pcre)

The library(pcre) is based on [libpcre](http://www.pcre.org/), providing
Perl compatible regular expressions.

## Consequences

The library(pcre) is currently not used by SWI-Prolog's own libraries
and consequences are thus limited to not having direct access to regular
expressions.  This is likely to change over time.

## Solutions

Install the development package for libpcre3.  This is available on
many platforms.

  $ Debian/Ubuntu :
  =|$ apt-get install libpcre3-dev|=

  $ Red Hat/Fedora :
  =|$ dnf install pcre-devel|=

After installing the development library, [re-install the
system](<RebuildAfterDevLib.html>)
