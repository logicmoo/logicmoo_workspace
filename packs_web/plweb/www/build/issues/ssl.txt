# Issues with library(ssl)

The library(ssl) provides stream wrappers to use SSL.  The implementation
is based on [OpenSSL](https://www.openssl.org/).

## Consequences

The ssl library is used by the HTTP server and client libraries to provide
HTTPS services and connect to them.  The add-on installer uses this library
if the package is hosted on an https server.

## Solutions

The library(ssl) should be provided and functional with all binary
distributions.  When installing from source, building the ssl package
is disabled if the OpenSSL libraries or headers could not be found.
