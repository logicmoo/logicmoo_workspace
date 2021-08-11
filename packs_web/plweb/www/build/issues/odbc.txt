# Issues with library(odbc)

The library(odbc) can be used to connect to relational databases, such as MySQL,
Postgres, Microsoft SQL server, etc.

## Consequences

The library is not used by any of the other system components and is thus
only necessary if your application requires it.

## Solutions

When building from source, the library requires the *odbc* or *iodbc* library
as well as the *sql.h* header.

### MacOS binary

The MacOSX binary package does *not* contain library(odbc).  Mavericks does provide
iodbc.dylib, but the development system does not provide sql.h.  Please contact
bugs@swi-prolog.org if you know a solution to this problem.
