# Issues with library(jpl)

## Consequences

The library(jpl) is used for calling Java from Prolog or embedding Prolog in a
Java program.  SWI-Prolog has no dependencies on this library, so if you do not
intend to mix Java and Prolog in one program there is no reason to take action.

## Solutions

### MacOS

The MacOS version has been tested with Oracle Java. The interface
object, =libjpl.dylib= needs to have its dependencies pointed at the
currently installed Oracle Java version. The dependencies can be updated
by running

    ?- jpl_config_dylib.

This will

  1. Find Java from either =|$JAVA_HOME|= or by running
  =|/usr/libexec/java_home|=
  2. Find =|libjpl.dylib|= in the SWI-Prolog library
  3. Find its dependencies and locate them in the Java home dir
  4. Use =|/usr/bin/install_name_tool|= to update the dependency
