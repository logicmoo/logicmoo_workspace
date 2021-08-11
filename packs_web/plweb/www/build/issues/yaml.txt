# Issues with library(yaml)

## Consequences

The library(yaml) provides parsing and generating
[YAML](http://yaml.org/). SWI-Prolog has no dependencies on this
library, so if you do not intend to use this library there is no reason
to take action.

## Solutions

Building the YAML library requires [libyaml](https://github.com/yaml/libyaml)
libraries and headers.  Packages:

  - Debian based Linux systems: =|sudo apt-get install libyaml-dev|=
  - Windows: see =README.mingw=
  - MacOS using Macports: =|sudo port install libyaml|=
