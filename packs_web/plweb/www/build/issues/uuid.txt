# Issues with library(uuid)

## Consequences

The library(uuid) is used to generate unique identifiers. It is used by
various of the HTTP and RDF support libraries.

## Solutions

The UUID library depends on [OSSP
uuid](http://www.ossp.org/pkg/lib/uuid/). Be careful here, as there are
many UUID libraries around, even with similar names and interfaces, but
all just a bit too different from each other to allow for the wrong one.

@see [Fix incomplete installation due to missing dependencies](<RebuildAfterDevLib.txt>)
