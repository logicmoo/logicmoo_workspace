# Using a multicast UDP network

Although the `broadcast` networks are the default.
[multicast](https://en.wikipedia.org/wiki/Multicast) networks have the
advantage that you do not need to know your IP address and, with router
support, they can be used on a WAN. Therefore simplest way to initialise
the network to make two or more processes communicate on a LAN is using
a `multicast` network:

```
?- udp_broadcast_initialize(ip(239,0,0,2), [method(multicast)]).
?- listen(write(X), writeln(X)).
?- broadcast(udp(subnet, write(hello))).
hello
```

@see [IPv4 Multicast Address Space
Registry](https://www.iana.org/assignments/multicast-addresses/multicast-addresses.xhtml).
The range 239.0.0.0 - 239.255.255.255 is reserved as _Organization-Local
Scope_.
