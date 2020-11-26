#!/bin/sh

install -m644 ../flashpolicy.xml /usr/local/etc/

install -m755 in.flashpolicyd.pl /usr/local/sbin/
install -m644 flashpolicyd.xinet /etc/xinetd.d/flashpolicyd
/etc/rc.d/init.d/xinetd reload

