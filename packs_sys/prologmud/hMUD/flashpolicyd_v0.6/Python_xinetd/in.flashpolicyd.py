#!/usr/bin/env python
#
# in.flashpolicyd.py
# Simple socket policy file server for Flash
#
# Usage: in.flashpolicyd.py --file=FILE
#
# Logs to stderr
# Requires Python 2.5 or later

from __future__ import with_statement

import sys
import optparse
import exceptions
import syslog

VERSION = 0.1

class policy_server(object):
    def __init__(self, path):
        self.path = path
        self.policy = self.read_policy(path)
    def read_policy(self, path):
        with file(path, 'rb') as f:
            policy = f.read(10001)
            if len(policy) > 10000:
                raise exceptions.RuntimeError('File probably too large to be a policy file',
                                              path)
            if 'cross-domain-policy' not in policy:
                raise exceptions.RuntimeError('Not a valid policy file',
                                              path)
            return policy
    def run(self):
        request = sys.stdin.readline().strip()
        if request != '<policy-file-request/>\0':
            self.log('Unrecognized request %s' % (request,))
            return
        self.log('Valid request received')
        sys.stdout.write(self.policy)
        self.log('Sent policy file')
    def log(self, str):
        syslog.syslog(str)

def main():
    parser = optparse.OptionParser(usage = '%prog --file=FILE',
                                   version='%prog ' + str(VERSION))
    parser.add_option('-f', '--file', dest='path',
                      help='server policy file FILE', metavar='FILE')
    opts, args = parser.parse_args()
    if args:
        parser.error('No arguments are needed. See help.')
    if not opts.path:
        parser.error('File must be specified. See help.')

    policy_server(opts.path).run()

if __name__ == '__main__':
    try:
        main()
    except Exception, e:
        syslog.syslog("".join(e))
        sys.exit(1)
