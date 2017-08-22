#!/usr/bin/env python
from parser import Parser
import sys

p = Parser()

if len(sys.argv) == 0:
    print "usage:  pair.py on | off"
    sys.exit(2)
elif sys.argv[1] == 'on':
    # connect
    p.write_serial("\xc2")
elif sys.argv[1] == 'off':
    # disconnect
    p.write_serial("\xc1")
else:
    print "Error"
    sys.exit(2)
