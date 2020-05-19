#!/usr/bin/python

from binascii import unhexlify

with open('fuck.txt', 'r') as f:
    snips = [ unhexlify(l.split("[")[1].split("]")[0].replace(" ", "")) for l in f.readlines() if l.startswith("    code:") ]

for (idx, snip) in enumerate(snips):
    with open('fuck_%4.4x.out' % idx, 'wb') as f:
        f.write(snip)
