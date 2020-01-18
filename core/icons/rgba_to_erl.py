#!/usr/bin/python3

import sys

with open(sys.argv[1], "rb") as f:
    print('{icon_name}() ->'.format(icon_name=sys.argv[2]))
    print('  {{ {w},{h}, <<'.format(w=sys.argv[3],h=sys.argv[4]))

    byte = f.read(4)
    r = format(byte[0], '02x')
    g = format(byte[1], '02x')
    b = format(byte[2], '02x')
    a = format(byte[3], '02x')
    print('      16#{red}, 16#{green}, 16#{blue}, 16#{alpha}'.format(red=r, green=g, blue=b, alpha=a))
    while byte:
        r = format(byte[0], '02x')
        g = format(byte[1], '02x')
        b = format(byte[2], '02x')
        a = format(byte[3], '02x')
        print('    , 16#{red}, 16#{green}, 16#{blue}, 16#{alpha}'.format(red=r, green=g, blue=b, alpha=a))
        byte = f.read(4)

    print('>> }.')
