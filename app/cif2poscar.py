#!/usr/bin/env python3

import sys

from pymatgen.io.cif import CifParser, CifWriter
from pymatgen.io.vasp import Poscar

def read_structure(filename, primitive = True, direct = True, sort = False):
    parser = CifParser(filename)
    s = parser.get_structures(primitive=primitive)[0]
    p = Poscar(s)
    print (p.get_string(direct=direct))

# howto call:
# cif2poscar.py filename.cif p         d
#                            primitive direct
cifFile = sys.argv[1]
primitive = sys.argv[2] == 'p'
direct = sys.argv[3] == 'd'
st = read_structure(cifFile,primitive,direct)
