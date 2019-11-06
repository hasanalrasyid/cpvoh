#!/usr/bin/env python3

import sys

from pymatgen.io.cif import CifParser, CifWriter
from pymatgen.io.vasp import Poscar

def read_structure(filename, primitive = True, sort = False):
    parser = CifParser(filename)
    s = parser.get_structures(primitive=primitive)[0]
    p = Poscar(s)
    print (p.get_string())

cifFile = sys.argv[1]
st = read_structure(cifFile)
