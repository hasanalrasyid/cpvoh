import json
import re
import os
from fnmatch import fnmatch
from pymatgen.io.cif import CifParser, CifWriter
from pymatgen.io.vasp import Poscar

def read_structure(filename, primitive = True, sort = False):
    parser = CifParser(filename)
    s = parser.get_structures(primitive=primitive)[0]
    p = Poscar(s)
    p.write_file('POSCAR')

st = read_structure('kadal.cif')
