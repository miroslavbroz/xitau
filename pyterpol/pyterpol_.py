#!/usr/bin/env python3

"""
pyterpol_.py
Generate synthetic spectra with Pyterpol (Nemravova et al. 2016).

"""

__author__ = "Miroslav Broz (miroslav.broz@email.cz)"
__version__ = "Jun 23rd 2016"

import os
import sys
import pyterpolmini

dir_ = os.path.dirname(os.path.realpath(__file__))

# read from stdin
lines = sys.stdin.readlines()

# parse parameters
nbod = int(lines[0])
name = lines[1].split()
T_eff = [ float(l) for l in lines[2].split() ]
log_g = [ float(l) for l in lines[3].split() ]
v_rot = [ float(l) for l in lines[4].split() ]
metal = [ float(l) for l in lines[5].split() ]
lambda_ = [ float(l) for l in lines[6].split() ]
absolute = lines[7].split()[0]
tmpdir = lines[8].split()[0]

if absolute == "T":
    absolute = True
else:
    absolute = False

# prepare grid
if absolute:
    flux_type = "absolute"
    extension = ".abs"
    step = 0.1
else:
    flux_type = "relative"
    extension = ".syn"
    step = 0.01
    
sg = pyterpolmini.SyntheticGrid(flux_type=flux_type, debug=False)

#sg.default_grid_order = ['BSTAR', 'OSTAR', 'AMBRE', 'POLLUX']

# Note: POLLUX still has 1.0 step, which is NOT sufficient.
#sg.ABS_default_grid_order = ['BSTAR', 'OSTAR', 'PHOENIX', 'POLLUX']
sg.ABS_default_grid_order = ['BSTAR', 'OSTAR', 'PHOENIX']

# user-defined grids
#pyterpolmini.grid_directory = os.path.join(dir_, "kurucz")
#pyterpolmini.grid_dict = dict(
#    identification=[
#        'atlas',
#        ],
#    directories=[
#        ['ATLAS_Z_0.5', 'ATLAS_Z_1.0', 'ATLAS_Z_2.0'],
#        ],
#    families=[
#        ['atlas', 'atlas', 'atlas'],
#        ],
#    columns=[
#        ['filename', 'teff', 'logg', 'z'],
#        ['filename', 'teff', 'logg', 'z'],
#        ['filename', 'teff', 'logg', 'z'],
#        ],
#    )
#
#pyterpolmini.grid_directory_ABS = os.path.join(dir_, "kurucz_ABS")
#pyterpolmini.grid_dict_ABS = pyterpolmini.grid_dict
#
#sg = pyterpolmini.SyntheticGrid(mode='atlas', flux_type=flux_type, debug=False)

# interpolate spectra
for j in range(0, nbod):
    comp = dict(teff=T_eff[j], logg=log_g[j], z=metal[j])
    vrot = v_rot[j]
    filename = tmpdir + name[j] + extension
    print(str(filename) + ": " + str(comp) + " vrot=" + str(vrot))

    c = sg.get_synthetic_spectrum(comp, lambda_, order=4, step=step, padding=0.0)
    c.process_spectrum(vrot=vrot, only_class=True)
    c.write_spectrum(f=filename)


