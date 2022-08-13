#!/usr/bin/env python3

"""
pyterpol_.py
Generate synthetic spectra with Pyterpol (Nemravova et al. 2016).

"""

__author__ = "Miroslav Broz (miroslav.broz@email.cz)"
__version__ = "Jun 23rd 2016"

import pyterpol3

# read input file
f = open("pyterpol_.in", "r")
lines = f.readlines()
f.close()

# parse parameters
nbod = int(lines[0])
name = lines[1].split()
T_eff = [ float(l) for l in lines[2].split() ]
log_g = [ float(l) for l in lines[3].split() ]
v_rot = [ float(l) for l in lines[4].split() ]
metal = [ float(l) for l in lines[5].split() ]
lambda_ = [ float(l) for l in lines[6].split() ]
absolute = lines[7].split()[0]

if absolute == "T":
    absolute = True
else:
    absolute = False

# prepare grid
if absolute:
    flux_type = "absolute"
    extension = ".abs"
    step = 1.0
else:
    flux_type = "relative"
    extension = ".syn"
    step = 0.01
    
sg = pyterpol3.SyntheticGrid(flux_type=flux_type, debug=False)

#sg.default_grid_order = ['BSTAR', 'OSTAR', 'AMBRE', 'POLLUX']
#sg.ABS_default_grid_order = ['BSTAR', 'OSTAR', 'PHOENIX', 'POLLUX']
sg.ABS_default_grid_order = ['BSTAR', 'OSTAR', 'PHOENIX']

# interpolate spectra
for j in range(0, nbod):
    comp = dict(teff=T_eff[j], logg=log_g[j], z=metal[j])
    vrot = v_rot[j]
    filename = name[j] + extension
    print(str(filename) + ": " + str(comp) + " vrot=" + str(vrot))

    c = sg.get_synthetic_spectrum(comp, lambda_, order=4, step=step, padding=0.0)
    c.get_spectrum(vrot=vrot, keep=True)
    c.write_spectrum(filename=filename)


