#!/usr/bin/env python

from astropy.io import fits
hdu = fits.open('alpha_lyr_stis_002.fits')

hdu.info()
print("")
print(repr(hdu[0].header))

print("")
print(repr(hdu[1].header))

koef = 1.e-7 * 1.e4 * 1.e10  # erg s^-1 cm^-2 A^-1 -> J s^-1 m^-2 m^-1

f = open("alpha_lyr_stis_002.dat", "w")
f.write("# wavelength [A] & flux [J s^-1 m^-2 m^-1] & staterror [dtto] & syserror [dtto]\n")

for data in hdu[1].data:
    wavelength, flux, staterror, syserror, fwhm, dataqual, totexp = data
    
    f.write("%12.4f  %14.8e  %14.8e  %14.8e\n" % (wavelength, koef*flux, koef*staterror, koef*syserror))

f.close()

