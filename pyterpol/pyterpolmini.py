#!/usr/bin/env python3

"""
pyterpolmini.py
A 'miniaturized' version of Pyterpol3.

Reference: Nemravová et al. (2016, A&A 594, A55).
Reference: Brož (2017, ApJS 230, 19).

"""

import os
import copy
import numpy as np

from astropy.constants import c
from scipy.interpolate import splrep, splev, RectBivariateSpline
from scipy.signal import fftconvolve

# from defaults.py

curdir = os.getcwd()

grid_directory = os.path.join("/".join(curdir.split('/')[:]), 'grids')
grid_directory_ABS = os.path.join("/".join(curdir.split('/')[:]), 'grids_ABS')

grid_dict = dict(
    identification=[
        'default',
        'ostar',
        'bstar',
        'pollux',
        'ambre',
        'powr',
        ],
    directories=[
        ['OSTAR_Z_0.5', 'OSTAR_Z_1.0', 'OSTAR_Z_2.0', 'BSTAR_Z_0.5', 'BSTAR_Z_1.0', 'BSTAR_Z_2.0', 'POLLUX_Z_1.0', 'AMBRE_Z_1.0'],
        ['OSTAR_Z_0.5', 'OSTAR_Z_1.0', 'OSTAR_Z_2.0'],
        ['BSTAR_Z_0.5', 'BSTAR_Z_1.0', 'BSTAR_Z_2.0'],
        ['POLLUX_Z_1.0'],
        ['AMBRE_Z_1.0'],
        ['POWR_Z_1.0'],
        ],
    families=[
        ['ostar', 'ostar', 'ostar', 'bstar', 'bstar', 'bstar', 'pollux', 'ambre'],
        ['ostar', 'ostar', 'ostar'],
        ['bstar', 'bstar', 'bstar'],
        ['pollux'],
        ['ambre'],
        ['powr'],
        ],
    columns=[
        ['filename', 'teff', 'logg', 'z'],
        ['filename', 'teff', 'logg', 'z'],
        ['filename', 'teff', 'logg', 'z'],
        ['filename', 'teff', 'logg', 'z'],
        ['filename', 'teff', 'logg', 'z'],
        ['filename', 'teff', 'logg', 'z'],
        ],
)

# Note: Pollux has a too narrow wavelength range => deleted from default!

grid_dict_ABS = dict(
    identification=[
        'default',
        'pollux',
        'ostar',
        'bstar',
        'phoenix',
        ],
    directories=[
        ['OSTAR_Z_1.0', 'BSTAR_Z_1.0', 'PHOENIX_Z_1.0'],
        ['POLLUX_Z_1.0'],
        ['OSTAR_Z_1.0'],
        ['BSTAR_Z_1.0'],
        ['PHOENIX_Z_1.0'],
        ],
    families=[
        ['ostar', 'bstar', 'phoenix'],
        ['pollux'],
        ['ostar'],
        ['bstar'],
        ['phoenix'],
        ],
    columns=[
        ['filename', 'teff', 'logg', 'z'],
        ['filename', 'teff', 'logg', 'z'],
        ['filename', 'teff', 'logg', 'z'],
        ['filename', 'teff', 'logg', 'z'],
        ['filename', 'teff', 'logg', 'z'],
        ],
)

default_grid_order = ['bstar', 'ostar', 'ambre', 'pollux']
default_grid_order_ABS = ['bstar', 'ostar', 'phoenix', 'pollux']

########################################################################

# from auxiliary.py

ZERO_TOLERANCE = 1.0e-6

def is_within_interval(v, arr):

    if (v-max(arr) > ZERO_TOLERANCE) | (min(arr)-v > ZERO_TOLERANCE):
        return False
    else:
        return True

def doppler_shift(wave, rv):

    return wave * (1.0 + rv*1.0e3/c.value)

def interpolate_spectrum(wave, intens, wave_):
    """
    Fast linear interpolation

    Note: In previous version, cubic splines were used:

    tck = splrep(wave, intens, k=3)
    intens_ = splev(wave_, tck) 

    """

    intens_ = np.interp(wave_, wave, intens)

    return intens_

def instrumental_broadening(wave, intens, width=0.25, type='fwhm'):
    """
    Instrumental broadening; a convolution with a normal distribution.

    :param wave: wavelengths
    :param intens: intensities
    :param width: width in A
    :param type: either 'fwhh', or 'sigma'
    :return intens: the broadened spectrum

    """
    if width < ZERO_TOLERANCE:
        return intens

    if type == 'fwhm':
        sigma = width/2.3548
    elif type == 'sigma':
        sigma = width
    else:
        raise ValueError(("Unrecognised type='{}'").format(type))

    # Make sure the wavelengths are equidistant
    delta = np.diff(wave).min()
    Delta = wave.ptp()
    n = int(Delta/delta) + 1
    wave_ = np.linspace(wave[0], wave[-1], n)

    intens_ = interpolate_spectrum(wave, intens, wave_)

    # Construct the kernel!
    delta = wave_[1]-wave_[0]
    n_kernel = int(2*4*sigma/delta)

    if n_kernel == 0:
        raise ValueError(("Spectrum resolution too low for instrumental broadening (delta={}, width={}").format(delta, width))

    if n_kernel > n:
        raise ValueError(("Spectrum range too low for instrumental broadening"))

    wave_k = np.arange(n_kernel)*delta
    wave_k -= wave_k[-1]/2.0
    kernel = np.exp(-(wave_k)**2/(2.0*sigma**2))
    kernel /= sum(kernel)

    # Convolve the flux!
    intens_conv = fftconvolve(1.0-intens_, kernel, mode='same')

    if n_kernel%2 == 1:
        offset = 0.0
    else:
        offset = dwave/2.0

    intens = np.interp(wave+offset, wave_, 1.0-intens_conv, left=1, right=1)
    return intens

def rotational_broadening(wave, intens, vrot, epsilon=0.6):
    """
    Rotational broadening.

    :param wave: wavelengths
    :param intens: intensities
    :param vrot: projected rotational velocity in km/s
    :param epsilon: coefficient of linear limb-darkening
    :return intens: the rotated spectrum

    """
    if vrot < ZERO_TOLERANCE:
        return intens

    # Make sure the RVs are equidistant
    wave_log = np.log(wave)
    rv = np.linspace(wave_log[0], wave_log[-1], len(wave))
    step = rv[1] - rv[0]

    intens_rv = interpolate_spectrum(wave_log, intens, rv)

    vrot *= 1.0e3/c.value

    # Construct the kernel!
    n = int(np.ceil(2.0*vrot/step))
    rv_ker = np.arange(n)*step
    rv_ker = rv_ker - rv_ker[-1]/2.0
    y = 1.0 - (rv_ker/vrot)**2

    kernel = (2.0*(1.0-epsilon)*np.sqrt(y) + np.pi*epsilon/2.0*y) / (np.pi*vrot*(1.0-epsilon/3.0))
    kernel /= kernel.sum()

    # Convolve the flux!
    intens_conv = fftconvolve(1 - intens_rv, kernel, mode='same')

    if n % 2 == 1:
        rv = np.arange(len(intens_conv))*step + rv[0]
    else:
        rv = np.arange(len(intens_conv))*step + rv[0] - step/2.0

    wave_conv = np.exp(rv)

    intens = interpolate_spectrum(wave_conv, 1.0-intens_conv, wave)
    return intens

def interpolate_block_fast(x, block, xnew):
    """
    Interpolation of a *block* of spectra; a fast version.

    :param x: parameters (teff, logg, z)
    :param block: a block of spectra
    :param xnew: new parameters, to interpolate at
    :return:
    """

    nx = len(block[0])
    ny = len(x)

    if ny > 3 and ny < 6:
        ky = 3
    elif ny > 5:
        ky = 5
    else:
        ky = ny-1

    f = RectBivariateSpline(x, np.arange(nx), block, kx=ky, ky=1)
    y = f(xnew, np.arange(nx))[0]

    return y

########################################################################

class SyntheticSpectrum:
    """A synthetic spectrum."""

    def __init__(self, f=None, wave=None, intens=None, do_not_load=False, **props):
        """
        Init a synthetic spectrum and its properties.

        :param f: filename
        :param wave: wavelengths
        :param intens: intensities
        :param do_not_load: Do Not Load
        :param **props: a dictionary of properties {'teff': teff, ...}

        """

        self.filename = f
        self.wave = wave
        self.intens = intens
        self.props = props
        self.loaded = False

        if f is not None and not do_not_load:
            self.wave, self.intens = np.loadtxt(filename, usecols=[0, 1], unpack=True)

        if self.intens is not None:
            self.loaded = True
            self.measure_spectrum()

    def __str__(self):
        """String representation."""

        s = ""
        if self.filename is not None:
            s += "filename:%s " % (self.filename)
            s += "loaded:%s " % (str(self.loaded))
        for key in self.props.keys():
            s += "%s:%s " % (key, str(self.props[key]))
        if self.loaded:
            s += "(wmin, wmax): (%s, %s)" % (str(self.wmin), str(self.wmax))
            s += '\n'
        return s

    def load_spectrum(self, f=None):
        """
        Loads a spectrum.

        :param f: filename

        """
        if f is not None:
            self.filename = f

        # check if a binary representation exists -- then load it
        binary_file = self.filename + '.npz'
        if os.path.isfile(binary_file):
            npz = np.load(binary_file, mmap_mode='r')
            self.wave = npz['arr_0']
            self.intens = npz['arr_1']

        # otherwise, load ascii (VERY SLOW!) and save it as binary
        else:
            self.wave, self.intens = np.loadtxt(self.filename, unpack=True, usecols=[0, 1])
            print(("Saving binary file: " + str(binary_file)))
            np.savez(binary_file, self.wave, self.intens)

        self.measure_spectrum()
        self.loaded = True

    def write_spectrum(self, f='spectrum.dat', fmt='%12.6f %12.8e'):
        """
        Writes a spectrum.

        :param f: filename
        :param fmt: format

        """
        header = str(self.props.keys())
        np.savetxt(f, np.column_stack([self.wave, self.intens]), fmt=fmt, header=header)

    def measure_spectrum(self):

        self.wmin = self.wave.min()
        self.wmax = self.wave.max()
        self.step = self.wave[1] - self.wave[0]

    def check_boundaries(self, wmin, wmax):

        if wmin is None:
            wmin = self.wmin
        if wmax is None:
            wmax = self.wmax
        if (wmin-(self.wmin-self.step) < ZERO_TOLERANCE) | (wmax-(self.wmax+self.step) > ZERO_TOLERANCE):
            return False
        else:
            return True

    def get_range(self):

        if not self.loaded:
            raise Exception('Spectrum has not been loaded.')
        self.measure_spectrum()
        return self.wmin, self.wmax, self.step

    def process_spectrum(self, wave=None, rv=None, vrot=None, lr=1.0, \
        wmin=None, wmax=None, fwhm=None, korel=False, only_class=False, only_intensity=False):
        """
        Process the sythetic spectrum stored within the class.

        - instrumental broadening
        - rotational broadening ... I.E., NOT USED IN PHOEBE!
        - Doppler shift
        - normalisation
        - interpolation

        :param wave: wavelengths
        :param rv: radial velocity in km/s
        :param vrot: projected rotational velocity in km/s
        :param korel: korel format
        :param only_class: only class
        :param only_intensity: only intensity
        :return wave: wavelengths
        :return intens: intensities

        """
        if wave is None:
            wave = self.wave

        w0min = wave.min()
        w0max = wave.max()
        mins = np.array([w0min, w0max])

        if rv is not None and abs(rv) > ZERO_TOLERANCE:
            bump = np.ceil(np.max(np.absolute(mins * (1.0 + 1000*rv/c.value) - mins)))
        else:
            bump = 0.0

        wmin = w0min - bump
        wmax = w0max + bump

        if not self.check_boundaries(wmin, wmax):
            raise ValueError('Extrapolation in process_spectrum() is FORBIDDEN!')

        syn_wave, intens = self.select_interval(wmin, wmax)

        # instrumental broadening
        if fwhm is not None and fwhm > ZERO_TOLERANCE:
            intens = instrumental_broadening(syn_wave, intens, width=fwhm)

        # rotational broadening
        if vrot is not None and vrot > ZERO_TOLERANCE:
            intens = rotational_broadening(syn_wave, intens, vrot)

        # Doppler shift
        if rv is not None and abs(rv) > ZERO_TOLERANCE:
            syn_wave = doppler_shift(syn_wave, rv)

        # normalisation
        if lr is not None and abs(lr-1.0) > ZERO_TOLERANCE:
            intens = lr*intens

        # user-specified wavelengths
        intens = interpolate_spectrum(syn_wave, intens, wave)

        # if we want to extract the spectra in KOREL format
        if korel:
            intens = 1.0 - (normal-intens)

        if only_class:
            self.intens = intens
            self.wave = wave
            self.measure_spectrum()

            for key, val in zip(['rv', 'vrot', 'lr', 'korel'], [rv, vrot, lr, korel]):
                self.props[key] = val
            return

        if only_intensity:
            return intens

        return wave, intens

    def select_interval(self, wmin, wmax):
        ind = np.where((self.wave >= wmin) & (self.wave <= wmax))[0]
        return self.wave[ind], self.intens[ind]

    def truncate_spectrum(self, wmin=None, wmax=None):
        """
        Truncates a spectrum.

        :param wmin: minimum wavelength
        :param wmax: maximum wavelength
        """
        if self.loaded is False:
            raise Exception('Spectrum has not been loaded.')
        if wmin is None:
            wmin = self.wave.min()
        if wmax is None:
            wmax = self.wave.max()
        if (self.wave.min() > wmin+ZERO_TOLERANCE) | (self.wave.max() < wmax-ZERO_TOLERANCE):
            raise ValueError('The spectrum %s does not cover the whole spectral region <%s,%s>.' % (str(self).rstrip('\n'), str(wmin), str(wmax)))

        ind = np.where((self.wave >= wmin-ZERO_TOLERANCE) & (self.wave <= wmax+ZERO_TOLERANCE))[0]
        self.wave = self.wave[ind]
        self.intens = self.intens[ind]

########################################################################

class SyntheticGrid:
    """A grid of synthetic spectra."""

    def __init__(self, mode='default', flux_type='relative', debug=False):
        """
        Setup the grid.

        :param mode:
        :param flux_type:

        """
        if flux_type=='relative':

            self.grid_dict = grid_dict
            self.grid_directory = grid_directory
            self.default_grid_order = default_grid_order

        elif flux_type=='absolute':

            self.grid_dict = grid_dict_ABS
            self.grid_directory = grid_directory_ABS
            self.default_grid_order = default_grid_order_ABS

        else:
            raise ValueError("Flux type '%s' not implemented.")

        self.SyntheticSpectraList = []

        self.parameterList = []
        self.columns = []
        self.grid_order = self.default_grid_order
        self.wave = None
        self.debug = debug

        self.set_mode(mode)

    def set_mode(self, mode):
        """
        Sets mode (a group of synthetic spectra).

        :param mode: default, ostar, bstar, pollux, ambre, powr, ...

        """
        i = self.grid_dict['identification'].index(mode)

        if i < 0:
            raise ValueError('Mode named %s not found.' % (mode))

        directories = self.grid_dict['directories'][i]
        families = self.grid_dict['families'][i]
        columns = self.grid_dict['columns'][i]

        # read all gridlists
        for i, d in enumerate(directories):
            gridlist = os.path.join(self.grid_directory, d, 'gridlist')
            directory = os.path.join(self.grid_directory, d)
            family = families[i]

            self.read_gridlist(gridlist, columns=columns, directory=directory, family=family)

    def __str__(self):
        """String representation."""

        s = "List of spectra:\n"
        for val in self.SyntheticSpectraList:
            s += str(val)
        return s

#    @profile
    def get_synthetic_spectrum(self, props, wave, order=2, step=0.01, padding=20.0):
        """
        Computes a synthetic (interpolated) spectrum.

        Cf. SyntheticSpectrum().

        :param props: a dictionary of properties, at which to interpolate
        :param wave: wavelengths
        :param order: order of interpolation: k = order-1 if order < 4, k = 3 otherwise
        :param step: step in A
        :param padding: padding in A

        """
        if isinstance(wave, (list, tuple)):
            wave = np.array(wave)

        wmin = int((wave.min() - padding)/step + 0.0)*step
        wmax = int((wave.max() + padding)/step + 1.0)*step
        self.set_wave(wmin, wmax, step)

        parList, vals, keys = self.verify_parameters(order=order, **props)
        spectra = self.for_interpolation(parList, keys, step=step, wmin=wmin, wmax=wmax)
        intens = self.interpolate_spectra(parList, spectra, vals)

        spectrum = SyntheticSpectrum(wave=self.wave.copy(), intens=intens, **props)

        return spectrum

    def set_wave(self, wmin, wmax, step):

        n = int((wmax-wmin)/step + 0.5) + 1
        self.wave = np.linspace(wmin, wmax, n)

    def read_gridlist(self, f, columns=None, directory=None, family=None):
        """
        Reads gridist.

        Note: For 1 family, there should NOT exist 2 spectra with same properties.

        :param f: filename
        :param columns: description of columns 
        :param directory: directory of spectra (if relative)
        :param family: family of spectra

        """

        with open(f, 'r') as tmp:
            lines = tmp.readlines()

        for j, line in enumerate(lines):
            if line[0:1] == '#':
                continue
            data = line.split()

            if len(data) > len(columns):
                raise KeyError('Description of some columns is missing!')

            rec = {'family': family}
            for i, col in enumerate(columns):
                if col in ['filename']:
                    rec[col] = os.path.join(directory, data[i])
                else:
                    rec[col] = float(data[i])

            filename = rec.pop('filename')

            spectrum = SyntheticSpectrum(f=filename, do_not_load=True, **rec)

            self.SyntheticSpectraList.append(spectrum)

            columns_ = [x for x in columns if x not in ['family', 'filename']]
            self.parameterList.append([rec[x] for x in columns_])
            self.columns = columns_

#    @profile
    def get_all(self, **props):
        """
        Returns all spectra having a certain property.

        :param props: a dictionary of properties

        """
        if len(list(props.keys())) == 0:
            return self.SyntheticSpectraList.copy()

        spectra = []
        for spectrum in self.SyntheticSpectraList:
            for i, key in enumerate(props.keys()):
                if (abs(props[key] - spectrum.props[key]) < ZERO_TOLERANCE):
                    if i == (len(list(props.keys()))-1):
                        spectra.append(spectrum)
                else:
                    break

        if len(spectra) == 0:
            warnings.warn("No eligible spectrum was found! Out of the grid?")
        return spectra

    def delete_exact(self, l, **props):
        """
        Deletes spectra where interpolating at an exact value.

        :param l: list of spectra
        :param props: a dictionary of properties
        :return: narrow-downed list

        """
        l = np.array(l)
        keys = list(props.keys())

        for i in range(0, np.shape(l)[-1]):
            v = props[keys[i]]
            if np.any(abs(np.unique(l[:,i]) - v) < ZERO_TOLERANCE):
                ind = np.where(abs(l[:,i] - v) < ZERO_TOLERANCE)
                l = l[ind]

        return l

    def get_available_values_fast(self, prop, **constraints):
        """
        Returns available values of a property.

        :param prop: a property
        :param constraints: a dictionary of constraints
        :return: sorted list

        """
        l = np.array(self.parameterList)

        for key in list(constraints.keys()):
            v = constraints[key]
            col = self.columns.index(key)
            ind = np.where(abs(l[:, col] - v) < ZERO_TOLERANCE)[0]
            l = l[ind]

        col = self.columns.index(prop)

        return sorted(set(l[:, col]))

#    @profile
    def for_interpolation(self, parList, header, step=0.01, wmin=None, wmax=None):
        """
        Returns spectra for interpolation.

        """
        spectra = []

        if wmin is None and wmax is None:
            truncate = False
        else:
            truncate = True

        for i, row in enumerate(parList):
            props = {prop: row[j] for j, prop in enumerate(header)}

            spectrum = self.get_all(**props)

            if len(spectrum) > 1:
                spectrum = self.resolve_degeneracy(spectrum)
            else:
                spectrum = spectrum[0]

            if not spectrum.loaded:
                if self.debug:
                    print("Loading spectrum: %s" % (str(spectrum).rstrip('\n')))
                else:
                    print("Loading spectrum: %s" % (str(spectrum).rstrip('\n')))

                spectrum.load_spectrum()

            else:
                if not spectrum.check_boundaries(wmin, wmax):
                    spectrum.load_spectrum()

            if truncate:
                spectrum.truncate_spectrum(wmin, wmax)

            # Note: It is crucial that all spectra have the same wavelength scale!

            swmin, swmax, sstep = spectrum.get_range()
            if np.any(np.abs([swmin-wmin, swmax-wmax, sstep-step]) > ZERO_TOLERANCE):
                if self.debug:
                    print("Spectrum %s does not have the wavelength scale (wmin, wmax, step)=(%s, %s, %s)" % (str(spectrum).rstrip('\n'), str(wmin), str(wmax), str(step)))

            # Note: Don't call spectrum.process_spectrum(), if it's not needed.
            intens = spectrum.intens

            spectra.append(intens)

        return spectra

    def interpolate_spectra(self, parList, spectra, properties):
        """
        Interpolates spectra in all parameters.

        :param parList: a list generated by select_parameters()
        :param spectra: a list generated by for_interpolation()
        :param properties: a list of properties, for interpolation
        :returns intens: intensities

        """

        # i.e., easier to handle
        plist = np.array(parList)
        syns = np.array(spectra)
        ncol = len(plist[0])

        while ncol > 0:
            xnew = properties[ncol-1]

            new_plist = []
            new_syns = []

            j = 0
            while j < len(plist):
                row = plist[j]

                # narrow it down
                t_plist = plist.copy()
                t_syns = syns.copy()
                for i in range(ncol-1):
                    ind = np.where(abs(t_plist[:, i] - row[i]) < ZERO_TOLERANCE)[0]
                    t_plist = t_plist[ind]
                    t_syns = t_syns[ind]

                # if there is really nothing, one value is copied
                if len(t_plist) == 1:
                    intens = t_syns[0]
                    new_plist.append(row[:ncol-1])
                    new_syns.append(intens)
                    j += len(ind)
                    if self.debug:
                        print("Skipping interpolation in %s - there is only one spectrum for %s." % (str(xnew), str(t_plist[:,:ncol-1])))
                    continue

                # sort according to the last column
                ind = np.argsort(t_plist[:, ncol-1])

                # abscissa
                x = t_plist[ind, ncol-1]
                t_syns = t_syns[ind]

                if self.debug:
                    print("Interpolating in: %s at value %s." % (str(x), xnew))

                intens = interpolate_block_fast(x, t_syns, xnew)

                new_plist.append(row[:ncol-1])
                new_syns.append(intens)
                j += len(ind)

            syns = np.array(new_syns)
            plist = np.array(new_plist)
            ncol = len(plist[0])

        return syns[0]

    def resolve_degeneracy(self, spectra):
        """
        Choose the preferred spectrum, if 2 have same properties.

        :param spectra: a list of spectra

        """
        ind = []
        for i in range(0, len(spectra)):
            ind.append(self.grid_order.index(spectra[i].props['family']))

        ind = np.array(ind)
        if np.any(ind < -1):
            warnings.warn('One grid was not found in self.grid_order.')

        return spectra[np.argmin(ind)]

#    @profile
    def select_parameters(self, values=[], order=2, constraints={}, **props):
        """
        Select parameters of synthetic spectra.

        :param values: parameter values (old)
        :param order: how many spectra should be used, adjusted dynamically
        :param constraints: to resolve conflicts between grids
        :param props: a dictionary of properties
        :return values: parameter values (new)

        """
        key = list(props.keys())[0]
        v = props.pop(key)

        eligible = np.array(self.get_available_values_fast(key, **constraints))
        ind = np.argsort(abs(eligible - v))
        vals = eligible[ind]

        # Note: What if the grid step is inhomogeneous?
        # Note: Actually, it is in z --- what shall we do?!

        if vals[:order].min() > v or vals[:order].max() < v:
            try:
                lower = np.max(vals[np.where(vals - v < ZERO_TOLERANCE)[0]])
                upper = np.min(vals[np.where(vals - v > ZERO_TOLERANCE)[0]])
                vals = np.array([lower, upper])
            except:
                pass

        if not is_within_interval(v, vals):
            return values

        # if there are no other spectra to interpolate in
        if len(list(props.keys())) == 0:
            for i in range(0, len(vals)):

                # append those that are already fixed
                row = []
                for key in list(constraints.keys()):
                    row.append(constraints[key])

                # append the last parameter
                row.append(vals[i])
                values.append(row)

                # once 'order' spectra are appended, end
                if i == order-1:
                    break

        else:
            j = 0
            for i in range(0, len(vals)):

                # add a constraint
                constraints[key] = vals[i]

                # recursively call this function!
                values_new = self.select_parameters(values=copy.deepcopy(values), order=order, constraints=constraints, **props)

                # some searches are in vain - so we wait until...
                if len(values_new) > len(values):
                    j += 1

                # copies the result, so we can go on
                values = values_new

                # remove constraint
                constraints.pop(key)
                if j == order:
                    break

        return values

    def verify_parameters(self, order=2, **props):
        """
        A wrapper to select_parameters().

        :parameter order: how many spectra should be used
        :parameter props: a dictionary of properties
        :return parList: parameters of spectra, 1 row = 1 spectrum
        :return vals: values to be interpolated
        :return keys: keys of the interpolated

        """
        keys = list(props.keys())
        vals = [props[key] for key in keys]

        parList = self.select_parameters(order=order,**props)
        parList = self.delete_exact(parList, **props)

        if len(parList) == 0:
            raise Exception('Do %s lie within the grid? I do not think so...' % (str(props)))

        tmp = np.array(parList)
        for i, val in enumerate(vals):
            if not is_within_interval(val, tmp[:, i]):
                raise ValueError('Parameters %s lie outside the grid.' % (str(props)))

        return parList, vals, keys


########################################################################

def main():
    import sys

    if len(sys.argv) < 6:
        print("Usage: pyterpolmini.py Teff logg metal lambda1 lambda2")
        sys.exit(1)

    teff = float(sys.argv[1])
    logg = float(sys.argv[2])
    z = float(sys.argv[3])
    lambda1 =float(sys.argv[4]) 
    lambda2 = float(sys.argv[5])

    flux_type = 'relative'
    step = 0.01

    sg = SyntheticGrid(flux_type=flux_type, debug=False)

    props = {'teff': teff, 'logg': logg, 'z': z}

    c = sg.get_synthetic_spectrum(props, [lambda1, lambda2], order=2, step=step, padding=0.0)

    c.write_spectrum('pyterpolmini.out')

if __name__ == "__main__":
    main()


