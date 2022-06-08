#!/usr/bin/env python3
# coding: utf-8

__author__ = "Miroslav Broz (miroslav.broz@email.cz)"
__version__ = "Jun 6th 2022"

import sys
import time
import numpy as np
import subprocess
import multiprocessing
import emcee

class Myxitau(object):
  '''
  My wrapper for Xitau computations.

  '''

  def __init__(self, debug=True):
    '''
    Initialisation.

    :param debug: Debugging.
    :return:

    lns ... \sum_i\ln sigma_i
    n   ... number of measurements

    ^* see ../xitau/simplex/chi2_func_SKY.f

    '''
    self.lns = None
    self.n = None
    self.debug = debug

    self.template = """# chi2.in
# nparam number of paramters
# x() vector:
#
#  m1 m2 m3
#  P1 loge1 i1 Omega1 varpi1 lambda1
#  P2 loge2 i2 Omega2 varpi2 lambda2
#  T1 T2 T3
#  logg1 logg2 logg3
#  vrot1 vrot2 vrot3
#  metal1 metal2 metal3
#  Deltat1 Deltat2 Deltat3
#  gamma
#  d_pc
#  pole_l pole_b
#  ...
#
22
 
   %22.16e  0.0000000000000000
   %22.16e  %22.16e  %22.16e  %22.16e  %22.16e  %22.16e
   5780.0000000000000        5780.0000000000000
   4.5000000000000000        4.5000000000000000
   0.0000000000000000        0.0000000000000000
   0.0000000000000000        0.0000000000000000
   0.0000000000000000        0.0000000000000000
   0.0000000000000000
   2.12472941525386e-05
   116.55899295367399       -64.999918878940207               18           0           0           0           0           0           0           0           0           0           0           0           0          18   16.620676462920510        0.0000000000000000        0.0000000000000000        0.0000000000000000        0.0000000000000000        0.0000000000000000        0.0000000000000000        0.0000000000000000        0.0000000000000000        0.0000000000000000        0.0000000000000000        0.0000000000000000        0.0000000000000000        1.9999999999984679        18.620676462918979     

2458373.907201		! fixed (dependent) parameters: T0 [JD]; HST, Brown etal. (2021), but in TDB

2			! nbod number of bodies; a list of 2*nbod+4 files with observational data (or '-' if no data)...
-
Sky2.dat
-
-
-
-
-
-

0			! nband number of photometric bands; a list of bands and lightcurve files
-
-
-
-
-
-
-
-
-

4                       ! geometry of the system: 0 ... hierarchical ((1+2)+3)+4, 1 ... two pairs ((1+2)+(3+4))+5, 4 ... hierarchical w. true longitude

-1.0 -1.0		! m_min(nbod) [M_S] minimum masses
10.0 10.0		! m_max(nbod) [M_S] maximum masses
F F			! use_hec88(nbod) use Harmanec (1988) to constrain components (i.e. use only T_eff as a parameter!)

0.005			! lightcurve_timestep [day] timestep of synthetic lightcurve; set to 0.0 if all observed points should be computed
0.100			! approx_eclipse_duration [day] approximate eclipse duration for eclipse detection
399.e-9 675.e-9		! lambda1 lambda2 [metres] wavelength range for rectified synthetic spectra
321.e-9 998.e-9		! lambda3 lambda4 [metres] range for absolute spectra (to fit SED)

0.4			! silh_factor [] signal = factor*max(signal) to compute a silhouette

T			! use_planck [T|F] use Planck approximation; otherwise use absolute synthetic spectra (*.abs files)
F			! use_filters [T|F] use filter transmissions to compute SED; otherwise use effective wavelengths and bandpasses
F			! use_limbdark [T|F] use linear limb-darkening coefficients for visibility |V|^2 calculation
F			! use_pyterpol [T|F] use Pyterpol (Nemravova et al. 2016) to generate synthetic spectra on-the-fly
T			! use_vardist [T|F] use variable distance/geometry (in solar system)
F			! use_varpole [T|F] use variable pole of body 1 (in solar system)
T			! use_multipole [T|F] use multipole expansion of body 1
F			! use_bruteforce [T|F] use shape of body 1 and brute-force algorithm
F                       ! use_ppn [T|F] use parametrized post-Newtonian approximation

1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 0.3 1.0 1.0	! w_SKY w_RV w_TTV w_ECL w_VIS w_CLO w_T3 w_LC w_SYN w_SED w_AO w_SKY2 w_SKY3 weights for chi^2 computation
1.e-8			! eps_BS for Bulirsch-Stoer integrator
F			! debugging output?
F			! debug integrator?

"""

  def chi2(self, theta):
    '''
    Computes chi^2 by calling Xitau.
    Uses stdin/stdout to be thread-safe!

    :param theta: Vector of free parameters.
    :return:

    '''
    if self.debug:
      print('theta = ', theta)

    stdin = self.template % tuple(theta)
#    if self.debug:
#      f = open("chi2.in", "w")
#      f.write(stdin)
#      f.close()
    stdin = bytes(stdin,'utf-8')

    stdout = subprocess.check_output("./chi2", input=stdin, shell=True)

    stdout = str(stdout,'utf-8')
#    if self.debug:
#      f = open("chi2.out", "w")
#      f.write(stdout)
#      f.close()
    stdout = stdout.split('\n')

    for line in stdout:
      l = line.split()
      if len(l)<=3:
        continue

      key = l[1]
      val = l[3]
      if key == "chi2":
         chi2 = float(val)
      elif key == "lns":
         self.lns = float(val)
      elif key == "n_fit":
         self.n = int(val)

    if self.debug:
      print('chi2 = ', chi2)
#      print('lns = ', self.lns)
#      print('n = ', self.n)

    return chi2

  def lnlike(self, theta):
    '''
    Likelihood +ln p(data|theta).

               1                   x^2
    p = ---------------- exp [- ---------]
        sigma sqrt(2 pi)        2 sigma^2

    ln p = -ln sigma - 0.5 ln(2 pi) - 0.5 x^2/sigma^2

    :param theta: Vector of free parameters.
    :return:

    '''
    chi2 = self.chi2(theta)
    lp = -self.lns - 0.5*self.n*np.log(2.0*np.pi) - 0.5*chi2

    if self.debug:
      print('lp = ', lp)

    return lp

  def lnprob(self, theta):
    '''
    Posterior +ln p(theta|data).

    :param theta: Vector of free parameters.
    :return:

    '''
    lp = self.lnprior(theta)
    if not np.isfinite(lp):
        return -np.inf
    return lp + self.lnlike(theta)

  def initial_parameters(self):
    '''
    Setting of initial parameters

    :return theta: Vector of free parameters.

    '''
    m1      = 7.6105998854483623E-014  # M_S
    P1      = 82.461062027919255       # day
    loge1   = -0.90326105659770728     # 1
    i1      = 154.99912245820809       # deg
    Omega1  = 296.55504801333757       # deg
    varpi1  = 153.37226353663044       # deg
    lambda1 = 138.73286940743924       # deg

    theta = m1,P1,loge1,i1,Omega1,varpi1,lambda1
    return theta

  def lnprior(self, theta):
    '''
    Prior +ln p(theta). Uninformative; assures appropriate ranges.
    Note: without a normalisation of p!

                1
    p(a) = ----------- if amin < a < amax else 0.0
           amax - amin 

    ln(p) = -ln(amax - amin) ... else -inf

    :param theta: Vector of free parameters.
    :return:

    '''
    m1,P1,loge1,i1,Omega1,varpi1,lambda1 = theta

    if  6.6e-14 <= m1      <= 8.6e-14 and \
        82.0    <= P1      <= 83.0    and \
        -3.0    <= loge1   <= -0.3    and \
        150.0   <= i1      <= 165.0   and \
        290.0   <= Omega1  <= 300.0   and \
        140.0   <= varpi1  <= 160.0   and \
        130.0   <= lambda1 <= 150.0   :
      return 0.0
    else:
      return -np.inf


def p0_func(theta, nwalkers=None, delta=1.0e-3):
  '''
  Creating initial positions of walkers.
  Note: Does not work for zero parameters!

  :param theta: Vector of free parameters.
  :param nwalkers: Number of walkers.
  :param delta: Dispesion of random numbers.
  :return:

  '''
  p0 = []
  for i in range(nwalkers):
    tmp = []
    for j in range(len(theta)):
      tmp.append(np.random.uniform(theta[j]*(1.0-delta), theta[j]*(1.0+delta)))
    p0.append(tmp)

  return np.array(p0)


def run_mcmc(myxitau, nwalkers=16, niter=1000, seed=1, thin=1, delta=1.0e-3, **kwarg):
  '''
  Running Monte-Carlo-Markov-Chain method.

  :param myxitau: Ref. to myxitau object.
  :param nwalkers: Number of walkers; minimum is 2 times the number of free parameters.
  :param niter: Number of iterations.
  :param seed: Random seed.
  :param thin: Use only every thin step from the chain.
  :return:

  '''
  theta = myxitau.initial_parameters()
  print('theta = ', theta)

  np.random.seed(seed)
  p0 = p0_func(theta, nwalkers=nwalkers, delta=delta)

  print('Checking p0:')
  for tmp in p0:
    lp = myxitau.lnprior(tmp)
    print('lp = ', lp)
    if not np.isfinite(lp):
      print('theta = ', np.array(tmp))
      raise ValueError('p0 out of range in self.lnprior()')

  ndim = len(theta)

  sampler = emcee.EnsembleSampler(nwalkers, ndim, myxitau.lnprob, **kwarg)

  f = open(f'chain.tmp', 'a')
  f.write("# iter walker lnprob m1 P1 loge1 i1 Omega1 varpi1 lambda1\n")
  f.write("# - - 1 M_S 1 deg deg deg deg\n")

  print("Running production...")
  t1 = time.time()

  pos, lnprob, state = sampler.run_mcmc(p0, 1, progress=True)

  for i in range(niter):
    print('iter = ', i)

    pos, lnprob, state = sampler.run_mcmc(None, 1, progress=True)

    for j in range(0,len(pos)):
      f.write("%6d %6d %16.8e" % (i, j, lnprob[j]))
      for k in range(0,len(pos[j])):
        f.write(" %22.16e" % (pos[j][k]))
      f.write("\n")

  f.close()

  print("Average acceptance fraction:", np.around(np.mean(sampler.acceptance_fraction),3), "(it should be between 0.2-0.5)")
  try:
     print("Autocorrelation time estimate:", sampler.get_autocorr_time(), "(it should be around n x 10)")
  except:
     print("Warning: Autocorrelation time can not be reliably estimated!")

  samples = sampler.flatchain
  theta_maxprob = samples[np.argmax(sampler.flatlnprobability)]
  chain = sampler.get_chain(thin=thin, flat=True, discard=0)

  np.savetxt('theta_maxprob.csv', theta_maxprob, delimiter=',')
  np.savetxt('chain.csv', chain, delimiter=',')

  t2 = time.time()
  print("Time: ", t2-t1, " s = ", (t2-t1)/3600.0, " h")

  print('run_mcmc() has ended sucessfully!')


def main():
  '''
  Main program.

  '''

  myxitau = Myxitau()

  theta = myxitau.initial_parameters()

#  myxitau.chi2(theta)
  run_mcmc(myxitau)

#  print(vars(myxitau))  # dbg

if __name__ == "__main__":
  main()


