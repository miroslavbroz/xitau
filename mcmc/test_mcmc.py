#!/usr/bin/env python3
# coding: utf-8

from myxitau import *

class Myxitau2(Myxitau):
  '''
  A linear model; w. a lot of stuff inherited from Myxitau.

  Reference: see bayes.pdf.

  '''

  def __init__(self):
    self.x, self.yobs, self.yerr = np.loadtxt("xy.dat", usecols=[0,1,2], unpack=True)
    self.ysyn = None
    self.chi = None

  def model(self, theta):
    a, b, f = theta
    return a*self.x + b

  def lnlike(self, theta):
    '''
    Note: f .. nuisance parameter for underestimated uncertainties (self.yerr).

    '''
    a, b, f = theta
    s2 = (f*self.yerr)**2
    self.ysyn = self.model(theta)
    self.chi = (self.yobs - self.ysyn)**2/s2
    return -0.5*np.sum(self.chi + np.log(s2) + np.log(2.0*np.pi))

  def lnprior(self, theta):
    '''
    Note: without a normalisation of p!

    '''
    a, b, f = theta
    if 0.1 <= a <= 2.0 and -2.0 <= b <= 2.0 and 0.5 <= f <= 5.0:
      return 0.0
    else:
      return -np.inf

  def initial_parameters(self):
    a = 1.0
    b = 0.1
    f = 1.0
    theta = [a, b, f]
    return theta

def main():
  '''
  Test MCMC on a linear model.

  '''

  myxitau = Myxitau2()

  run_mcmc(myxitau, nwalkers=6, delta=0.001)

if __name__ == "__main__":
  main()


