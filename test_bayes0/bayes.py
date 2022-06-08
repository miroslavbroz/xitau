#!/usr/bin/env python3

"""
bayes.py
Aproximace primkou, vc. vypoctu nejistot (Monte Carlo Markov Chain; MCMC)

"""

import numpy as np
import scipy.optimize
import matplotlib.pyplot as plt
import emcee
import corner

def minus_ln_likelihood(params, x, y, sigma):
    """Verohodnost -ln p(x_i,y_i,sigma_i|a,b,f)"""
    a, b, f = params
    model = a*x + b
    s2 = (f*sigma)**2
    J = 0.5*np.sum((y-model)**2/s2 + np.log(s2) + np.log(2.0*np.pi))
#    print("J = ", J)
    return J

def minus_ln_prior(params):
    """Prior -ln p(a,b,f)"""
    a, b, f = params
    if 0.1 < a < 2.0 and 0.0 < b < 2.0 and 0.5 < f < 5.0:
        return 0.0
    else:
        return np.inf

def ln_posterior(params, x, y, sigma):
    """Posterior ln p(a,b,f|x_i,y_i,sigma_i)"""
    tmp = minus_ln_prior(params)
    if not np.isfinite(tmp):
        return -np.inf
    else:
        return -(minus_ln_prior(params) + minus_ln_likelihood(params, x, y, sigma))

def lsm(x, y, sigma):
    """Metoda nejmensich ctvercu (LSM)"""
    n = len(x)
    S   = 0.0
    Sx  = 0.0
    Sxx = 0.0
    Sy  = 0.0
    Syy = 0.0
    Sxy = 0.0

    for i in range(0,n):
        s2 = sigma[i]**2
        S   += 1.0/s2
        Sx  += x[i]/s2
        Sy  += y[i]/s2
        Sxx += x[i]*x[i]/s2
        Syy += y[i]*y[i]/s2
        Sxy += x[i]*y[i]/s2

    Delta = S*Sxx - Sx**2
    a = (S*Sxy - Sx*Sy)/Delta
    b = (Sxx*Sy - Sx*Sxy)/Delta
    sigma_a = np.sqrt(S/Delta)
    sigma_b = np.sqrt(Sxx/Delta)

    return a, b, sigma_a, sigma_b

def main():
    """Nacteni dat, maximalizace verohodnosti, vypocet MCMC, rohovy obrazek, ..."""

    x, y, sigma = np.loadtxt("xy.dat", usecols=[0,1,2], unpack=True)

    # metoda nejmensich ctvercu
    a, b, sigma_a, sigma_b = lsm(x, y, sigma)
    a_lsm = a
    b_lsm = b

    print("# lsm (1-sigma):")
    print("a = ", a, " +- ", sigma_a)
    print("b = ", b, " +- ", sigma_b)
    print()

    # numericka optimalizace
    a = 1.0
    b = 0.0
    f = 1.0
    result = scipy.optimize.minimize(lambda *args: minus_ln_likelihood(*args), \
        [a,b,f], args=(x,y,sigma), method='Nelder-Mead', tol=1.0e-4)
    a, b, f = result.x
    a_opt = a
    b_opt = b
    f_opt = f

    print("# optimize:")
    print("a = ", a)
    print("b = ", b)
    print("f = ", f)
    print()

    # MCMC
    ndim = 3
    walkers = 32
    position = [ result.x + 1.0e-4*np.random.rand(ndim) for i in range(walkers) ]
    sampler = emcee.EnsembleSampler(walkers, ndim, ln_posterior, args=(x,y,sigma))
    sampler.run_mcmc(position, 500)

    samples = sampler.chain[:,100:,:].reshape((-1,ndim))
    a, b, f = map(lambda x: (x[1], x[2]-x[1], x[0]-x[1]), zip(*np.percentile(samples, [16,50,84], axis=0)))
    a_mcmc = a[0]
    b_mcmc = b[0]
    f_mcmc = f[0]
    print("# mcmc (16,50,84-percentiles):")
    print("a = ", a[0], " + ", a[1], " - ", abs(a[2]))
    print("b = ", b[0], " + ", b[1], " - ", abs(b[2]))
    print("f = ", f[0], " + ", f[1], " - ", abs(f[2]))
    print()

    fig = plt.figure()
    plt.subplot(311)
    plt.plot(sampler.chain[:,:,0].transpose())
    plt.xlabel("krok")
    plt.ylabel("$a$")
    plt.subplot(312)
    plt.plot(sampler.chain[:,:,1].transpose())
    plt.ylabel("$b$")
    plt.subplot(313)
    plt.plot(sampler.chain[:,:,2].transpose())
    plt.ylabel("$f$")
    plt.savefig("chain.png")

    fig = corner.corner(samples, labels=["$a$","$b$","$f$"], truths=[a_mcmc,b_mcmc,f_mcmc])
    fig.savefig("corner.png")

    fig = plt.figure()
    plt.xlabel("$x$")
    plt.ylabel("$y$")
    for a_tmp, b_tmp, f in samples[np.random.randint(len(samples), size=100)]:
        plt.plot(x, a_tmp*x + b_tmp, color='k', alpha=0.1)
    plt.plot(x, a_lsm*x + b_lsm, color='orange', lw=6, label='lsm')
    plt.plot(x, a_opt*x + b_opt, color='lime', lw=4, label='opt')
    plt.plot(x, a_mcmc*x + b_mcmc, color='blue', lw=2, label='mcmc')
    plt.legend()
    plt.errorbar(x, y, yerr=sigma, fmt='k+', ecolor='y', capsize=2.0)
    plt.savefig("xy.png")

if __name__ == "__main__":
    main()

