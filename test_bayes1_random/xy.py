#!/usr/bin/env python3

import random

def main():

    a = 0.45
    b = 1.05
    n = 1000
    sigma0 = 0.1

    random.seed(-3)

    x = []
    y = []
    sigma = []
    for i in range(0,n):
        tmp = 10.0*float(i)/n
        x.append(tmp)
        y.append(a*tmp + b + random.normalvariate(0.0, sigma0))
        sigma.append(sigma0)

    f = open("xy.dat", "w")
    for i in range(0,n):
        f.write("%.8e %.8e %.8e\n" % (x[i], y[i], sigma[i]))
    f.close()

if __name__ == "__main__":
    main()


