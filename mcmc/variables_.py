#!/usr/bin/env python3

import numpy as np
import corner

def main():

  chain = []
  f = open("variables.tmp", "r")
  for line in f.readlines():
    if line[0:1] == "#":
      continue
    l = line.split()
    jd = float(l[0])
    if jd > 2459875.0:
      tmp = list(map(lambda x: float(x), l[1:3]))
      chain.append(tmp)
  f.close()

  walkers = 16
  burn = 150
  burn = 0
  chain = chain[burn*walkers:]
  chain = np.array(chain)

  fig = corner.corner(chain, \
    quantiles=[0.16, 0.5, 0.84], \
    labels=["u", "v"], \
    show_titles=True, \
    title_fmt=".3g",\
    )
  fig.savefig('variables_.png')

if __name__ == "__main__":
  main()


