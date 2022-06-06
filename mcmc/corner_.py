#!/usr/bin/env python3

import numpy as np
import corner

def main():

  burn = 150

  chain = []
  lnprob = []
  f = open("chain.tmp2", "r")
  for line in f.readlines():
    if line[0:1] == "#":
      continue
    l = line.split()
    iter = int(l[0])
    if iter < burn:
      continue
    tmp = list(map(lambda x: float(x), l[3:]))
    chain.append(tmp)
    lnprob.append(float(l[2]))
  f.close()

  chain = np.array(chain)
  lnprob = np.array(lnprob)
  print(lnprob)
  i = np.argmax(lnprob)

  fig = corner.corner(chain, \
    quantiles=[0.16, 0.5, 0.84], \
    labels=["m1", "P1", "loge1", "i1", "Omega1", "varpi1", "lambda1"], \
    show_titles=True, \
    truths=chain[i], \
    )
  fig.savefig('corner_.png')

if __name__ == "__main__":
  main()

