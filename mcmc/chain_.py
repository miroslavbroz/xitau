#!/usr/bin/env python3

import sys
import numpy as np
import matplotlib.pyplot as plt

def main():

  walkers = 16
  chain = []
  for i in range(0,walkers):
    chain.append([])
  print(chain)
  f = open("chain.tmp2", "r")
  for line in f.readlines():
    if line[0:1] == "#":
      continue
    l = line.split()
    iter = int(l[0])
    walker = int(l[1])
    tmp = list(map(lambda x: float(x), l[3:]))
    chain[walker].append(tmp)
  f.close()

  chain = np.array(chain)

  fig = plt.figure(figsize=(8.0,10.0))

  plt.subplot(711)
  plt.plot(chain[:,:,0].transpose())
  plt.ylabel("m1")

  plt.subplot(712)
  plt.plot(chain[:,:,1].transpose())
  plt.ylabel("P1")

  plt.subplot(713)
  plt.plot(chain[:,:,2].transpose())
  plt.ylabel("loge1")

  plt.subplot(714)
  plt.plot(chain[:,:,3].transpose())
  plt.ylabel("i1")

  plt.subplot(715)
  plt.plot(chain[:,:,4].transpose())
  plt.ylabel("Omega1")

  plt.subplot(716)
  plt.plot(chain[:,:,5].transpose())
  plt.ylabel("varpi1")

  plt.subplot(717)
  plt.plot(chain[:,:,6].transpose())
  plt.ylabel("lambda1")

  plt.xlabel("iter")
  plt.xticks()
  plt.tight_layout()
  plt.savefig("chain_.png")

if __name__ == "__main__":
  main()

