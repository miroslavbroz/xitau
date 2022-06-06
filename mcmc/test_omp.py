#!/usr/bin/env python3
# coding: utf-8

from multiprocessing import Pool

from myxitau import *

def main():
  '''
  Test OpenMP parallelisation (1 node, 1 cpu, >threads).

  '''

  myxitau = Myxitau()

  pool = Pool()
  run_mcmc(myxitau, pool=pool)

if __name__ == "__main__":
  main()


