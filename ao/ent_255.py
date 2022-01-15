#!/usr/bin/env python

from math import log10
import subprocess

def log2(x):
    return log10(x)/log10(2.0)

def read_pnm(filename):

    f = open(filename, "r")
    f.readline()
    f.readline()
    l = f.readline().split()
    w = int(l[0])
    h = int(l[1])
    f.readline()

    im = []
    tmp = []
    i = 0
    j = 0
    for line in f.readlines():
        l = line.split()
        for k in xrange(0,len(l)):
            if i >= w:
               i = 0
               j += 1
               im.append(tmp)
               tmp = []
            tmp.append(float(l[k]))
            i += 1

    f.close()
    im.append(tmp)        
    return im

def entropy(im):
    w = len(im)
    h = len(im[0])
    tot = 255.0

    S = 0.0
    for i in xrange(0,w):
        for j in xrange(0,h):
            p_i = im[i][j]/tot
            if p_i > 0.0:
                S -= p_i*log2(p_i) 

    return S, tot

def main():
    files = subprocess.check_output("ls *.pnm", shell=True)
    files = files.split()

    for fil in files:
        im = read_pnm(fil)
        S, tot = entropy(im)
        print "fil = %-24s	S = %f		tot = %f" % (fil, S, tot)

if __name__ == "__main__":
    main()



