#!/bin/sh

make

export OMP_NUM_THREADS=4
export OMP_NUM_THREADS=1

./gravity < gravity.in > gravity.out; less -S gravity.out

