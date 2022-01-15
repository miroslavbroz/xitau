#!/bin/sh

DATE="date +%s.%N"

make test_omp

export OMP_NUM_THREADS=1
T1=`$DATE`
./test_omp > test_omp.out
T2=`$DATE`
awk -vT1=$T1 -vT2=$T2 'BEGIN{ print T2-T1; }' /dev/null >> test_omp.out

export OMP_NUM_THREADS=2
T1=`$DATE`
./test_omp >> test_omp.out
T2=`$DATE`
awk -vT1=$T1 -vT2=$T2 'BEGIN{ print T2-T1; }' /dev/null >> test_omp.out

export OMP_NUM_THREADS=4
T1=`$DATE`
./test_omp >> test_omp.out
T2=`$DATE`
awk -vT1=$T1 -vT2=$T2 'BEGIN{ print T2-T1; }' /dev/null >> test_omp.out

less -S test_omp.out

