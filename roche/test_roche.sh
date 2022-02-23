#!/bin/sh

make clean
make || exit

./test_roche

./test_roche.plt

