#!/bin/sh

make test_legendre

./test_legendre > test_legendre.out; less -S test_legendre.out


