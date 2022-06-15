#!/bin/sh

make && ./test_sofa > test_sofa.out && less -S test_sofa.out

