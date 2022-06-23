#!/bin/sh

make && ./test_preces > test_preces.out && less -S test_preces.out

