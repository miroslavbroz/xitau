#!/bin/sh

make && ./test_occult < test_occult.in > test_occult.out && ./test_occult.plt

