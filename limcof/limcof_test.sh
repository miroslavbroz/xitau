#!/bin/sh

make

./limcof_test > limcof_test.out

awk '/^ *#.* = /{ print $2 " = " $4; }' < limcof_test.out > limcof_test.lab
./limcof_test.plt

