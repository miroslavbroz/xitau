#!/bin/sh

FILE=planck3

./$FILE.plt
patch $FILE.eps $FILE.eps.diff


