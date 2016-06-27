#!/bin/sh

./lc < lc.in > lc.out
diff lc.out lc.out_BACKUP > lc.out.diff
./lc.plt

