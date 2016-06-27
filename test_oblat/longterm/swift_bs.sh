#!/bin/sh

./rm.sh
./pl.sh

./swift_bs < swift_bs.in
#nohup ./swift_bs < swift_bs.in > swift_bs.out 2> swift_bs.err &

cd ../xvpl2el/
./xvpl2el.sh


