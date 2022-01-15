#!/bin/sh

FILE=sphere-a

./pointsfaces2smesh.awk $FILE.points $FILE.faces > $FILE.smesh

tetgen -Vgpnq1.2a0.001 $FILE > tetgen.log

./nodeeleface2msh.awk $FILE.1.node $FILE.1.ele $FILE.1.face > $FILE.1.msh

ffmedit $FILE.1.mesh

exit

tetgen -Vgpnq1.2a0.001 $FILE > tetgen.log

