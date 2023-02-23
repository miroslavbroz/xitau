#!/bin/sh

FILE=tri_file_octdec_3

./obj2smesh.awk $FILE.obj > $FILE.smesh

tetgen -Vgpna200.0 $FILE.smesh > tetgen.log

./nodeeleface2msh.awk $FILE.1.node $FILE.1.ele $FILE.1.face > $FILE.1.msh

ffmedit $FILE.1.mesh

exit


# we have to use Y parameter, if we have to preserve the surface mesh (due to precomputed shadowing)
#tetgen -VYgpna200.0 $FILE.smesh > tetgen.log

#./pointsfaces2smesh.awk $FILE.points $FILE.faces > $FILE.smesh
#tetgen -Vgpnq1.2a0.001 $FILE > tetgen.log

