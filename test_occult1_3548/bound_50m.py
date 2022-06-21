#!/usr/bin/env python3

import shapefile
sf = shapefile.Reader("ne_50m_admin_0_boundary_lines_land.shp")
shapes = sf.shapes()

f = open("bound_50m.txt", "w")

i = 0
for shape in shapes:
  i += 1
  for point in shape.points:
    f.write("%f %f\n" % (point[0], point[1]))
  f.write("\n\n")

f.close()

