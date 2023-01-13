// crop_in_c.cpp
// Crop polygon in C++ library.
// Miroslav Broz (miroslav.broz@email.cz), Nov 5th 2022

// Note: On output, the polygon must be reverted (C -> F90).

#include <cstdlib>
#include "../clipper2/clipper.h"

using namespace Clipper2Lib;

#include "../lc_polygon/clip_in_c.h"

extern "C" {
  void crop_in_c(polystype* poly_i, polystype* poly_j, polystype* poly_k);
}

void crop_in_c(polystype* poly_i, polystype* poly_j, polystype* poly_k) {

  PathsD path_i, path_j, path_k;

  path_i = MakePathsFromD(poly_i);
  path_j = MakePathsFromD(poly_j);

  path_k = Intersect(path_i, path_j, FillRule::NonZero, DECIMAL_PRECISION);

  *poly_k = MakePolysFromD(path_k);

  return;
}


