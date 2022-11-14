// clip_in_c.cpp
// Clip polygon in C++ library.
// Miroslav Broz (miroslav.broz@email.cz), Nov 5th 2022

#include <cstdlib>
#include "../clipper2/clipper.h"

using namespace Clipper2Lib;

#include "clip_in_c.h"

#define DECIMAL_PRECISION	6

typedef struct {
  int c;
  double p[3][MAXPOLY];
} polytype;

typedef struct {
  int c;
  polytype s[MAXPOLYS];
} polystype;

extern "C" {
  void clip_in_c(polystype* poly_i, polystype* poly_j, polystype* poly_k);
}

inline PathsD MakePathsFromD(polystype* poly)
{
  PathsD result;
  for (int i=0; i<(*poly).c; i++) {
    PathD one;
    for (int j=0; j<(*poly).s[i].c; j++) {
      one.push_back(PointD((*poly).s[i].p[0][j], (*poly).s[i].p[1][j]));
    }
    result.push_back(one);
  }
  return result;
}

void clip_in_c(polystype* poly_i, polystype* poly_j, polystype* poly_k) {

  PathsD path_i, path_j, path_k;

  // structure -> object
  path_i = MakePathsFromD(poly_i);
  path_j = MakePathsFromD(poly_j);

  path_k = Difference(path_i, path_j, FillRule::NonZero, DECIMAL_PRECISION);

  // checks
  if (path_k.size() > MAXPOLYS) {
    std::cout << "clip_in_c(): Error: Set of polygons > MAXPOLYS! Recompile..." << std::endl;
    std::cout << "poly_i.c = " << (*poly_i).c << std::endl;
    std::cout << "poly_j.c = " << (*poly_j).c << std::endl;
    std::cout << "path_k.size() = " << path_k.size() << std::endl;
    std::cout << "MAXPOLYS = " << MAXPOLYS << std::endl;
    std::exit(1);
  }
  for (int i=0; i<path_k.size(); i++) {
    if (path_k[i].size() > MAXPOLY) {
      std::cout << "clip_in_c(): Error: Polygon length > MAXPOLY! Recompile..." << std::endl;
      std::cout << "poly_i.s[i].c = " << (*poly_i).s[i].c << std::endl;
      std::cout << "poly_j.s[i].c = " << (*poly_j).s[i].c << std::endl;
      std::cout << "path_k[i].size() = " << path_k[i].size() << std::endl;
      std::cout << "MAXPOLY = " << MAXPOLY << std::endl;
      std::exit(1);
    }
  }

  // object -> structure
  (*poly_k).c = path_k.size();
  for (int i=0; i<path_k.size(); i++) {
    (*poly_k).s[i].c = path_k[i].size();
    for (int j=0; j<path_k[i].size(); j++) {
      (*poly_k).s[i].p[0][j] = path_k[i][j].x;
      (*poly_k).s[i].p[1][j] = path_k[i][j].y;
      (*poly_k).s[i].p[2][j] = 0.0;
    }
  }

//  std::cout << "poly_i.s[1].c = " << (*poly_i).s[0].c << std::endl;
//  std::cout << "path_k = " << path_k << std::endl;
//  std::cout << "poly_k.c = " << (*poly_k).c << std::endl;
//  std::cout << "poly_k.s[0].c = " << (*poly_k).s[0].c << std::endl;
//  std::cout << "poly_k.s[1].c = " << (*poly_k).s[1].c << std::endl;
//  std::cout << "path_k.size = " << path_k.size() << std::endl;

  return;
}



