// clip_in_c.h
// Clip polygon in C++ library.
// Miroslav Broz (miroslav.broz@email.cz), Nov 5th 2022

// cf. polytype.f90!

#define MAXPOLY		64
#define MAXPOLYS	64

#define DECIMAL_PRECISION	6

typedef struct {
  int c;
  double p[3][MAXPOLY];
} polytype;

typedef struct {
  int c;
  polytype s[MAXPOLYS];
} polystype;

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

inline polystype MakePolysFromD(PathsD path)
{
  if (path.size() > MAXPOLYS) {
    std::cout << "clip_in_c(): Error: Set of polygons > MAXPOLYS! Recompile..." << std::endl;
    std::cout << "path.size() = " << path.size() << std::endl;
    std::cout << "MAXPOLYS = " << MAXPOLYS << std::endl;
    std::exit(1);
  }

  for (int i=0; i<path.size(); i++) {
    if (path[i].size() > MAXPOLY) {
      std::cout << "clip_in_c(): Error: Polygon length > MAXPOLY! Recompile..." << std::endl;
      std::cout << "path[i].size() = " << path[i].size() << std::endl;
      std::cout << "MAXPOLY = " << MAXPOLY << std::endl;
      std::exit(1);
    }
  }

  polystype poly;
  poly.c = path.size();
  for (int i=0; i<path.size(); i++) {
    poly.s[i].c = path[i].size();
    for (int j=0; j<path[i].size(); j++) {
      int k = path[i].size()-j-1;
      poly.s[i].p[0][k] = path[i][j].x;
      poly.s[i].p[1][k] = path[i][j].y;
      poly.s[i].p[2][k] = 0.0;
    }
  }
  return poly;
}


