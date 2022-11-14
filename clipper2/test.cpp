#include <cstdlib>
#include <iomanip>
#include <string>

#include "clipper.h"

using namespace Clipper2Lib;

void Output(PathsD path) {
  for (int i=0; i<path.size(); i++) {
    for (int j=0; j<=path[i].size(); j++) {
      int k = j % path[i].size();
      std::cout << std::scientific << std::setprecision(8) << path[i][k].x << " " << path[i][k].y << std::endl;
    }
    std::cout << std::endl;
  }
}

int main()
{
  PathsD subject, clip, solution;
  subject.push_back(MakePathD("0, 0, 1, 0, 0, 1"));
  clip.push_back(MakePathD("0, 0, 1, 0, 1, 1"));
  solution = Difference(subject, clip, FillRule::NonZero, 6);

  PathsD clip2, solution2;
  clip2.push_back(MakePathD("0.1, 0.3, 0.2, 0.3, 0.1, 0.4"));
  solution2 = Difference(solution, clip2, FillRule::NonZero, 6);

//  std::cout << solution.size() << std::endl;
//  std::cout << solution[0].size() << std::endl;
//  std::cout << solution << std::endl;
//  std::cout << solution[0] << std::endl;
//  std::cout << solution[0][0] << std::endl;
//  std::cout << solution[0][0].x << std::endl;
//  std::cout << solution[0][0].y << std::endl;

  Output(solution2);

  return 0;
}


