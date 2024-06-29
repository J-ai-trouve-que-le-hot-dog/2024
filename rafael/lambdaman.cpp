#include "header.hpp"
// #include "ga-eax/environment.h"

using f64 = double;
using f32 = float;

const i64 MAX_ITER = 5'000'000'000;

const i32 dx[4] = {-1,1,0,0};
const i32 dy[4] = {0, 0, -1, 1};
const char* dc = "UDLR";


// int gBestValue = -1; // global best value
// TIndi gBest; // global best solution
// int optimum; // optimum cost
// int duration; // used time


int main(int argc, char** argv) {
  runtime_assert(argc >= 2);
  i64 id = atoi(argv[1]);
  runtime_assert(1 <= id && id <= 21);

  // i32 seed; cin>>seed; debug(seed);
  
  ifstream is("inputs/lambdaman" + to_string(id));
  runtime_assert(is.good());
  vector<string> grid;
  string line;
  while(getline(is, line)) {
    if(line.empty()) break;
    grid.eb(line);
    debug(line);
  }

  i32 n = grid.size(), m = grid[0].size();
  i32 sz = n*m;
  debug(n,m,sz);

  i32 x,y;
  FOR(i, n) FOR(j, m) if(grid[i][j] == 'L') {
    x=i; y=j;
  }
  grid[x][y] = ' ';

  string out; cin>>out;
  for(char c : out) {
    i32 d = 0; while(dc[d] != c) d += 1;
    x += dx[d];
    y += dy[d];
    if(x<0||x>=n||y<0||y>=m||grid[x][y]=='#') {
      x -= dx[d];
      y -= dy[d];
    }
    grid[x][y] = ' ';
  }

  grid[x][y] = 'L';

  FOR(i, n) {
    FOR(j, m) {
      cout << grid[i][j];
    }
    cout << endl;
  }
  
  return 0;
}
