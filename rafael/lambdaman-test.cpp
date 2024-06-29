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
  vector<vector<i32>> dist(sz+1, vector<i32>(sz+1, 1000));

  FOR(x0, n) {
    debug(x0);
#pragma omp parallel for
    FOR(y0, m) if(grid[x0][y0] != '#') {
      queue<tuple<i32,i32>> Q;
      Q.push(mt(x0,y0)); dist[x0*m+y0][x0*m+y0] = 0;
      while(!Q.empty()) {
        auto [x,y] = Q.front(); Q.pop();
        auto di = dist[x0*m+y0][x*m+y];
        FOR(d, 4) {
          i32 x1 = x+dx[d], y1 = y+dy[d];
          if(x1<0||x1==n||y1<0||y1==m) continue;
          if(grid[x1][y1] == '#') continue;
          if(dist[x0*m+y0][x1*m+y1] <= di+1) continue;
          dist[x0*m+y0][x1*m+y1] = di+1;
          Q.push(mt(x1,y1));
        }
      }
    }
  }

  vector<i32> route;
  i32 start;
  FOR(x0, n) {
    FOR(y0, m) {
      if(grid[x0][y0] != '#') {
        dist[x0*m+y0][sz] = 0;
        if(grid[x0][y0] == 'L') {
          dist[sz][x0*m+y0] = 0;
          start = x0*m+y0;
        }else{
          route.eb(x0*m+y0);
        }
      }
    }
  }

  rng.shuffle(route);
  route.insert(begin(route), start);
  route.eb(sz);
  
  debug(route.size());

  i32 rsz = route.size()-1;
  
  i64 score = 0;
  FOR(i, rsz) score += dist[route[i]][route[i+1]];

  debug(score);

  i64 niter = 0;
  f64 done  = 0.0;
  f64 temp0 = 16.0;
  f64 temp1 = 0.1;
  f64 temp = temp0;

  auto best_route = route;
  auto best_score = score;
  
  auto accept = [&](i64 delta) -> bool {
    if(delta <= 0) return true;
    return delta <= temp * rng.randomDouble();
  };
  
  while(1) {
    niter += 1;
    if(niter % 1024 == 0) {
      done = (f64) niter / MAX_ITER;
      if(done > 1.0) break;
      temp = temp0 * pow(temp1 / temp0, done);
    }
    if(niter % (1<<20) == 0) {
      cerr
        << "id = " << setw(2) << id << ", "
        << "niter = " << setw(12) << niter << ", "
        << "done = " << fixed << setprecision(6) << done << ", "
        << "temp = " << fixed << setprecision(6) << temp << ", "
        << "score = " << setw(9) << score << ", "
        << "best_score = " << setw(9) << best_score << ", "
        << endl;
    }


    i32 ty = rng.random32(2);
    if(ty == 0) {
      i32 i = rng.random32(rsz);
      i32 j = rng.random32(rsz);
      if(i > j) swap(i,j);
      if(i == j) continue;

      i32 a1 = route[i], a2 = route[i+1];
      i32 b1 = route[j], b2 = route[j+1];

      i64 delta = 0;
      delta -= dist[a1][a2];
      delta -= dist[b1][b2];
      delta += dist[a1][b1];
      delta += dist[a2][b2];

      if(accept(delta)) {
        score += delta;
        reverse(begin(route)+i+1, begin(route)+j+1);
      }
    }else if(ty == 1) {
      i32 i = rng.random32(rsz);
      i32 j = rng.random32(rsz);
      i32 k = rng.random32(rsz);
      if(i > j) swap(i,j);
      if(j > k) swap(j,k);
      if(i > j) swap(i,j);
      if(i == j) continue;
      if(j == k) continue;

      i32 a1 = route[i], a2 = route[i+1];
      i32 b1 = route[j], b2 = route[j+1];
      i32 c1 = route[k], c2 = route[k+1];

      i64 delta = 0;
      delta -= dist[a1][a2];
      delta -= dist[b1][b2];
      delta -= dist[c1][c2];
      delta += dist[a1][b2];
      delta += dist[b1][c2];
      delta += dist[c1][a2];

      if(accept(delta)) {
        score += delta;
        rotate(begin(route)+i+1, begin(route)+j+1, begin(route)+k+1);
      }
    }

    // runtime_assert(route[0] == sz && route.back() == sz);
    
    // FOR(i, rsz) if(dist[route[i]][route[i+1]] > 100000) {
    //   debug(route[i], route[i+1], dist[route[i]][route[i+1]]);
    // }
    
    // { i64 rscore = 0;
    //   debug(route);
    //   FOR(i, rsz) rscore += dist[route[i]][route[i+1]];
    //   debug(score, rscore);
    //   runtime_assert(rscore == score);
    // }
    
    if(score < best_score) {
      best_score = score;
      best_route = route;
    }
  }
  
  return 0;
}
