#include "header.hpp"
#include <omp.h>

struct union_find {
  vector<int> A;

  union_find(int n = 0) : A(n) {
    iota(all(A), 0);
  }

  int find(int a) {
    return A[a] == a ? a : A[a] = find(A[a]);
  }

  bool unite(int a, int b) {
    a = find(a);
    b = find(b);
    if(a == b) {
      return false;
    }
    A[a] = b;
    return true;
  }
};


using f64 = double;
using f32 = float;

const i64 dx[4] = {-1,1,0,0};
const i64 dy[4] = {0, 0, -1, 1};
const char* dc = "UDLR";

// ( ((call @/ (i -/ !+ 1)) @/ ((s */ !+ 11) %/ !+ 78074891)) ^/
// i64 random_next(i64 x) { return (x * 17) % 78074891; }
i64 random_next(i64 x) { return (x * 48271) % 2147483647; }

struct state {
  bitset<20000> visited;
  i64           position;
  i64           nvisited;
  
  void reset(i32 start) {
    visited = 0;
    visited[start] = 1;
    nvisited = 1;
    position = start;
  }

  i64 calc_value(vector<vector<i64>> const& T) const {
    i64 ret = 0;

    auto dfs = letrec([&](auto dfs, i64 i, i64 p) -> bool {
      bool r = !visited[i];
      for(auto j : T[i]) if(j != p) {
          if(dfs(j,i)) r = 1;
        }
      if(r) ret += 1;
      return r;
    });
    
    FOR(i, T.size()) if(!visited[i]) {
      dfs(i, -1);
      return ret;
    }
    return ret;
  }
  
  void step(vector<array<i64,4>> const& graph, i32 seed) {
    FOR(i, 250'000) {
      i32 d = (seed % 4);
      FOR(k, 2) {
        i64 nposition = graph[position][d];
        if(nposition != -1) {
          position = nposition;
          if(!visited[position]) {
            visited[position] = 1;
            nvisited += 1;
          }
        }
      }
      seed = random_next(seed);
    }
  }


};

void print_string(i64 seed) {
  string s;
  FOR(i, 25000) {
    i64 d = seed % 4;
    s += dc[d];
    s += dc[d];
    seed = random_next(seed);
  }
  s.resize(1000);
  cout << s << endl;
}

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

  i64 n = grid.size(), m = grid[0].size();
  vector<array<i64,2>> points;
  vector<vector<i64>> unpoint(n, vector<i64>(m));
  FOR(i, n) FOR(j, m) if(grid[i][j] != '#') {
    unpoint[i][j] = points.size();
    points.pb({i,j});
  }
  i64 sz = points.size();
  debug(sz);
  runtime_assert(sz <= 20000);
  i64 start = 0;
  FOR(i, n) FOR(j, m) if(grid[i][j] == 'L') {
    start = unpoint[i][j];
  }

  vector<array<i64,4>> graph(sz);
  FOR(i, sz) {
    auto p = points[i];
    FOR(d, 4) {
      i64 x = p[0] + dx[d];
      i64 y = p[1] + dy[d];
      if(x<0||x>=n||y<0||y>=m||grid[x][y]=='#') {
        graph[i][d] = -1;
      }else{
        graph[i][d] = unpoint[x][y];
      }
    }
  }

  vector<vector<i64>> T(sz);
  union_find uf(sz);
  FOR(i, sz) FOR(d, 4) if(i64 j = graph[i][d]; j != -1) {
    if(uf.unite(i,j)) {
      T[i].eb(j);
      T[j].eb(i);
    }
  }

  i64 maxvisited = 0;
  i32 iter = 0;

  u64 base = rng.randomInt64();

#pragma omp parallel
  {
#pragma omp critical
    {
      debug(omp_get_thread_num(), base + 42 + omp_get_thread_num(), &rng);
      rng.reset(base + 42 + omp_get_thread_num());
    }
    state S;
    while(1) {
      S.reset(start);
      auto seed1 = rng.random64(2147483647);
      S.step(graph, seed1);
      auto value = S.calc_value(T);
      if(value < 1800) {
        #pragma omp critical
        {
          debug("here");
        }
        FOR(i, 1024) {
          auto T = S;
          auto seed2 = rng.random64(2147483647);
          T.step(graph, seed2);
#pragma omp critical
          {
            maxvisited = max(maxvisited, T.nvisited);
            if(T.nvisited == sz) {
              debug(seed1,seed2);
              exit(0);
            }
          }
        }
      }
#pragma omp critical
      {
        iter += 1;
        if(iter % 100 == 0) debug(iter, maxvisited, sz);
      }
    }
  }
  
  return 0;
}
