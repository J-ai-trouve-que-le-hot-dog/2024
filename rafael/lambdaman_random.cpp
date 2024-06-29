#include "header.hpp"

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

i64 random_next(i64 x) { return (x * 48271) % 2147483647; }

const i64 RANDOM_SIZE = 62'500;

struct state {
  bitset<20000> visited;
  i64           position;
  i64           nvisited;
  vector<i64>   history;
  i64           value;
  i64           maxseed;
  
  bool operator<(state const& o) const { return value < o.value; }
  bool operator>(state const& o) const { return value > o.value; }

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
      return 50000 - ret;
    }
    return 50000 - ret;
  }
  
  void step(vector<array<i64,4>> const& graph, vector<vector<i64>> const& T) {
    i64 seed = 1+rng.random64((1<<10)-1);
    history.eb(seed);

    maxseed = max(maxseed, seed);
    
    FOR(i, RANDOM_SIZE) {
      i64 d = seed % 4;

      i64 nposition = graph[position][d];
      if(nposition != -1) {
        position = nposition;
        if(!visited[position]) {
          visited[position] = 1;
          nvisited += 1;
        }
      }
      
      seed = random_next(seed);
    }

    value = 100000 * calc_value(T) - maxseed;
  }


};

// void print_string(vector<i64> const& seeds) {
//   string s; 
//   for(auto seed : seeds){
//     FOR(i, RANDOM_SIZE) {
//       i64 d = seed % 4;
//       s += dc[d];
//       seed = random_next(seed);
//     }
//   }
//   s.resize(1000);
// }

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

  vector<state> BEAM;
  BEAM.eb();
  BEAM.back().visited = 0;
  BEAM.back().visited[start] = 1;
  BEAM.back().nvisited = 1;
  BEAM.back().position = start;

  const i64 BRANCH = 128;
  const i64 WIDTH  = 1'000;
  
  FOR(step, 1'000'000 / RANDOM_SIZE) {  
    debug(BEAM.size());
    vector<min_queue<state> > NBEAM(sz);

    i64 branch = BRANCH;
    if(step == 0) branch *= WIDTH;

    i64 it = 0;
    for(auto const& sa : BEAM) {
      it += 1;
      if(it % 1000 == 0) debug(it);
      vector<state> children(branch);
#pragma omp parallel for
      FOR(i, branch) {
        children[i] = sa;
        children[i].step(graph, T);
      }
      FOR(i, branch) {
        auto const& sb = children[i];
        auto key = 0; // sb.position;
        if(NBEAM[key].size() < WIDTH){
          NBEAM[key].push(sb);
        }else if(sb.value > NBEAM[key].top().value){
          NBEAM[key].pop();
          NBEAM[key].push(sb);
        }
      }
    }

    BEAM.clear();
    FOR(i, sz) while(!NBEAM[i].empty()){
      BEAM.eb(NBEAM[i].top());
      NBEAM[i].pop();
    }

    sort(all(BEAM), greater<>());

    for(auto const& sa : BEAM) {
      if(sa.nvisited == sz) {
        debug(sa.history);
        auto seeds = sa.history;
        // print_string(seeds);
        cout << id << ' ';
        cout << RANDOM_SIZE << ' ';
        cout << sa.history.size() << ' ';
        for(auto x : sa.history) cout << x << ' ';
        cout << endl;
        return 0;
      }
    }
    
    i64 best = 0;
    for(auto const& sa : BEAM) best = max(best, sa.value);
    debug((step+1) * RANDOM_SIZE, best, sz);
  }
  
  return 0;
}
