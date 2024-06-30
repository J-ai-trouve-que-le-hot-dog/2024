#include "lambdaman.hpp"

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

int main(int argc, char** argv) {
  runtime_assert(argc >= 2);
  i64 id = atoi(argv[1]);
  runtime_assert(1 <= id && id <= 21);

  problem pb; pb.load(id);
  
  vector<state> BEAM;
  BEAM.eb();
  BEAM.back().visited = 0;
  BEAM.back().visited[pb.start] = 1;
  BEAM.back().nvisited = 1;
  BEAM.back().position = pb.start;

  const i64 BRANCH = 128;
  const i64 WIDTH  = 1'000;
  
  FOR(step, 1'000'000 / RANDOM_SIZE) {  
    debug(BEAM.size());
    vector<min_queue<state> > NBEAM(pb.sz);

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
        children[i].step(pb.graph, pb.T);
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
    FOR(i, pb.sz) while(!NBEAM[i].empty()){
      BEAM.eb(NBEAM[i].top());
      NBEAM[i].pop();
    }

    sort(all(BEAM), greater<>());

    for(auto const& sa : BEAM) {
      if(sa.nvisited == pb.sz) {
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
    debug((step+1) * RANDOM_SIZE, best, pb.sz);
  }
  
  return 0;
}
