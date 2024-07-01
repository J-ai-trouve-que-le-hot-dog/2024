#include "header.hpp"
#include "lambdaman.hpp"
#include <omp.h>


const i64 MAXN = 250'000;

const i64 B = 94;
const i64 MAXM = 2'000'000;

i64 random_next(i64 x, i64 c, i64 m) { return (x * c) % m; }

thread_local i64 xs[MAXM];
thread_local i64 last[MAXM];

thread_local i64 visited[MAXN];
thread_local i64 visited2[MAXN];
thread_local i64 date = 0;

i64 best = 0;

bool isprime(i64 m) {
  FORU(i, 2, m) {
    if(i*i > m) break;
    if(m%i==0) return 0;
  }
  return 1;
}

const i32 NBLOCK = 30;
const i32 BLOCK_SIZE = 1'000'000 / NBLOCK - 128;

struct state {
  bitset<10000> visited;
  i32 position;
  i32 nvisited;
  i32 value;
  i32 total;

  i32 history[NBLOCK];

  bool operator<(state const& o) const{ return value < o.value; }

  void reset(problem const& pb) {
    visited = 0;
    position = pb.start;
    nvisited = 0;
    total = 0;
  }
  
  void step(problem const& pb, i64 x){
    i64 nr = 1; // x%8<4?3:1;
    FOR(k, nr) {
      i64 nposition = pb.graph[position][x%4];
      total += 1;
      if(nposition != -1) {
        position = nposition;
        if(!visited[position]) {
          visited[position] = 1;
          nvisited += 1;
        }
      }
    }
  }

  void calc_value(problem const& pb) {
    value = 0;
    if(total > 1000000) value += 1000000;
    
    auto dfs = letrec([&](auto dfs, i64 i, i64 p) -> bool {
      bool r = !visited[i];
      for(auto j : pb.T[i]) if(j != p) {
          if(dfs(j,i)) r = 1;
        }
      if(r) value += 1;
      return r;
    });
    
    FOR(i, pb.sz) if(!visited[i]) {
      dfs(i, -1);
      break;
    }
  }


};

const i32 WIDTH    = 8000;
const i32 MAX_SEED = 110;

bool test(problem const& pb, i64 m, i64 c) {
  vector<state> BEAM;
  BEAM.eb();
  BEAM.back().reset(pb);

  FOR(iter, NBLOCK) {
    max_queue<state> NBEAM;
    i32 cnt = 0;
    for(auto const& s : BEAM) {
      cnt += 1;
      if(cnt % 128 == 0) debug(cnt);
#pragma omp parallel for
      FORU(x0, 1, MAX_SEED) {
        state t = s;
        t.history[iter] = x0;
        auto x = x0;
        FOR(i, BLOCK_SIZE) {
          t.step(pb,x);
          x = random_next(x, c, m);
        }
        t.calc_value(pb);
        if(t.value == 0) runtime_assert(t.nvisited == pb.sz);
#pragma omp critical
        {
          if(NBEAM.size() < WIDTH) {
            NBEAM.push(t);
          }else if(t.value < NBEAM.top().value) {
            NBEAM.pop();
            NBEAM.push(t);
          }
        }
      }
    }
    BEAM.clear();
    while(!NBEAM.empty()) {
      BEAM.eb(NBEAM.top());
      NBEAM.pop();
    }

    i32 fr = min_element(all(BEAM))->value;
    i32 to = max_element(all(BEAM))->value;
    debug((iter+1) * BLOCK_SIZE, fr, to);
    if(fr == 0) break;
  }

  cout << BLOCK_SIZE << endl;
  
  auto s = *min_element(all(BEAM));
  FOR(i, NBLOCK) {
    cout << (i?"; ":"") << s.history[i] << ' ';
  }
  cout << endl;

  FOR(i, NBLOCK) {
    auto x = s.history[i];
    FOR(j, BLOCK_SIZE) {
      i64 nr = x%8<4?3:1;
      FOR(k, nr) cout << dc[x%4];
      x = random_next(x, c, m);
    }
  }

  return false;
}
 
int main(int argc, char** argv) {
  runtime_assert(argc >= 2);
  i64 id = atoi(argv[1]);
  runtime_assert(1 <= id && id <= 21);

  problem pb; pb.load(id);

  test(pb,3903689,3);
  
//   { i64 m = 1'078'074'896;
//     while(1) {
//       if(!isprime(m)) { m -= 1; continue; }
//       debug(m, best, pb.sz);
// #pragma omp parallel for
//       FORU(c, 2, (B-1)) {
//         if(test(pb,m,c)) {
//           exit(0);
//         }
//       }
//       m -= 1;
//     }
//   }
  
  return 0;
}

