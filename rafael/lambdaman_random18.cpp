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

void step(problem const& pb, i64& position, i64& nvisited, i64* visited, i64 d){
  i64 nr = 1;
  FOR(k, nr) {
    i64 nposition = pb.graph[position][d];
    if(nposition != -1) {
      position = nposition;
      if(!visited[position]) {
        visited[position] = 1;
        nvisited += 1;
      }
    }
  }
}

bool test(problem const& pb, i64 m, i64 c, i64 x0) {
  date += 1;
  i64 x = x0;
  runtime_assert(x0 < B);
  i64 sz = 0;
  while(sz < 500'000) {
    xs[sz] = x;
    // last[x] = date;
    sz += 1;
    x = random_next(x,c,m);
    // if(last[x] == date) break;
  }

  while(xs[sz-1] >= B) sz -= 1;
  i64 x1 = xs[sz-1];

  FOR(i, pb.sz) visited[i] = 0;
  i64 nvisited = 0;

  i64 position = pb.start;
  FORD(i, sz-2, 0) {
    step(pb, position, nvisited, visited, xs[i]%4);
  }

  i64 value = 0;
 
  {
    auto dfs = letrec([&](auto dfs, i64 i, i64 p) -> bool {
      bool r = !visited[i];
      for(auto j : pb.T[i]) if(j != p) {
          if(dfs(j,i)) r = 1;
        }
      if(r) value += 1;
      return r;
    });
    
    FOR(i, pb.T.size()) if(!visited[i]) {
      dfs(i, -1);
      break;
    }
  }

  if(value > 2000) return false;

#pragma omp critical
  {
    debug(value);
  }
  
  FORU(y0, 1, (B-1)) if(y0 != x0) {
    runtime_assert(y0 < B);

    FOR(j, pb.sz) visited2[j] = visited[j];
    i64 position2 = position;
    i64 nvisited2 = nvisited;

    date += 1;
    i64 y = y0;
    i64 sz2 = 0;
    while(sz2 < 500'000) {
      xs[sz2] = y;
      // last[y] = date;
      sz2 += 1;
      y = random_next(y,c,m);
      // if(last[y] == date) break;
    }
    
    while(sz2 - 1 >= 0 && xs[sz2-1] >= B) {
      sz2 -= 1;
    }
    if(sz2 == 0) continue;
    i64 y1 = xs[sz2-1];

    FORD(i, sz2-2, 0) {
      step(pb, position2, nvisited2, visited2, xs[i]%4);
    }

    if(nvisited2 >= 9900) {
#pragma omp critical
      {
        debug(m, c, x0, xs[sz-1], nvisited2);
      }
    }
  
    if(nvisited2 > best) {
#pragma omp critical
      {
        if(nvisited2 > best) {
          best = nvisited2;
        }
      }
    }
  
    if(nvisited2 == pb.sz) {
#pragma omp critical
      {
        
        cout << "Found: " 
             << "m = " << m << ", "
             << "c = " << c << ", "
             << "start1 = " << x0 << ", "
             << "stop1 = " << x1 << ", "
             << "start2 = " << y0 << ", "
             << "stop2 = " << y1 << " " << B*B << ", "
             << endl;
        cout << c << " " << m << " " << x0 << " " << x1 << " " << y0 << " " << y1 << endl;
      }
      return true;
    }
  }

  return false;
  
//   if(nvisited >= 8100) {
//     #pragma omp critical
//     {
//       debug(m, c, x0, xs[sz-1], nvisited, total);
//     }
//   }
  
//   if(nvisited > best) {
// #pragma omp critical
//     {
//       if(nvisited > best) {
//         best = nvisited;
//       }
//     }
//   }
  
//   if(nvisited == pb.sz) {
// #pragma omp critical
//     {
//       cout << "Found: " 
//            << "m = " << m << ", "
//            << "c = " << c << ", "
//            << "start = " << x0 << ", "
//            << "stop = " << xs[sz-1] << ", "
//            << endl;
//     }
//     return true;
//   }else{
//     return false;
//   }
}

int main(int argc, char** argv) {
  runtime_assert(argc >= 2);
  i64 id = atoi(argv[1]);
  runtime_assert(1 <= id && id <= 21);

  problem pb; pb.load(id);
  
  { i64 m = 78'074'896 / 20;
    while(1) {
      if(!isprime(m)) { m -= 1; continue; }
      debug(m, best, pb.sz);
#pragma omp parallel for collapse(2) schedule(dynamic)
      FORU(c, 2, (B-1)) {
        FORU(start1, 1, (B-1)) {
          if(test(pb,m,c,start1)) {
            exit(0);
          }
        }
      }
      m -= 1;
    }
  }
  
  return 0;
}

