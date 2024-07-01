#include "lambdaman.hpp"
#include <cstdio>
const i64 MAXN = 100'000;

const i64 B = 94;
const i64 MAXM = 1'000'000;

i64 perm[24][4];

i64 random_next(i64 x, i64 c, i64 m) { return (x * c) % m; }

thread_local i64 xs[MAXM];
thread_local i64 last[MAXM];

thread_local i64 visited[MAXN];
thread_local i64 date = 0;

bool isprime(i64 m) {
  FORU(i, 2, m) {
    if(i*i > m) break;
    if(m%i==0) return 0;
  }
  return 1;
}

i64 best = 0;

bool test(problem const& pb, i64 permi, i64 m, i64 c, i64 x0) {
  date += 1;
  i64 x = x0;
  i64 sz = 0;
  while(1) {
    if(sz == 1'000'000) break;
    xs[sz] = x;
    last[x] = date;
    sz += 1;
    x = random_next(x,c,m);
    if(last[x] == date) break;
  }

  while(xs[sz-1] >= B*B) sz -= 1;
  FOR(i, pb.sz) visited[i] = 0;
  i64 position = pb.start;
  i64 nvisited = 0;

  FORD(i, sz-2, 0) {
    i64 d = perm[permi][xs[i]%4];
    i64 nposition = pb.graph[position][d];
    if(nposition != -1) {
      position = nposition;
      if(!visited[position]) {
        visited[position] = 1;
        nvisited += 1;
      }
    }
  }

  if(nvisited >= 33580) {
    #pragma omp critical
    {
      debug(m, c, x0, xs[sz-1], nvisited);
    }
  }
  
  if(nvisited > best) {
#pragma omp critical
    {
      if(nvisited > best) {
        best = nvisited;
      }
    }
  }
  
  if(nvisited == pb.sz) {
#pragma omp critical
    {
      cout << "Found: "
           << "perm = " << perm[permi][0] << perm[permi][1]
           << perm[permi][2] << perm[permi][3] << ", "
           << "m = " << m << ", "
           << "c = " << c << ", "
           << "start = " << x0 << ", "
           << "stop = " << xs[sz-1] << ", "
           << endl;
    }
    return true;
  }else{
    return false;
  }
}

int main(int argc, char** argv) {
  runtime_assert(argc >= 2);
  i64 id = atoi(argv[1]);
  runtime_assert(1 <= id && id <= 21);

  { vector<i64> I(4);
    iota(all(I),0);
    FOR(i, 24) {
      FOR(j, 4) perm[i][j] = I[j];
      next_permutation(all(I));
    }
  }
      
  
  problem pb; pb.load(id);
  
  { i64 m = 820584;
    while(1) {
      if(!isprime(m)) { m -= 1; continue; }
      debug(m, best, pb.sz);
#pragma omp parallel for collapse(3)
      FOR(permi, 1) {
        FORU(c, 3, 3) {
          FORU(start, 1, B-1) {
            if(test(pb,0,m,c,start)) {
              exit(0);
            }
          }
        }
      }
      m -= 1;
    }
  }
  
  return 0;
}
