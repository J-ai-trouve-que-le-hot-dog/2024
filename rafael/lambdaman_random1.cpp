#include "lambdaman.hpp"
#include <cstdio>
const i32 MAXN = 100'000;

const i32 B = 94;
const i32 MAXM = 2'000'000;

i32 perm[24][4];

i32 random_next(i32 x, i32 c, i32 m) { return (x * c) % m; }

thread_local i32 xs[MAXM];
thread_local i32 ds[MAXM];
thread_local i64 last[MAXM];

thread_local i32 visited[MAXN];
thread_local i64 date = 0;

bool isprime(i32 m) {
  FORU(i, 2, m) {
    if(i*i > m) break;
    if(m%i==0) return 0;
  }
  return 1;
}

i32 best = 0;

bool test(problem const& pb, i32 permi, i32 m, i32 c, i32 x0) {
  date += 1;
  i32 x = x0;
  i32 sz = 0;
  while(1) {
    xs[sz] = x;
    last[x] = date;
    sz += 1;
    x = random_next(x,c,m);
    if(last[x] == date) break;
  }

  while(xs[sz-1] >= B) sz -= 1;
  FOR(i, pb.sz) visited[i] = 0;
  i32 position = pb.start;
  i32 nvisited = 0;

  FORD(i, sz-2, 0) {
    i32 d = perm[permi][xs[i]%4];
    i64 nposition = pb.graph[position][d];
    if(nposition != -1) {
      position = nposition;
      if(!visited[position]) {
        visited[position] = 1;
        nvisited += 1;
      }
    }
  }

  if(nvisited >= 2000) {
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
  i32 id = atoi(argv[1]);
  runtime_assert(1 <= id && id <= 21);

  { vector<i32> I(4);
    iota(all(I),0);
    FOR(i, 24) {
      FOR(j, 4) perm[i][j] = I[j];
      next_permutation(all(I));
    }
  }
      
  
  problem pb; pb.load(id);
  
  { i32 m = 8836;
    while(1) {
      if(!isprime(m)) { m -= 1; continue; }
      debug(m, best, pb.sz);
#pragma omp parallel for collapse(3)
      FOR(permi, 24) {
        FORU(c, 2, B-1) {
          FORU(start, 1, B-1) {
            if(test(pb,permi,m,c,start)) {
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
