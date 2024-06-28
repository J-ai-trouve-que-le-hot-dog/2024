#include "header.hpp"
#include <filesystem>

using f64 = double;
using f32 = float;

struct pt {
  i32 x,y;

  pt() : x(0), y(0) { }

  pt(i32 x_, i32 y_) : x(x_), y(y_) { }
};

ostream& operator<<(ostream& os, pt const &p) {
  return os << mt(p.x,p.y);
}

vector<pt> A;
i32 n;

const i32 MAXD = 25'000;
const i32 MAXV = 50;

uint64_t pre[MAXD+1][2*MAXV+1][2*MAXV+1];

i32 cost(i32 dx, i32 dy, i32 vx0, i32 vy0, i32 vx1, i32 vy1) {
  if(dx < 0) { dx = -dx; vx0 = 2*MAXV-vx0; vx1 = 2*MAXV-vx1; }
  if(dy < 0) { dy = -dy; vy0 = 2*MAXV-vy0; vy1 = 2*MAXV-vy1; }
  if(dx > MAXD || dy > MAXD) return 1'000'000;
  auto m1 = pre[dx][vx0][vx1];
  auto m2 = pre[dy][vy0][vy1];
  auto m = (m1 & m2);
  if(m == 0) return 1'000'000;
  return __builtin_ctzll(m);
}

i32 cost(i32 i, pt si, i32 j, pt sj) {
  i32 dx = A[j].x-A[i].x;
  i32 dy = A[j].y-A[i].y;
  return cost(dx,dy,si.x,si.y,sj.x,sj.y);
}

const string dc[3] =
  { "123",
    "456",
    "789"
  };

void reconstruct(string& out,
                 i32 dx, i32 dy,
                 i32 vx0, i32 vy0,
                 i32 vx1, i32 vy1,
                 bool flipx, bool flipy) {
  if(dx == 0 && dy == 0 && vx0 == vx1 && vy0 == vy1) {
    return;
  }
  if(dx < 0) { dx = -dx; vx0 = 2*MAXV-vx0; vx1 = 2*MAXV-vx1; flipx ^= 1; }
  if(dy < 0) { dy = -dy; vy0 = 2*MAXV-vy0; vy1 = 2*MAXV-vy1; flipy ^= 1; }
  if(dx > MAXD || dy > MAXD) runtime_assert(false);
  auto m1 = pre[dx][vx0][vx1];
  auto m2 = pre[dy][vy0][vy1];
  auto m = (m1 & m2);
  runtime_assert(m != 0);
  auto c = __builtin_ctzll(m);
  runtime_assert(c == cost(dx,dy,vx0,vy0,vx1,vy1));
  FOR(dvx, 3) FOR(dvy, 3) {
    i32 nvx0 = vx0 + (dvx-1);
    if(nvx0 < 0 || nvx0 >= 2*MAXV+1) continue;
    i32 nvy0 = vy0 + (dvy-1);
    if(nvy0 < 0 || nvy0 >= 2*MAXV+1) continue;
    i32 ndx = dx - (nvx0-MAXV);
    i32 ndy = dy - (nvy0-MAXV);
    i32 nvx1 = vx1, nvy1 = vy1;
    bool nflipx = flipx;
    bool nflipy = flipy;
    if(ndx < 0) {
      ndx = -ndx;
      nvx0 = 2*MAXV - nvx0;
      nvx1 = 2*MAXV - nvx1;
      nflipx ^= 1;
    }
    if(ndy < 0) {
      ndy = -ndy;
      nvy0 = 2*MAXV - nvy0;
      nvy1 = 2*MAXV - nvy1;
      nflipy ^= 1;
    }
    if(c == 1 + cost(ndx,ndy,nvx0,nvy0,nvx1,nvy1)){
      out += dc[flipy?2-dvy:dvy][flipx?2-dvx:dvx];
      reconstruct(out,ndx,ndy,nvx0,nvy0,nvx1,nvy1,nflipx,nflipy);
      return;
    }
  }
  runtime_assert(false);
}

void reconstruct(string& out, i32 i, pt si, i32 j, pt sj) {
  i32 dx = A[j].x-A[i].x;
  i32 dy = A[j].y-A[i].y;
  reconstruct(out,dx,dy,si.x,si.y,sj.x,sj.y,0,0);
}


struct state {
  vector<i32> perm;
  vector<pt>  speed;
  i64         score;

  void reset() {
    perm.resize(n); iota(all(perm),0);
    rng.shuffle(perm.data()+1, perm.data()+n);
    speed.assign(n, pt(MAXV, MAXV));
    score = calc_score();
  }

  void reconstruct(string& out) const {
    FOR(i, n-1) {
      auto a = perm[i], b = perm[i+1];
      auto sa = speed[a], sb = speed[b];
      ::reconstruct(out,a,sa,b,sb);
    }
  }
  
  i64 calc_score() const {
    i64 score = 0;
    FOR(i, n-1) {
      auto a = perm[i], b = perm[i+1];
      auto sa = speed[a], sb = speed[b];
      score += cost(a,sa,b,sb);
    }
    return score;
  }

  void check_score() const {
    runtime_assert(score == calc_score());
  }
};


int main(int argc, char** argv) {
  runtime_assert(argc == 2);
  i32 id = atoi(argv[1]);
  runtime_assert(1 <= id && id <= 25);
  
  ifstream is("inputs/spaceship" + to_string(id));
  runtime_assert(is.good());
  A.eb(0,0);
  { string line;
    while(getline(is,line)) {
      if(line.empty()) break;
      istringstream iline(line);
      i32 x,y; iline >> x >> y;
      A.eb(x,y);
    }
  }

  n = A.size();
  debug(n);

  FOR(dx, MAXD+1) FOR(v0, 2*MAXV+1) FOR(v1, 2*MAXV+1) {
    pre[dx][v0][v1] = 0;
  }

  FOR(v, 2*MAXV+1) {
    pre[0][v][v] = 1;
  }

  FOR(i, 63) {
    cerr << "Precomputation: " << i << "/63" << endl;
    FOR(dx, MAXD+1) FOR(v0, 2*MAXV+1) FOR(v1, 2*MAXV+1) {
      FOR(e, 3) {
        i32 nv0 = v0 + (e-1);
        if(nv0 < 0 || nv0 >= 2*MAXV+1) continue; 
        i32 ndx = dx - (nv0 - MAXV);
        i32 nv1 = v1;
        if(ndx < 0) {
          ndx = -ndx;
          nv0 = 2*MAXV - nv0;
          nv1 = 2*MAXV - nv1;
        }
        if(ndx > MAXD) continue;
        if(pre[ndx][nv0][nv1] & (1ull<<i)) {
          pre[dx][v0][v1] |= (1ull<<(i+1));
        }
      }
    }
  }
  
  const i64 MAX_ITER = 5'000'000'000;
  
  i64 niter = 0;
  f64 done  = 0;
  f64 temp0 = 16.0;
  f64 temp  = temp0;

  state S; S.reset();
  debug(S.score);

  i64 best_score = S.score;
  auto best_state = S;
  
  while(1) {
    niter += 1;
    if(niter % 1024 == 0) {
      done = (f64) niter / MAX_ITER;
      if(done > 1.0) break;
      temp = temp0 * (1-done);
    }
    if(niter % (1<<20) == 0) {
      cerr
        << "niter = " << setw(12) << niter << ", "
        << "score = " << setw(12) << S.score << ", "
        << "best_score = " << setw(12) << best_score << ", "
        << endl;
    }

    auto accept = [&](i64 delta) -> bool {
      if(delta <= 0) return true;
      return delta <= temp * rng.randomDouble();
    };

    i32 ty = rng.random32(2);
    if(ty == 0) {
      i32 i = rng.random32(n-1);
      i32 j = rng.random32(n-1);
      i32 k = rng.random32(n-1);
      if(i > j) swap(i,j);
      if(j > k) swap(j,k);
      if(i > j) swap(i,j);
      if(i == j || j == k) continue;
      auto a1 = S.perm[i], a2 = S.perm[i+1];
      auto sa1 = S.speed[a1], sa2 = S.speed[a2];
      auto b1 = S.perm[j], b2 = S.perm[j+1];
      auto sb1 = S.speed[b1], sb2 = S.speed[b2];
      auto c1 = S.perm[k], c2 = S.perm[k+1];
      auto sc1 = S.speed[c1], sc2 = S.speed[c2];

      // TODO: change speeds of a1,a2,b1,b2,c1,c2
      // careful: it is possible that b1 = a2, c1 = a2
      // TODO: allow moving the last item (use a dummy c2)
      
      i64 delta = 0;
      delta -= cost(a1,sa1, a2,sa2);
      delta -= cost(b1,sb1, b2,sb2);
      delta -= cost(c1,sc1, c2,sc2);
      delta += cost(a1,sa1, b2,sb2);
      delta += cost(b1,sb1, c2,sc2);
      delta += cost(c1,sc1, a2,sa2);

      if(accept(delta)) {
        S.score += delta;
        rotate(begin(S.perm)+i+1, begin(S.perm)+j+1, begin(S.perm)+k+1);
      }
    }else if(ty == 1){
      i32 i = rng.random32(n);
      if(i == 0) continue;

      i32 b = S.perm[i];
      auto sb = S.speed[b];
      pt nsb;
      if(rng.random32(2)) {
        nsb = pt(rng.random32(2*MAXV+1), rng.random32(2*MAXV+1));
      }else{
        nsb = sb;
        nsb.x += rng.random32(3)-1;
        nsb.y += rng.random32(3)-1;
        if(nsb.x < 0 || nsb.x >= 2*MAXV+1) continue; 
        if(nsb.y < 0 || nsb.y >= 2*MAXV+1) continue; 
      }

      i64 delta = 0;

      i32 a = S.perm[i-1];
      auto sa = S.speed[a];
      delta -= cost(a,sa, b,sb);
      delta += cost(a,sa, b,nsb);

      if(i+1 < n) {
        i32 c = S.perm[i+1];
        auto sc = S.speed[c];
        delta -= cost(b,sb, c,sc);
        delta += cost(b,nsb, c,sc);
      }
    
      if(accept(delta)) {
        S.score += delta;
        S.speed[b] = nsb;
      }
    }

    // S.check_score();
    
    if(S.score < best_score) {
      best_score = S.score;
      best_state = S;
    }
  }

  // FOR(i, n) {
  //   debug(A[best_state.perm[i]]);
  // }

  best_state.check_score();
  
  debug(best_score);
  if(best_score < 1'000'000) {
    string out;
    best_state.reconstruct(out);
    cout << out << endl;

    string filename = "solutions/spaceship" + to_string(id);
    
    if(filesystem::exists(filename)) {
      string previous_best; 
      { ifstream file(filename);
        file >> previous_best;
      }
      if(out.size() < previous_best.size()) {
        ofstream file(filename);
        file << out;       
      }
    }else{
      ofstream file(filename);
      file << out;
    }


  }
  
  return 0;
}
