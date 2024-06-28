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

const i32 MAXD = 10'000;
const i32 MAXV = 80;

uint64_t pre[MAXD+1][2*MAXV+1][2*MAXV+1];

i64 cost(i32 dx, i32 dy, i32 vx0, i32 vy0, i32 vx1, i32 vy1) {
  if(dx < 0) { dx = -dx; vx0 = 2*MAXV-vx0; vx1 = 2*MAXV-vx1; }
  if(dy < 0) { dy = -dy; vy0 = 2*MAXV-vy0; vy1 = 2*MAXV-vy1; }
  if(dx > MAXD || dy > MAXD) return 1'000'000ull * (dx + dy);
  auto m1 = pre[dx][vx0][vx1];
  auto m2 = pre[dy][vy0][vy1];
  auto m = (m1 & m2);
  if(m == 0) return 1'000'000ull * (dx + dy);
  return __builtin_ctzll(m);
}

i64 cost(i32 i, pt si, i32 j, pt sj) {
  if(j == n) return 0;
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
    perm = {0,n};
    FORU(i, 1, n-1) {
      i32 bj = 0;
      i64 bc = 1ull<<60;
      FOR(j, (i32)perm.size()-1) {
        i64 c =
          cost(perm[j],pt(MAXV,MAXV), i,pt(MAXV,MAXV)) +
          cost(i,pt(MAXV,MAXV), perm[j+1],pt(MAXV,MAXV));
        if(c < bc) {
          bc = c;
          bj = j;
        }
      }
      perm.insert(begin(perm)+bj, i);
    }
    speed.assign(n+1, pt(MAXV, MAXV));
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
  
  const i64 MAX_ITER = 2'000'000'000;
  
  i64 niter = 0;
  f64 done  = 0;
  f64 temp0 = 8.0;
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

    auto get_new_speed = [&](pt s) {
      if(rng.random32(3) == 0) {
        return pt(rng.random32(2*MAXV+1), rng.random32(2*MAXV+1));
      }else{
        auto ns = s;
        ns.x += rng.random32(3)-1;
        ns.y += rng.random32(3)-1;
        if(ns.x < 0 || ns.x >= 2*MAXV+1) ns.x = s.x;
        if(ns.y < 0 || ns.y >= 2*MAXV+1) ns.y = s.y;
        return ns;
      }
    };

    i32 ty = rng.random32(3);
    if(ty == 0) {
      i32 i = rng.random32(n);
      i32 j = rng.random32(n);
      i32 k = rng.random32(n);
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

      // auto nsa2 = get_new_speed(sa2);
      // auto nsb1 = get_new_speed(sb1);
      // auto nsb2 = get_new_speed(sb2);
      // auto nsc1 = get_new_speed(sc1);
      // if(b1 == a2) nsb1 = nsa2;
      // if(c1 == b2) nsc1 = nsb2;
      
      // TODO: change speeds of a1,a2,b1,b2,c1,c2
      // careful: it is possible that b1 = a2, c1 = a2
      // TODO: allow moving the last item (use a dummy c2)
      
      i64 delta = 0;
      // if(i > 0) {
      //   auto x = S.perm[i-1];
      //   delta -= cost(x,S.speed[x], a2,);
      // }
      delta -= cost(a1,sa1, a2,sa2);
      delta -= cost(b1,sb1, b2,sb2);
      delta -= cost(c1,sc1, c2,sc2);
      delta += cost(a1,sa1, b2,sb2);
      delta += cost(b1,sb1, c2,sc2);
      delta += cost(c1,sc1, a2,sa2);

      if(accept(delta)) {
        S.score += delta;
        rotate(begin(S.perm)+i+1, begin(S.perm)+j+1, begin(S.perm)+k+1);
        // S.speed[a2] = nsa2;
        // S.speed[b1] = nsb1;
        // S.speed[b2] = nsb2;
        // S.speed[c1] = nsc1;
      }
    }else if(ty == 1) {
      i32 i = 1+rng.random32(n-1);
      i32 j = rng.random32(n);
      if(j == i || j+1 == i) {
        continue;
      }

      i32 b = S.perm[i];
      auto sb = S.speed[b];
      pt nsb = get_new_speed(sb);

      i64 delta = 0;

      { i32 a = S.perm[i-1];
        auto sa = S.speed[a];
        delta -= cost(a,sa, b,sb);
        
        i32 c = S.perm[i+1];
        auto sc = S.speed[c];
        delta -= cost(b,sb, c,sc);

        delta += cost(a,sa, c,sc);
      }
      
      { i32 a = S.perm[j];
        auto sa = S.speed[a];
        delta += cost(a,sa, b,nsb);

        i32 c = S.perm[j+1];
        auto sc = S.speed[c];
        delta += cost(b,nsb, c,sc);

        delta -= cost(a,sa, c,sc);
      }

      if(accept(delta)) {
        S.score += delta;
        S.perm.erase(begin(S.perm)+i);
        if(i < j) j -= 1;
        S.perm.insert(begin(S.perm)+j+1, b);
        S.speed[b] = nsb;
      }
    }else if(ty == 2){
      i32 i = 1+rng.random32(n-1);

      i32 b = S.perm[i];
      auto sb = S.speed[b];
      pt nsb = get_new_speed(sb);

      i64 delta = 0;

      i32 a = S.perm[i-1];
      auto sa = S.speed[a];
      delta -= cost(a,sa, b,sb);
      delta += cost(a,sa, b,nsb);

      i32 c = S.perm[i+1];
      auto sc = S.speed[c];
      delta -= cost(b,sb, c,sc);
      delta += cost(b,nsb, c,sc);
    
      if(accept(delta)) {
        S.score += delta;
        S.speed[b] = nsb;
      }
    }

    if(S.score < best_score) {
      best_score = S.score;
      best_state = S;
    }
  }

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
