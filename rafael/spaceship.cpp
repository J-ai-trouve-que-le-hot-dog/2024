#include "header.hpp"
#include <filesystem>
#include <numeric>

using f64 = double;
using f32 = float;

struct pt {
  i64 x,y;

  pt() : x(0), y(0) { }

  pt(i64 x_, i64 y_) : x(x_), y(y_) { }

  bool operator==(pt const& o) const { return tie(x,y) == tie(o.x,o.y); }
  bool operator<(pt const& o) const { return tie(x,y) < tie(o.x,o.y); }
};

ostream& operator<<(ostream& os, pt const &p) {
  return os << mt(p.x,p.y);
}

const i64 MAXD = 20'000;
const i64 MAXV = 50;

using pre_inner = array<array<u64,2*MAXV+1>,2*MAXV+1>;
pre_inner *pre = 0;

void do_precomputation() {
  pre = new pre_inner[MAXD+1];

  FOR(i, 63) { 
    cerr << "Precomputation: " << i << "/63" << endl;
    FOR(dx, MAXD+1) FOR(v0, 2*MAXV+1) FOR(v1, 2*MAXV+1) {
      FOR(e, 3) {
        i64 nv0 = v0 + (e-1);
        if(nv0 < 0 || nv0 >= 2*MAXV+1) continue; 
        i64 ndx = dx - (nv0 - MAXV);
        i64 nv1 = v1;
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
}

i64 f2(i64 x) {
  return x*(x+1)/2;
}

i64 naive_cost_1d(i64 dx, i64 vx0, i64 vx1) {
  vx0 -= MAXV;
  vx1 -= MAXV;
  i64 cost = abs(vx0)+abs(vx1);

  if(vx0 > 0) { dx -= f2(vx0-1); }
  if(vx0 < 0) { dx += f2(-vx0-1); }
  if(vx1 > 0) { dx -= f2(vx1); }
  if(vx1 < 0) { dx += f2(-vx1); }
 
  return cost + 2*sqrt(abs(dx));
}

i64 naive_cost(i64 dx, i64 dy, i64 vx0, i64 vy0, i64 vx1, i64 vy1) {
  return max(naive_cost_1d(dx,vx0,vx1),
             naive_cost_1d(dy,vy0,vy1));
}

i64 cost(i64 dx, i64 dy, i64 vx0, i64 vy0, i64 vx1, i64 vy1) {
  if(dx < 0) { dx = -dx; vx0 = 2*MAXV-vx0; vx1 = 2*MAXV-vx1; }
  if(dy < 0) { dy = -dy; vy0 = 2*MAXV-vy0; vy1 = 2*MAXV-vy1; }
  if(dx > MAXD || dy > MAXD) return naive_cost(dx,dy,vx0,vy0,vx1,vy1);
  auto m1 = pre[dx][vx0][vx1];
  auto m2 = pre[dy][vy0][vy1];
  auto m = (m1 & m2);
  if(m == 0) return naive_cost(dx,dy,vx0,vy0,vx1,vy1);
  return __builtin_ctzll(m);
}

i64 cost(i32 n, vector<pt> const& A, i64 i, pt si, i64 j, pt sj) {
  if(j == n) return 0;
  i64 dx = A[j].x-A[i].x;
  i64 dy = A[j].y-A[i].y;
  return cost(dx,dy,si.x,si.y,sj.x,sj.y);
}

const string dc[3] =
  { "123",
    "456",
    "789"
  };

vector<i64> reconstruct_naive_1d(i64 dx, i64 vx0, i64 vx1) {
  vx0 -= MAXV;
  vx1 -= MAXV;

  vector<i64> A;
  A.eb(vx0);
  while(vx0 > 0) { vx0 -= 1; A.eb(vx0); dx -= vx0; }
  while(vx0 < 0) { vx0 += 1; A.eb(vx0); dx -= vx0; }
  while(vx0 < vx1) { vx0 += 1; A.eb(vx0); dx -= vx0; }
  while(vx0 > vx1) { vx0 -= 1; A.eb(vx0); dx -= vx0; }

  while(dx != 0) {
    i64 bi = -1;
    i64 bx = 0;
    i64 bc = abs(dx);
    FOR(i, A.size()) for(auto x : {A[i]-1,A[i]+1}) {
      i64 c = abs(dx - A[i] - x);
      if(c < bc) {
        bi = i;
        bx = x;
        bc = c;
      }
    }
    runtime_assert(bi != -1);
    dx -= A[bi];
    dx -= bx;
    A.insert(begin(A)+bi+1, A[bi]);
    A.insert(begin(A)+bi+1, bx);
  }

  return A;
}

void reconstruct_naive
(string& out,
 i64 dx, i64 dy,
 i64 vx0, i64 vy0,
 i64 vx1, i64 vy1,
 bool flipx, bool flipy) {
  auto ax = reconstruct_naive_1d(dx,vx0,vx1);
  auto ay = reconstruct_naive_1d(dy,vy0,vy1);

  while(ax.size() < ay.size()) {
    i64 i = 0; while(ax[i] != 0) i += 1;
    ax.insert(begin(ax)+i+1, 0);
  }
  while(ay.size() < ax.size()) {
    i64 i = 0; while(ay[i] != 0) i += 1;
    ay.insert(begin(ay)+i+1, 0);
  }
  runtime_assert(ax.size() == ay.size());
  runtime_assert(ax.front() == vx0-MAXV);
  runtime_assert(ax.back() == vx1-MAXV);
  runtime_assert(ay.front() == vy0-MAXV);
  runtime_assert(ay.back() == vy1-MAXV);
  auto totx = accumulate(begin(ax)+1, end(ax), 0);
  auto toty = accumulate(begin(ay)+1, end(ay), 0);
  runtime_assert(totx == dx);
  runtime_assert(toty == dy);
  
  FOR(i, ax.size()-1) {
    i64 cnt = 0;
    FOR(dvx, 3) FOR(dvy, 3) if(ax[i] + (dvx-1) == ax[i+1] && ay[i] + (dvy-1) == ay[i+1]) {
      cnt += 1;
      out += dc[flipy?2-dvy:dvy][flipx?2-dvx:dvx];
    }
    runtime_assert(cnt == 1);
  }
}

void reconstruct(string& out,
                 i64 dx, i64 dy,
                 i64 vx0, i64 vy0,
                 i64 vx1, i64 vy1,
                 bool flipx, bool flipy) {
  if(dx == 0 && dy == 0 && vx0 == vx1 && vy0 == vy1) {
    return;
  }
  if(dx < 0) { dx = -dx; vx0 = 2*MAXV-vx0; vx1 = 2*MAXV-vx1; flipx ^= 1; }
  if(dy < 0) { dy = -dy; vy0 = 2*MAXV-vy0; vy1 = 2*MAXV-vy1; flipy ^= 1; }
  if(dx > MAXD || dy > MAXD) {
    reconstruct_naive(out,dx,dy,vx0,vy0,vx1,vy1,flipx,flipy);
    return;
  }
  auto m1 = pre[dx][vx0][vx1];
  auto m2 = pre[dy][vy0][vy1];
  auto m = (m1 & m2);
  if(m == 0) {
    reconstruct_naive(out,dx,dy,vx0,vy0,vx1,vy1,flipx,flipy);
    return;
  }
  auto c = __builtin_ctzll(m);
  runtime_assert(c == cost(dx,dy,vx0,vy0,vx1,vy1));
  FOR(dvx, 3) FOR(dvy, 3) {
    i64 nvx0 = vx0 + (dvx-1);
    if(nvx0 < 0 || nvx0 >= 2*MAXV+1) continue;
    i64 nvy0 = vy0 + (dvy-1);
    if(nvy0 < 0 || nvy0 >= 2*MAXV+1) continue;
    i64 ndx = dx - (nvx0-MAXV);
    i64 ndy = dy - (nvy0-MAXV);
    i64 nvx1 = vx1, nvy1 = vy1;
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

struct problem {
  i32 id;
  i32 n;
  vector<pt> A;

  void reconstruct(string& out, i64 i, pt si, i64 j, pt sj) {
    i64 dx = A[j].x-A[i].x;
    i64 dy = A[j].y-A[i].y;
    ::reconstruct(out,dx,dy,si.x,si.y,sj.x,sj.y,0,0);
  }

  void check_solution(string s) {
    set<pt> todo;
    FORU(i, 1, n-1) todo.insert(A[i]);
    i64 vx = 0, vy = 0;
    i64 x = 0, y = 0;
    for(char c : s) {
      FOR(dx, 3) FOR(dy, 3) if(dc[dy][dx] == c) {
        vx += dx-1;
        vy += dy-1;
      }
      x += vx; y += vy;
      pt p(x,y);
      todo.erase(p);
    }
    debug(todo.size());
    runtime_assert(todo.empty());
  }

  void read(i32 id_) {
    id = id_;
    ifstream is("inputs/spaceship" + to_string(id));
    runtime_assert(is.good());

    A.clear();
    A.eb(0,0);
    { string line;
      while(getline(is,line)) {
        if(line.empty()) break;
        istringstream iline(line);
        i64 x,y; iline >> x >> y;
        A.eb(x,y);
      }
    }

    n = A.size();
  }

  void save(string sol) {
    check_solution(sol);
    string filename = "solutions/spaceship" + to_string(id);
    
    if(filesystem::exists(filename)) {
      string previous_best; 
      { ifstream file(filename);
        file >> previous_best;
      }
      if(sol.size() < previous_best.size()) {
        ofstream file(filename);
        file << sol;       
      }
    }else{
      ofstream file(filename);
      file << sol;
    }
  }
};

int main(int argc, char** argv) {
  runtime_assert(argc >= 2);
  i64 id = atoi(argv[1]);
  runtime_assert(1 <= id && id <= 25);
  do_precomputation();

  problem pb; pb.read(id);
  
  
  
  return 0;
}
