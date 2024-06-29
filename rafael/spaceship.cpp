#include "header.hpp"
#include <filesystem>
#include <numeric>

#include <sys/mman.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

using f64 = double;
using f32 = float;

struct pt {
  i64 x,y;

  pt() : x(0), y(0) { }

  pt(i64 x_, i64 y_) : x(x_), y(y_) { }

  i64 dist2() const { return x*x+y*y; }
  
  bool operator==(pt const& o) const { return tie(x,y) == tie(o.x,o.y); }
  bool operator<(pt const& o) const { return tie(x,y) < tie(o.x,o.y); }
};

ostream& operator<<(ostream& os, pt const &p) {
  return os << mt(p.x,p.y);
}

const i64 MAXD = 20'000;
const i64 MAXV = 300;

using pre_inner = array<array<u64,2*MAXV+1>,2*MAXV+1>;
pre_inner *pre = 0;

void do_precomputation() {
  if(!filesystem::exists("rafael/pre_table")){
    pre = new pre_inner[MAXD+1];

    FOR(dx, MAXD+1) FOR(v0, 2*MAXV+1) FOR(v1, 2*MAXV+1) {
      pre[dx][v0][v1] = 0;
    }

    FOR(v, 2*MAXV+1) {
      pre[0][v][v] = 1;
    }
    
    FOR(i, 63) { 
      cerr << "Precomputation: " << i << "/63" << endl;
#pragma omp parallel for
      FOR(dx, MAXD+1) {
        FOR(v0, 2*MAXV+1) FOR(v1, 2*MAXV+1) {
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
              break;
            }
          }
        }
      }
    }
    
    ofstream fout("rafael/pre_table", ios::out | ios::binary);
    fout.write((char const*)pre, sizeof(pre_inner)*(MAXD+1));
    fout.close();
    delete[] pre;
  }

  int fd = open("rafael/pre_table", O_RDONLY);
  pre = (pre_inner*)(char*)mmap(nullptr, sizeof(pre_inner)*(MAXD+1),
                                PROT_READ, MAP_SHARED,
                                fd, 0);
  if(pre == MAP_FAILED) {
    perror("mmap failed");
    exit(1);
  }
  close(fd);
  
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

  i64 cost(i64 i, pt si, i64 j, pt sj) const {
    if(j == n) return 0;
    i64 dx = A[j].x-A[i].x;
    i64 dy = A[j].y-A[i].y;
    return ::cost(dx,dy,si.x,si.y,sj.x,sj.y);
  }

  void reconstruct(string& out, i64 i, pt si, i64 j, pt sj) const {
    i64 dx = A[j].x-A[i].x;
    i64 dy = A[j].y-A[i].y;
    ::reconstruct(out,dx,dy,si.x,si.y,sj.x,sj.y,0,0);
  }

  void check_solution(string s) const {
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

struct state {
  vector<i64> perm;
  vector<pt>  speed;
  i64         score;

  void reset(problem const& pb) {
    speed.assign(pb.n+1, pt(MAXV, MAXV));

    vector<i32> points;
    FORU(i, 1, pb.n-1) points.eb(i);
    sort(all(points), [&](i32 i, i32 j) {
      return pb.A[i].dist2() < pb.A[j].dist2();
    });
    
    perm = {0, pb.n};
    FOR(i0, points.size()) {
      if(i0 % 100 == 0) debug(i0, points.size());
      auto i = points[i0];
      i64 bj = 0; 
      i64 bc = 1ull<<60;
      pt  bs = pt(MAXV,MAXV);
#pragma omp parallel for
      FOR(j, (i64)perm.size()-1) {
        FORU(sx, MAXV, MAXV) FORU(sy, MAXV, MAXV) {
          auto si = pt(sx,sy);
          i64 c =
            pb.cost(perm[j],speed[perm[j]], i,si) +
            pb.cost(i,si, perm[j+1],speed[perm[j+1]]) -
            pb.cost(perm[j],speed[perm[j]], perm[j+1],speed[perm[j+1]]);
          if(c < bc) {
#pragma omp critical
            {
              if(c < bc) {
                bc = c;
                bj = j;
                bs = si;
              }
            }
          }
        }
      }
      perm.insert(begin(perm)+bj+1, i);
      speed[i] = bs;
    }
    
    score = calc_score(pb);
  }

  void reconstruct(problem const& pb, string& out) const {
    FOR(i, pb.n-1) {
      auto a = perm[i], b = perm[i+1];
      auto sa = speed[a], sb = speed[b];
      pb.reconstruct(out,a,sa,b,sb);
    }
  }
  
  i64 calc_score(problem const& pb) const {
    i64 score = 0;
    FOR(i, pb.n) {
      auto a = perm[i], b = perm[i+1];
      auto sa = speed[a], sb = speed[b];
      score += pb.cost(a,sa,b,sb);
    }
    return score;
  }

  void check_score(problem const& pb) const {
    runtime_assert(score == calc_score(pb));
  }

  void solve_speeds(problem const& pb) {
    const i32 width = 128;
    cerr << "Solving speeds, width = " << width << endl;
    struct beam_entry {
      i64 score;
      pt  speed;
      i32 from;
    };
    vector<vector<beam_entry>> beam(pb.n);
    beam[0].eb(beam_entry {
        .score = 0,
        .speed = pt(MAXV,MAXV),
        .from = -1
      });
    FOR(i, pb.n-1) {
      if(i % 1000 == 0) cerr << i << "/" << pb.n << endl;
      auto a = perm[i], b = perm[i+1];
      FORU(vx, speed[a].x - 32, speed[a].x + 32) {
        if(vx < 0 || vx >= 2*MAXV) continue;
        FORU(vy, speed[a].y - 32, speed[a].y + 32) {
          if(vy < 0 || vy >= 2*MAXV) continue;
          i64 score = 1ll<<60;
          pt  speed;
          i32 from = -1;
          FOR(j, beam[i].size()) {
            i64 cur_score =
              beam[i][j].score +
              pb.cost(a,beam[i][j].speed, b,pt(vx,vy));
            if(cur_score < score) {
              score = cur_score;
              speed = pt(vx,vy);
              from = j;
            }
          }
          beam[i+1].eb(beam_entry {
              .score = score,
              .speed = speed,
              .from  = from,
            });
        }
        sort(all(beam[i+1]), [&](auto const& a, auto const& b) { return a.score < b.score; });
        if(beam[i+1].size() > width) {
          beam[i+1].resize(width);
        }
      }
    }
    debug(beam[pb.n-1][0].score);
    if(beam[pb.n-1][0].score < score) {
      i32 j = 0;
      FORD(i, pb.n-1, 0) {
        speed[perm[i]] = beam[i][j].speed;
        j = beam[i][j].from;
      }
    }
    score = calc_score(pb);
    debug(score);
  }
};



int main(int argc, char** argv) {
  runtime_assert(argc >= 2);
  i64 id = atoi(argv[1]);
  runtime_assert(1 <= id && id <= 25);
  do_precomputation();

  problem pb; pb.read(id);

  state S; S.reset(pb);
  debug(S.score);
  S.solve_speeds(pb);
 
  i64 niter = 0;
  f64 done  = 0;
  f64 temp0 = 0.1;
  f64 temp1 = 0.05;
  f64 temp  = temp0;

  i64 last_improvement = 0;
  
  i64 best_score = S.score;
  auto best_state = S;

  const i32 MAX_ITER = 100'000'000;
  
  while(1) {
    niter += 1;
    if(niter % 1024 == 0) {
      done = (f64) niter / MAX_ITER;
      done = min(done, 1.0);
      temp = temp0 * pow(temp1 / temp0, done);
    }
    if(niter % (1<<20) == 0) {
#pragma omp critical
      {
        cerr
          << "id = " << setw(2) << id << ", "
          << "niter = " << setw(12) << niter << ", "
          << "done = " << fixed << setprecision(6) << done << ", "
          << "temp = " << fixed << setprecision(6) << temp << ", "
          << "score = " << setw(9) << S.score << ", "
          << "best_score = " << setw(9) << best_score << ", "
          << endl;
      }
      string out;
      best_state.reconstruct(pb,out);
      pb.save(out);
    }

    auto accept = [&](i64 delta) -> bool {
      if(delta <= 0) return true;
      return delta <= temp * rng.randomDouble();
    };

    auto get_new_speed = [&](pt s) {
      if(rng.random32(30) == 0) {
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

    i64 ty = rng.random32(3);
    if(ty == 0) {
      i64 i = rng.random32(pb.n);
      i64 j = rng.random32(pb.n);
      i64 k = rng.random32(pb.n);
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

      i32 change_speeds = rng.random32(2) == 0;
      
      auto nsa2 = change_speeds ? get_new_speed(sa2) : sa2;
      auto nsb1 = change_speeds ? get_new_speed(sb1) : sb1;
      auto nsb2 = change_speeds ? get_new_speed(sb2) : sb2;
      auto nsc1 = change_speeds ? get_new_speed(sc1) : sc1;
      if(b1 == a2) nsb1 = nsa2;
      if(c1 == b2) nsc1 = nsb2;

      // TODO: change speeds of a1,a2,b1,b2,c1,c2
      // careful: it is possible that b1 = a2, c1 = a2
      // TODO: allow moving the last item (use a dummy c2)
      
      i64 delta = 0;
      delta -= pb.cost(a1,sa1, a2,sa2);
      delta -= pb.cost(b1,sb1, b2,sb2);
      delta -= pb.cost(c1,sc1, c2,sc2);
      delta += pb.cost(a1,sa1, b2,nsb2);
      delta += pb.cost(b1,nsb1, c2,sc2);
      delta += pb.cost(c1,nsc1, a2,nsa2);
      if(i+2 == j) {
        delta -= pb.cost(a2,sa2, b1,sb1);
        delta += pb.cost(a2,nsa2, b1,nsb1);
      }else if(i+2 < j) {
        { auto x = S.perm[i+2];
          delta -= pb.cost(a2,sa2, x,S.speed[x]);
          delta += pb.cost(a2,nsa2, x,S.speed[x]);
        }
        { auto x = S.perm[j-1];
          delta -= pb.cost(x,S.speed[x], b1,sb1);
          delta += pb.cost(x,S.speed[x], b1,nsb1);
        }
      }
      if(j+2 == k) {
        delta -= pb.cost(b2,sb2, c1,sc1);
        delta += pb.cost(b2,nsb2, c1,nsc1);
      }else if(j+2 < k) {
        { auto x = S.perm[j+2];
          delta -= pb.cost(b2,sb2, x,S.speed[x]);
          delta += pb.cost(b2,nsb2, x,S.speed[x]);
        }
        { auto x = S.perm[k-1];
          delta -= pb.cost(x,S.speed[x], c1,sc1);
          delta += pb.cost(x,S.speed[x], c1,nsc1);
        }
      }


      if(accept(delta)) {
        S.score += delta;
        rotate(begin(S.perm)+i+1, begin(S.perm)+j+1, begin(S.perm)+k+1);
        S.speed[a2] = nsa2;
        S.speed[b1] = nsb1;
        S.speed[b2] = nsb2;
        S.speed[c1] = nsc1;
      }
    }else if(ty == 1) {
      i64 i = 1+rng.random32(pb.n-1);
      i64 j = rng.random32(pb.n);
      if(j == i || j+1 == i) {
        continue;
      }

      i64 b = S.perm[i];
      auto sb = S.speed[b];
      pt nsb = get_new_speed(sb);

      i64 delta = 0;

      { i64 a = S.perm[i-1];
        auto sa = S.speed[a];
        delta -= pb.cost(a,sa, b,sb);
        
        i64 c = S.perm[i+1];
        auto sc = S.speed[c];
        delta -= pb.cost(b,sb, c,sc);

        delta += pb.cost(a,sa, c,sc);
      }
      
      { i64 a = S.perm[j];
        auto sa = S.speed[a];
        delta += pb.cost(a,sa, b,nsb);

        i64 c = S.perm[j+1];
        auto sc = S.speed[c];
        delta += pb.cost(b,nsb, c,sc);

        delta -= pb.cost(a,sa, c,sc);
      }

      if(accept(delta)) {
        S.score += delta;
        S.perm.erase(begin(S.perm)+i);
        if(i < j) j -= 1;
        S.perm.insert(begin(S.perm)+j+1, b);
        S.speed[b] = nsb;
      }
    }else if(ty == 2){
      i64 i = 1+rng.random32(pb.n-1);

      i64 b = S.perm[i];
      auto sb = S.speed[b];
      pt nsb = get_new_speed(sb);

      i64 delta = 0;

      i64 a = S.perm[i-1];
      auto sa = S.speed[a];
      delta -= pb.cost(a,sa, b,sb);
      delta += pb.cost(a,sa, b,nsb);

      i64 c = S.perm[i+1];
      auto sc = S.speed[c];
      delta -= pb.cost(b,sb, c,sc);
      delta += pb.cost(b,nsb, c,sc);
    
      if(accept(delta)) {
        S.score += delta;
        S.speed[b] = nsb;
      }
    }

    if(niter % 100'000'000 == 0) {
      S.solve_speeds(pb);
    }
    
    // S.check_score();

    if(S.score < best_score) {
      last_improvement = niter;
      best_score = S.score;
      best_state = S;
    }

    if(niter > last_improvement + 300'000'000) {
      last_improvement = 0;
      niter = 0;
      S.reset(pb);
      S.solve_speeds(pb);
    }
  }
  
  return 0;
}
