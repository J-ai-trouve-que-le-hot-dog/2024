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

ostream &operator<<(ostream &os, pt const &p) { return os << mt(p.x, p.y); }

struct problem {
  i32 id;
  i32 n;
  vector<pt> A;

  void read(i32 id_) {
    id = id_;
    ifstream is("inputs/spaceship" + to_string(id));
    runtime_assert(is.good());

    A.clear();
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

const string dc[3] =
  { "123",
    "456",
    "789"
  };


int main(int argc, char** argv) {
  runtime_assert(argc >= 2);
  i64 id = atoi(argv[1]);
  runtime_assert(1 <= id && id <= 25);

  problem pb; pb.read(id);

  i32 x = 0, y = 0;
  i32 vx = 0, vy = 0;

  string ans;

  vector<i32> done(pb.n, 0);
  auto bt = letrec([&](auto bt, i32 i) -> void {
    if(i == pb.n) {
      pb.save(ans);
      debug("OK");
      exit(0);
      return;
    }

    FOR(j, pb.n) if(!done[j]) {
      FOR(dx,3) FOR(dy,3) {
        i32 x2 = x + vx + dx-1;
        i32 y2 = y + vy + dy-1;
        if(pb.A[j] == pt(x2,y2)) {
          vx += dx-1; vy += dy-1;
          x += vx; y += vy;
          ans += dc[dy][dx];
          done[j] = 1;
          bt(i+1);
          done[j] = 0;
          ans.pop_back();
          x -= vx; y -= vy;         
          vx -= dx-1; vy -= dy-1;
        }
      }
    }
    debug("back", i);
  });
  
  bt(0);
  
  // FOR(i, pb.n) {
  //   FOR(j, pb.n) if(!done[j]) {
  //     FOR(dx,3) FOR(dy,3) {
  //       i32 x2 = x + vx + dx-1;
  //       i32 y2 = y + vy + dy-1;
  //       if(pb.A[j] == pt(x2,y2)) {
  //         debug("OK", i, j, dx, dy, pt(x2,y2));
  //       }
  //     }
  //   }
        
  //   FOR(j, pb.n) if(!done[j]) {
  //     FOR(dx,3) FOR(dy,3) {
  //       i32 x2 = x + vx + dx-1;
  //       i32 y2 = y + vy + dy-1;
  //       if(pb.A[j] == pt(x2,y2)) {
  //         x = x2; y = y2;
  //         vx += dx-1; vy += dy-1;
  //         ans += dc[dy][dx];
  //         done[j] = 1;
  //         goto l_end;
  //       }
  //     }
  //   }
  //   debug("bad");
  //   return 0;
  // l_end:;
  // }

  // pb.save(ans);
  
  return 0;
}
