#define protected public
#include "spaceship.hpp"

// inline
// i64 cost_dist(problem const& pb, i64 a, i64 b) {
//   if(b == pb.n) return 0;
//   return pb.cost(a, pt(MAXV,MAXV), b, pt(MAXV,MAXV));
// }

void solve(problem const& pb, string sol) {
  while(1) {
    state S; S.reset(pb,sol); S.solve_speeds(pb);
    vector<pt> speeds = S.speed;

    auto cost_dist = [&](i64 a, i64 b) -> i64 {
      if(b == pb.n) return 0;
      return pb.cost(a, speeds[a], b, speeds[b]);
    };
    
    vector<i64> perm;
    FOR(i, pb.n+1) perm.eb(i);

    i64 score = 0;
    FOR(i, pb.n) score += cost_dist(perm[i], perm[i+1]);

    debug(score);

    auto best_score = score;
    auto best_perm = perm;
  
    const i64 MAX_ITER = 20'000'000'000;

    f64 done;
    f64 temp0 = 16.0;
    f64 temp1 = 0.5;
    f64 temp = temp0;

    i64 niter = 0;
    while(1) {
      niter += 1;
      if(niter % 1024 == 0) {
        done = 1.0 * niter / MAX_ITER;
        if(done > 1.0) break;
        temp = temp0 * pow(temp1 / temp0, done);
      }
      if(niter % (1<<25) == 0) {
        cerr
          << "id = " << setw(2) << pb.id << ", "
          << "niter = " << setw(12) << niter << ", "
          << "done = " << fixed << setprecision(6) << done << ", "
          << "temp = " << fixed << setprecision(6) << temp << ", "
          << "score = " << setw(9) << score << ", "
          << "best_score = " << setw(9) << best_score << ", "
          << endl;
      }

      auto accept = [&](i64 delta) {
        return delta <= 0 || delta <= temp * rng.randomDouble();
      };
    
      i64 ty = 1; //
      if(ty == 0) {
        i64 i = rng.random32(pb.n);
        i64 j = rng.random32(pb.n);
        if(i > j) swap(i,j);
        if(i == j) continue;

        auto a1 = perm[i], a2 = perm[i+1];
        auto b1 = perm[j], b2 = perm[j+1];
      
        i64 delta = 0;
        delta -= cost_dist(a1, a2);
        delta -= cost_dist(b1, b2);
        delta += cost_dist(a1, b1);
        delta += cost_dist(a2, b2);

        if(accept(delta)) {
          score += delta;
          reverse(begin(perm)+i+1, begin(perm)+j+1);
        }
      }else if(ty == 1) {
        i64 i = rng.random32(pb.n);
        i64 j = rng.random32(pb.n);
        i64 k = rng.random32(pb.n);
        if(i > j) swap(i,j);
        if(j > k) swap(j,k);
        if(i > j) swap(i,j);
        if(i == j) continue;
        if(j == k) continue;

        auto a1 = perm[i], a2 = perm[i+1];
        auto b1 = perm[j], b2 = perm[j+1];
        auto c1 = perm[k], c2 = perm[k+1];
      
        i64 delta = 0;
        delta -= cost_dist(a1, a2);
        delta -= cost_dist(b1, b2);
        delta -= cost_dist(c1, c2);
        delta += cost_dist(a1, b2);
        delta += cost_dist(b1, c2);
        delta += cost_dist(c1, a2);

        if(accept(delta)) {
          score += delta;
          rotate(begin(perm)+i+1, begin(perm)+j+1, begin(perm)+k+1);
        }

      }

      if(score < best_score) {
        best_score = score;
        best_perm = perm;
      }
    }

    { state S;
      S.reset_perm(pb, perm);
      S.reconstruct(pb, sol);
      
      sol = local_opt(pb, sol);
    }
  }
}

int main(int argc, char** argv) {
  runtime_assert(argc >= 2);
  i64 id = atoi(argv[1]);
  runtime_assert(1 <= id && id <= 25);
  do_precomputation();

  problem pb; pb.read(id);
  string sol = "";
  if(argc > 2) {
    auto filename = argv[2];
    ifstream is(filename);
    is>>sol;
  }
  
  solve(pb, sol);
  
  return 0;
}
