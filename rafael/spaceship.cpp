#include "spaceship.hpp"

const i32 MAXN   = 100'000;
const i32 OFFSET = 500'000;

u64 hash_visited[MAXN];
u64 hash_x[2*OFFSET+1];
u64 hash_y[2*OFFSET+1];
u64 hash_vx[2*OFFSET+1];
u64 hash_vy[2*OFFSET+1];

void init_hash() {
  FOR(i, MAXN) hash_visited[i] = rng.randomInt64();
  FOR(i, 2*OFFSET+1) hash_x[i] = rng.randomInt64();
  FOR(i, 2*OFFSET+1) hash_y[i] = rng.randomInt64();
  FOR(i, 2*OFFSET+1) hash_vx[i] = rng.randomInt64();
  FOR(i, 2*OFFSET+1) hash_vy[i] = rng.randomInt64();
}

struct beam_state {
  i32 time;
  i32 visit_at[MAXN];
  i32 x,y,vx,vy;

  i32 nvisited;
  u64 hvisited;

  void reset() {
    time = 0;
    FOR(i, MAXN) visit_at[i] = -1;
    visit_at[0] = 0;
    x = y = 0;
    vx = vy = 0;
    nvisited = 1;
    hvisited = 1; // TODO different seed for every iter
  }

  bool do_move(problem const& pb, i32 dx, i32 dy) {
    vx += (dx-1);
    vy += (dy-1);
    x += vx;
    y += vy;
    if(x < -OFFSET || x > OFFSET || y < -OFFSET || y > OFFSET ||
       vx < -OFFSET || vx > OFFSET || vy < -OFFSET || vy > OFFSET)
      {
        x -= vx;
        y -= vy;
        vy -= (dy-1);
        vx -= (dx-1);
        return false;
      }

    time += 1;
    auto it = pb.RA.find(pt(x,y));
    if(it != pb.RA.end()) {
      auto i = it->second;
      if(visit_at[i] == -1) {
        visit_at[i] = time;
        nvisited += 1;
        hvisited ^= hash_visited[i];
      }
    }

    return true;
  }
  
  bool do_move(problem const& pb, i32 m) {
    return do_move(pb,m/3,m%3);
  }

  void undo_move(problem const& pb, i32 dx, i32 dy) {
    auto it = pb.RA.find(pt(x,y));
    if(it != pb.RA.end()) {
      auto i = it->second;
      if(visit_at[i] == time) {
        visit_at[i] = -1;
        nvisited -= 1;
        hvisited ^= hash_visited[i];
      }
    }
    time -= 1;
    
    x -= vx;
    y -= vy;
    vy -= (dy-1);
    vx -= (dx-1);
  }
  
  void undo_move(problem const& pb, i32 m) {
    return undo_move(pb,m/3,m%3);
  }

  i32 value() const {
    return nvisited;
  }
  
  u64 get_hash() const {
    return hvisited
      ^ hash_x[x + OFFSET]
      ^ hash_y[y + OFFSET]
      ^ hash_vx[vx + OFFSET]
      ^ hash_vy[vy + OFFSET];
  }
};

const i32 TREE_SIZE  = 500'000;
const i32 LIMIT_SIZE = TREE_SIZE - 100'000; // need 2 times the number of cities

using euler_tour_edge = u8;
struct euler_tour {
  i32              size;
  euler_tour_edge* data;

  FORCE_INLINE void reset() { size = 0; }
  FORCE_INLINE void push(i32 x) {
    data[size++] = x;
  }
  FORCE_INLINE u8& operator[](i32 ix) { return data[ix]; }
};
  
vector<euler_tour> tree_pool;
euler_tour get_new_tree(){
  euler_tour tour;
#pragma omp critical
  {
    if(tree_pool.empty()) {
      tour.size = 0;
      tour.data = new u8[TREE_SIZE];
    }else{
      tour = tree_pool.back();
      tour.size = 0;
      tree_pool.pop_back();
    }
  }
  return tour;
}

const i64 HASH_SIZE = 1ull<<30;
const i64 HASH_MASK = HASH_SIZE-1;
atomic<uint64_t> *HS = nullptr;

void traverse_euler_tour
(i32& best, string& sol,
 problem const& pb,
 i32 istep,
 euler_tour tour_current,
 vector<euler_tour> &tours_next,
 i64* histogram,
 i32 cutoff, float cutoff_keep_probability)
{
  beam_state S; S.reset();

  vector<i32> stack_moves(istep+1);
  i32 nstack_moves = 0;

  i32 ncommit = 0;
  if(tours_next.empty()) tours_next.eb(get_new_tree());
  auto *tour_next = &tours_next.back(); 

  FOR(iedge, tour_current.size) {
    auto const& edge = tour_current[iedge];
    if(edge > 0) {
      stack_moves[nstack_moves] = edge - 1;
      S.do_move(pb, stack_moves[nstack_moves]);
      nstack_moves += 1;
    }else{
      if(nstack_moves == istep) {
        if(S.value() > cutoff || (S.value() == cutoff && rng.randomFloat() < cutoff_keep_probability)) {
          while(ncommit < nstack_moves) {
            tour_next->push(1+stack_moves[ncommit]);
            ncommit += 1;
          }

          if(S.nvisited > best) {
#pragma omp critical
            {
              if(S.nvisited > best) {
                best = S.nvisited;
                sol.clear();
                FOR(i, nstack_moves) {
                  i32 m = stack_moves[i];
                  i32 dx = m/3, dy = m%3;
                  sol += dc[dy][dx];
                }
              }
            }
          }

          FOR(m, 9) {
            if(S.do_move(pb,m)) {
              auto h = S.get_hash();
              auto prev = HS[h&HASH_MASK].exchange(h, std::memory_order_relaxed);
              if(prev != h) {
                histogram[S.value()] += 1;
                tour_next->push(1+m);
                tour_next->push(0);
              }
              S.undo_move(pb,m);
            }
          }
        }
      }

      if(nstack_moves == 0) {
        runtime_assert(S.nvisited == 1);
        return;
      }

      if(ncommit == nstack_moves) {
        tour_next->push(0);
        ncommit -= 1;
      }
				
      nstack_moves -= 1;
      S.undo_move(pb,stack_moves[nstack_moves]);

      if(tour_next->size > LIMIT_SIZE) {
        FORD(i,ncommit-1,0) tour_next->push(0);
        tour_next->push(0);
        tours_next.eb(get_new_tree());
        tour_next = &tours_next.back();
        ncommit = 0;
      }
    }
  }

  runtime_assert(false);
}

string beam_search(problem const& pb, i32 width) {
  init_hash();
  if(!HS) {
    auto ptr = new atomic<uint64_t>[HASH_SIZE];
    HS = ptr;
  }
  
  i32 max_score = pb.n;
  vector<i64> histogram(max_score+1, 0);
  
  vector<euler_tour> tours_current;
  tours_current.eb(get_new_tree());
  tours_current.back().push(0);
	
  i32   cutoff = 0;
  float cutoff_keep_probability = 1.0;

  for(i32 istep = 0;; ++istep) {
    timer timer_s;
    vector<euler_tour> tours_next;
    histogram.assign(max_score+1,0);

    i32 best = 0;
    string sol;

#pragma omp parallel
    {
      vector<euler_tour> L_tours_next;
      vector<i64> L_histogram(max_score+1,0);
      while(1) {
        euler_tour tour_current;
#pragma omp critical
        { if(!tours_current.empty()) {
            tour_current = tours_current.back();
            tours_current.pop_back();
          }else{
            tour_current.size = 0;
          }
        }
        if(tour_current.size == 0) break;

        i32 lbest = 0;
        string lsol;
        traverse_euler_tour
          (lbest,lsol,
           pb,
           istep,
           tour_current, 
           L_tours_next, 
           L_histogram.data(),
           cutoff, cutoff_keep_probability);

        #pragma omp critical
        {
          if(lbest > best) {
            best = lbest;
            sol = lsol;
          }

          tree_pool.eb(tour_current);
        }
      }
#pragma omp critical
      { if(!L_tours_next.empty()) {
          L_tours_next.back().push(0);
        }
        while(!L_tours_next.empty()) {
          tours_next.eb(L_tours_next.back());
          L_tours_next.pop_back();
        }
        FOR(i,max_score+1) histogram[i] += L_histogram[i];
      }
    }

    if(best == pb.n) debug("FOUND");
    
    if(best == pb.n || istep == upper_bounds[pb.id] + 1) {
      return sol;
    }

    { i64 total_count = 0;
      cutoff = 0;
      cutoff_keep_probability = 1.0;
      FORD(i,max_score,0) {
        if(total_count+histogram[i] > width) {
          cutoff = i;
          cutoff_keep_probability = (float)(width-total_count) / (float)(histogram[i]);
          break;
        }
        total_count += histogram[i];
      }
    }

    i64 total_size = 0;
    for(auto const& t : tours_next) total_size += t.size;
    
    i32 high = 0;
    FOR(i, max_score+1) if(histogram[i]) high = i;
    cerr << setw(3) << istep << ": " <<
      "nvisited = " << setw(3) << cutoff << ".." << setw(3) << high <<
      ", tree size = " << setw(12) << total_size <<
      ", num trees = " << setw(6) << tours_next.size() <<
      ", elapsed = " << setw(10) << timer_s.elapsed() << "s" <<
      endl;

    tours_current = tours_next;
  }
}

int main(int argc, char** argv) {
  runtime_assert(argc >= 3);
  i64 id = atoi(argv[1]);
  runtime_assert(1 <= id && id <= 25);
  i64 width = atoi(argv[2]);
  runtime_assert(1 <= width && width < 1'000'000'000);
  do_precomputation();

  
  debug(upper_bounds[id]);
  
  problem pb; pb.read(id);
  
  auto sol = argc == 3 ? beam_search(pb, width) : "";
  if(argc > 3) {
    auto filename = argv[3];
    ifstream is(filename);
    is>>sol;
  }
  local_opt(pb, sol);
  
  return 0;
}
