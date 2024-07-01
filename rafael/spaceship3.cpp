#include "spaceship.hpp"

const i64 MAXN   = 100'000;
const i64 OFFSET = 500'000;

u64 hash_visited[MAXN];
u64 hash_city[MAXN];
u64 hash_vx[2*OFFSET+1];
u64 hash_vy[2*OFFSET+1];

void init_hash() {
  FOR(i, MAXN) hash_visited[i] = rng.randomInt64();
  FOR(i, MAXN) hash_city[i] = rng.randomInt64();
  FOR(i, 2*OFFSET+1) hash_vx[i] = rng.randomInt64();
  FOR(i, 2*OFFSET+1) hash_vy[i] = rng.randomInt64();
}

vector<i64> neighbors[MAXN];

const i64 MAXNEI = 8;
const i64 MAXDV  = 32;

struct beam_state {
  bool visited[MAXN];
  i64  cost;
  i64  city;

  pt   speed;
  u64  hvisited;

  i64 time;
  i64 history[MAXN];
  pt  history_speed[MAXN];

  void reset() {
    cost = 0;
    city = 0;
    FOR(i, MAXN) visited[i] = 0;
    visited[0] = 1;
    speed = pt(MAXV, MAXV);
    hvisited = 1; // TODO different seed for every iter
    time = 0;
    history[0] = 0;
    history_speed[0] = speed;
  }

  bool do_move(problem const& pb, i64 i, i64 dvx, i64 dvy) {
    pt nspeed = speed + pt(dvx-MAXDV,dvy-MAXDV);
    if(nspeed.x < 0 || nspeed.x > 2*MAXV) return false;
    if(nspeed.y < 0 || nspeed.y > 2*MAXV) return false;

    visited[i] = 1;
    cost += pb.cost(city, speed, i, nspeed);
    city = i;
    time += 1;
    history[time] = i;
    history_speed[time] = nspeed;
    hvisited ^= hash_visited[i];
    speed = nspeed;

    return true;
  }
  
  bool do_move(problem const& pb, i64 m) {
    return do_move(pb, m/(2*MAXDV+1)/(2*MAXDV+1), m/(2*MAXDV+1)%(2*MAXDV+1), m%(2*MAXDV+1));
  }

  void undo_move(problem const& pb, i64 i, i64 dvx, i64 dvy) {
    pt ospeed = speed - pt(dvx-MAXDV, dvy-MAXDV);

    visited[i] = 0;
    city = history[time-1];
    cost -= pb.cost(city, ospeed, i, speed);
    speed = ospeed;
    hvisited ^= hash_visited[i];
    time -= 1;
  }
  
  void undo_move(problem const& pb, i64 m) {
    undo_move(pb, m/(2*MAXDV+1)/(2*MAXDV+1), m/(2*MAXDV+1)%(2*MAXDV+1), m%(2*MAXDV+1));
  }

  i64 value() const {
    return cost;
  }
  
  u64 get_hash() const {
    return hvisited
      ^ hash_city[city]
      ^ hash_vx[speed.x - MAXV + OFFSET]
      ^ hash_vy[speed.y - MAXV + OFFSET];
  }

  // void check(problem const& pb, i32) const {
  //   i64 total_cost = 0;
  //   FOR(i, time) {
  //     auto a = history[i], b = history[i+1];
  //     auto sa = history_speed[a], sb = history_speed[b];
  //     total_cost += pb.cost(a,sa,b,sb);
  //   }
  //   debug(total_cost, cost);
  //   runtime_assert(total_cost == cost);
  // }
  
  string reconstruct(problem const& pb) const {
    string out;

    i64 total_cost = 0;
    
    FOR(i, pb.n-1) {
      auto a = history[i], b = history[i+1];
      auto sa = history_speed[i], sb = history_speed[i+1];
      pb.reconstruct(out,a,sa,b,sb);
      total_cost += pb.cost(a,sa,b,sb);
    }
    debug(total_cost); // TODO
    return out;
  }
};

const i64 TREE_SIZE  = 500'000;
const i64 LIMIT_SIZE = TREE_SIZE - 100'000; // need 2 times the number of cities

using euler_tour_edge = u32;
struct euler_tour {
  i64              size;
  euler_tour_edge* data;

  FORCE_INLINE void reset() { size = 0; }
  FORCE_INLINE void push(i64 x) {
    data[size++] = x;
  }
  FORCE_INLINE u32& operator[](i64 ix) { return data[ix]; }
  FORCE_INLINE u32 const& operator[](i64 ix) const { return data[ix]; }
};
  
vector<euler_tour> tree_pool;
euler_tour get_new_tree(){
  euler_tour tour;
#pragma omp critical
  {
    if(tree_pool.empty()) {
      tour.size = 0;
      tour.data = new u32[TREE_SIZE];
    }else{
      tour = tree_pool.back();
      tour.size = 0;
      tree_pool.pop_back();
    }
  }
  return tour;
}

const i64 HASH_SIZE = 1ull<25;
const i64 HASH_MASK = HASH_SIZE-1;
atomic<uint64_t> *HS = nullptr;
atomic<int64_t> *HS_value = nullptr;

void euler_tour_best_state
    (beam_state &best_state,
     problem const& pb,
     i64 istep,
     euler_tour const& tour_current)
{
  beam_state S; S.reset();

  vector<i64> stack_moves(istep+1);
  i64 nstack_moves = 0;

  FOR(iedge, tour_current.size) {
    auto const& edge = tour_current[iedge];
    if(edge > 0) {
      stack_moves[nstack_moves] = edge - 1;
      S.do_move(pb, stack_moves[nstack_moves]);
      nstack_moves += 1;
    }else{
      if(nstack_moves == istep) {
        if(S.cost < best_state.cost) {
          best_state = S;
        }
      }
      
      if(nstack_moves == 0) {
        runtime_assert(S.cost == 0);
        return;
      }
				
      nstack_moves -= 1;
      S.undo_move(pb,stack_moves[nstack_moves]);
    }
  }

  runtime_assert(false);
}

void traverse_euler_tour
(problem const& pb,
 i64 istep,
 euler_tour const& tour_current,
 vector<euler_tour> &tours_next,
 vector<i64> &costs,
 i64 cutoff, float cutoff_keep_probability)
{
  beam_state S; S.reset();

  vector<i64> stack_moves(istep+1);
  i64 nstack_moves = 0;

  i64 ncommit = 0;
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
        if(S.value() < cutoff || (S.value() == cutoff && rng.randomFloat() < cutoff_keep_probability)) {
          while(ncommit < nstack_moves) {
            tour_next->push(1+stack_moves[ncommit]);
            ncommit += 1;
          }

          i64 count = 0;
          FOR(ix, neighbors[S.city].size()) {
            i64 i = neighbors[S.city][ix];
            if(S.visited[i]) continue;
            count += 1;
            FOR(dv, (2*MAXDV+1)*(2*MAXDV+1)) {
              u32 m = i * (2*MAXDV+1)*(2*MAXDV+1) + dv;
              if(S.do_move(pb,m)) {
                auto h = S.get_hash();
                auto prev = HS[h&HASH_MASK].exchange(h, std::memory_order_relaxed);
                if(prev != h || S.value() < HS_value[h&HASH_MASK]) {
                  HS_value[h&HASH_MASK] = S.value();
                  costs.eb(S.value());
                  tour_next->push(1+m);
                  tour_next->push(0);
                }
                S.undo_move(pb,m);
              }
            }

            if(count == MAXNEI) break;
          }
        }
      }

      if(nstack_moves == 0) {
        runtime_assert(S.cost == 0);
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

string beam_search(problem const& pb, i64 width) {
  init_hash();
  if(!HS) {
    auto ptr = new atomic<uint64_t>[HASH_SIZE];
    HS = ptr;
    auto ptr_value = new atomic<int64_t>[HASH_SIZE];
    HS_value = ptr_value;
  }
  
  vector<euler_tour> tours_current;
  tours_current.eb(get_new_tree());
  tours_current.back().push(0);
	
  i64   cutoff = 1e9;
  float cutoff_keep_probability = 1.0;

  FOR(istep, pb.n - 1) {
    timer timer_s;
    vector<euler_tour> tours_next;

    vector<i64> costs;
  
#pragma omp parallel
    {
      vector<euler_tour> L_tours_next;
      vector<i64> L_costs;
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

        traverse_euler_tour
          (pb,
           istep,
           tour_current, 
           L_tours_next, 
           L_costs,
           cutoff, cutoff_keep_probability);

        #pragma omp critical
        {
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
        costs.insert(end(costs), all(L_costs));
      }
    }

    if((i64)costs.size() > width) {
      nth_element(begin(costs), begin(costs)+width, end(costs));
      cutoff = costs[width];
      i64 nin = 0, ntotal = 0;
      FOR(i, costs.size()) if(costs[i] == cutoff) {
        if(i < width) nin += 1;
        ntotal += 1;
      }
      cutoff_keep_probability = 1.0 * nin / ntotal;
    }else{
      cutoff = 1e9;
    }

    i64 total_size = 0;
    for(auto const& t : tours_next) total_size += t.size;
    
    i64 low = *min_element(all(costs));
    cerr << setw(3) << istep+1 << "/" << pb.n-1 << ": " <<
      " costs = " << setw(3) << low << ".." << setw(3) << cutoff <<
      ", tree size = " << setw(12) << total_size <<
      ", num trees = " << setw(6) << tours_next.size() <<
      ", elapsed = " << setw(10) << timer_s.elapsed() << "s" <<
      endl;

    tours_current = tours_next;
  }

  beam_state best_state; best_state.cost = 1e9;
  for(auto tour : tours_current) {
    euler_tour_best_state
      (best_state,
       pb,
       pb.n-1,
       tour
       );
  }

  debug(best_state.cost);
  auto sol = best_state.reconstruct(pb);
  debug(sol.size());

  return sol;
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

  FOR(i, pb.n) {
    FOR(j, pb.n) if(j != i) {
      neighbors[i].eb(j);
    }
    sort(all(neighbors[i]),
         [&](i64 j, i64 k){
           pt s0(MAXV,MAXV);
           return pb.cost(i,s0,j,s0) < pb.cost(i,s0,k,s0);
         });
  }
  
  auto sol = argc == 3 ? beam_search(pb, width) : "";
  if(argc > 3) {
    auto filename = argv[3];
    ifstream is(filename);
    is>>sol;
  }
  local_opt(pb, sol);
  
  return 0;
}
