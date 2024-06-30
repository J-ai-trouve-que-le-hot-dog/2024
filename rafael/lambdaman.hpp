#pragma once
#include "header.hpp"

struct union_find {
  vector<int> A;

  union_find(int n = 0) : A(n) {
    iota(all(A), 0);
  }

  int find(int a) {
    return A[a] == a ? a : A[a] = find(A[a]);
  }

  bool unite(int a, int b) {
    a = find(a);
    b = find(b);
    if(a == b) {
      return false;
    }
    A[a] = b;
    return true;
  }
};

using f64 = double;
using f32 = float;

const i64 dx[4] = {-1,1,0,0};
const i64 dy[4] = {0, 0, -1, 1};
const char* dc = "UDLR";

struct problem {
  i32 id;
  i32 sz;
  vector<array<i64,4>> graph;
  vector<vector<i64>> T;
  i32 start;

  void load(i32 id_) {
    id = id_;
    ifstream is("inputs/lambdaman" + to_string(id));
    runtime_assert(is.good());
    vector<string> grid;
    string line;
    while(getline(is, line)) {
      if(line.empty()) break;
      grid.eb(line);
      debug(line);
    }

    i64 n = grid.size(), m = grid[0].size();
    vector<array<i64,2>> points;
    vector<vector<i64>> unpoint(n, vector<i64>(m));
    FOR(i, n) FOR(j, m) if(grid[i][j] != '#') {
      unpoint[i][j] = points.size();
      points.pb({i,j});
    }
    sz = points.size();
    debug(sz);
    start = 0;
    FOR(i, n) FOR(j, m) if(grid[i][j] == 'L') {
      start = unpoint[i][j];
    }

    graph.resize(sz);
    FOR(i, sz) {
      auto p = points[i];
      FOR(d, 4) {
        i64 x = p[0] + dx[d];
        i64 y = p[1] + dy[d];
        if(x<0||x>=n||y<0||y>=m||grid[x][y]=='#') {
          graph[i][d] = -1;
        }else{
          graph[i][d] = unpoint[x][y];
        }
      }
    }

    T.resize(sz);
    union_find uf(sz);
    FOR(i, sz) FOR(d, 4) if(i64 j = graph[i][d]; j != -1) {
      if(uf.unite(i,j)) {
        T[i].eb(j);
        T[j].eb(i);
      }
    }
  }
};
