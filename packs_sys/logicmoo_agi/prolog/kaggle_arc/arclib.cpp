//#include <ext/pb_ds/assoc_container.hpp>
//using namespace __gnu_pbds;

double now();

struct State {
  vImage vimg;
  int depth;
  bool isvec;
  State() {}
  State(vImage_ vimg_, bool isvec_, int depth_) : vimg(vimg_), isvec(isvec_), depth(depth_) {}
  ull hash() const {
    ull r = isvec;
    for (Image_ img : vimg) {
      r += hashImage(img)*123413491;
    }
    return r;
  }
};


#include "efficient.hpp"



struct Node {
  State state;
  //vImage vimg;
  //bool isvec;
  vector<pair<int,int>> child;
  //int depth;
  int par, pfi;
  bool freed, ispiece;
  Node() {
    freed = ispiece = false;
  }
  Node(vImage_ vimg_, bool isvec_, int depth_, int par_ = -1, int pfi_ = -1) : par(par_), pfi(pfi_) {
    freed = ispiece = false;
    state.vimg = vimg_;
    state.isvec = isvec_;
    state.depth = depth_;
  }
};


struct Functions3 {
  vector<int> listed, cost;
  vector<string> names;
  vector<function<bool(const State&,State&)>> f_list;

  void add(const string& name, int cost, const function<bool(const State&,State&)>&func, int list);
  void add(string name, int cost, const function<Image(Image_)>&f, int list = 1);
  void add(string name, int cost, const function<vImage(Image_)>&f, int list = 1);
  void add(string name, int cost, const function<Image(vImage_)>&f, int list = 1);
  void add(string name, int cost, const function<vImage(vImage_)>&f, int list = 1);
  void add(const vector<point>&sizes, string name, int cost, const function<Image(Image_,Image_)>&f, int list = 1);
  string getName(int fi);
  int findfi(string name);
};



struct DAG {
  Functions3 funcs;
  //vector<Node> node;
  TinyNodeBank tiny_node;
  int givens;
  point target_size;
  //gp_hash_table<ull, int> hashi;
  TinyHashMap hashi;
  vector<int> binary;
  int add(const State&nxt, bool force = false);
  Image getImg(int nodei);
  void build();
  void buildBinary();
  void initial(Image_ test_in, const vector<pair<Image,Image>>&train, vector<point> sizes, int ti);
  void benchmark();
  int applyFunc(int curi, int fi, const State&state);
  int applyFunc(int curi, int fi);
  void applyFunc(string name, bool vec);
  void applyFuncs(vector<pair<string,int>> names, bool vec);
};


struct Pieces;
vector<DAG> brutePieces2(Image_ test_in, const vector<pair<Image,Image>>&train, vector<point> out_sizes);
vector<point> bruteSize(Image_ test_in, vector<pair<Image,Image>> train);
vector<point> cheatSize(Image_ test_out, vector<pair<Image,Image>> train);
struct Candidate {
  vImage imgs;
  double score;
  int cnt_pieces, sum_depth, max_depth;
  Candidate(vImage_ imgs_, double score_) : imgs(imgs_), score(score_) {
    cnt_pieces = sum_depth = max_depth = -1;
  }
  Candidate(vImage_ imgs_, int cnt_pieces_, int sum_depth_, int max_depth_) :
    imgs(imgs_), cnt_pieces(cnt_pieces_), sum_depth(sum_depth_), max_depth(max_depth_) {
    score = -1;
  }
};

inline bool operator<(const Candidate& a, const Candidate& b) {
  return a.score > b.score;
}

vector<Candidate> composePieces2(Pieces&pieces, vector<pair<Image, Image>> train, vector<point> out_sizes);
vector<Candidate> evaluateCands(const vector<Candidate>&cand, vector<pair<Image,Image>> train);
namespace core {
  int colMask(Image_ img);
  int countCols(Image_ img, int include0 = 0);
  int count(Image_ img);
  Image full(point p, point sz, int filling = 1);
  Image empty(point p, point sz);
  Image full(point sz, int filling = 1);
  Image empty(point sz);
  bool isRectangle(Image_ a);
  int countComponents(Image img);
  char majorityCol(Image_ img, int include0 = 0);
  Image subImage(Image_ img, point p, point sz);
  vector<pair<Image,int>> splitCols(Image_ img, int include0 = 0);
};

struct deduceOuterProduct {
  vector<pair<Image,Image>> train_targets;
  int rec_funci;
  deduceOuterProduct(vector<pair<Image,Image>> train);
  Image reconstruct(Image_ a, Image_ b);
};
void addDeduceOuterProduct(Pieces&pieces, vector<pair<Image,Image>> train, vector<Candidate>&cands);
typedef unsigned long long ull;
struct hashEntry {
  ull key;
  int value, next;
};
struct TinyHashMap {
  static constexpr double sparse_factor = 1.5, resize_step = 2;
  static constexpr int minsz = 1<<20;
  vector<hashEntry> data;
  vector<int> table;
  ull mask;
  pair<int,bool> insert(ull key, int value);
  unsigned int size() {
    return data.size();
  }
  void clear() {
    data.clear();
    table.clear();
  }
};





struct TinyChildren {
  static constexpr int dense_thres = 10;
  static constexpr int None = -2;
  union {
    int*dense;
    pair<int,int>*sparse;
  };
  short sz = 0, cap = 0;
  TinyChildren() {
    dense = NULL;
  }
  void add(int fi, int to);
  int get(int fi);
  ~TinyChildren() {
    delete[]dense;
    dense = NULL;
  }
  TinyChildren(const TinyChildren&) = delete;
  TinyChildren(TinyChildren&&o) {
    dense = o.dense;
    sz = o.sz, cap = o.cap;
    o.dense = NULL;
  }

  void legacy(vector<pair<int,int>>&child);
  void clear() {
    delete[]dense;
    dense = NULL;
    sz = cap = 0;
  }
};




struct TinyBank {
  vector<unsigned int> mem;
  ll curi = 0;
  void alloc() {
    if (curi/32+2000 >= mem.size())
      mem.resize(curi/32+2000);
  }
  inline void set(ll bi, unsigned int v) {
    int i = bi>>5, j = bi&31;
    if (i >= mem.size()) mem.resize(max(i+1,1024));
    mem[i] |= v<<j;
    //if (v) mem[i] |= 1u<<j;
    //else mem[i] &= ~(1u<<j);
  }
  inline void set(ll bi, unsigned int v, int len) {
    int i = bi>>5, j = bi&31;

    mem[i] |= v << j;
    if (j+len > 32)
      mem[i+1] |= v >> 32-j;
  }
  inline int get(ll bi) {
    return mem[bi>>5]>>(bi&31)&1;
  }
};

struct TinyImage {
  static constexpr int align = 16;
  uint32_t memi;
  short sz;
  char x, y, w, h;
  uint8_t tree[9];
  TinyImage(Image_ img, TinyBank&bank);
  Image decompress(TinyBank&bank);
};

struct TinyNode {
  int*vimg = NULL;
  bool isvec, ispiece;
  short imgs; //TODO change to unsigned char
  char depth;
  TinyChildren child;
  TinyNode() {}
  TinyNode(TinyNode&&o) = default;
};

struct TinyNodeBank {
  TinyBank bank;
  vector<TinyImage> imgs;
  vector<TinyNode> node;
  Image read(int i) {
    assert(i >= 0 && i < imgs.size());
    return imgs[i].decompress(bank);
  }
  Image getImg(int ni) {
    return imgs[node[ni].vimg[0]].decompress(bank);
  }
  State getState(int ni) {
    State ret;
    ret.vimg.resize(node[ni].imgs);
    for (int i = 0; i < node[ni].imgs; i++)
      ret.vimg[i] = imgs[node[ni].vimg[i]].decompress(bank);
    ret.depth = node[ni].depth;
    ret.isvec = node[ni].isvec;
    return ret;
  }
  void append(const State&state, bool ispiece) {
    assert(state.depth >= 0 && state.depth < 128);
    TinyNode v;
    //assert(state.vimg.size() < 1<<16); //TODO: change to unsigned char

    v.imgs = min((int)state.vimg.size(),100000);
    v.vimg = new int[v.imgs];
    for (int i = 0; i < v.imgs; i++) {
      v.vimg[i] = imgs.size();
      imgs.emplace_back(state.vimg[i], bank);
    }
    v.isvec = state.isvec;
    v.ispiece = ispiece;
    v.depth = state.depth;
    node.push_back(move(v));
  }
  void addChild(int ni, int fi, int to) {
    node[ni].child.add(fi, to);
  }
  int getChild(int ni, int fi) {
    return node[ni].child.get(fi);
  }
  ~TinyNodeBank() {
    for (TinyNode&n : node)
      delete[]n.vimg;
  }
  TinyNode& operator[](int i) {
    return node[i];
  }
  int size() { return node.size(); }
};
void evalEvals(int print = 1);
vImage splitAll(Image_ img);
Image eraseCol(Image img, int col);
vImage insideMarked(Image_ in);
Image makeBorder(Image_ img, int bcol = 1);
Image makeBorder2(Image_ img, int usemaj = 1);
Image makeBorder2(Image_ img, Image_ bord);
Image compress2(Image_ img);
Image compress3(Image_ img);
Image greedyFillBlack(Image_ img, int N = 3);
Image greedyFillBlack2(Image_ img, int N = 3);
Image extend2(Image_ img, Image_ room);
Image connect(Image_ img, int id);
Image replaceTemplate(Image_ in, Image_ need_, Image_ marked_, int overlapping = 0, int rigids = 0);
Image swapTemplate(Image_ in, Image_ a, Image_ b, int rigids = 0);
Image spreadCols(Image img, int skipmaj = 0);
vImage splitColumns(Image_ img);
vImage splitRows(Image_ img);
Image half(Image_ img, int id);
Image smear(Image_ img, int id);
Image mirror2(Image_ a, Image_ line);
vImage gravity(Image_ in, int d);

Image myStack(vImage_ lens, int id);
Image stackLine(vImage_ shapes);
Image composeGrowing(vImage_ imgs);

Image pickUnique(vImage_ imgs, int id);
Image Col(int id);
Image Pos(int dx, int dy);
Image Square(int id);
Image Line(int orient, int id);
Image getPos(Image_ img);
Image getSize(Image_ img);
Image hull(Image_ img);
Image toOrigin(Image img);
Image getW(Image_ img, int id);
Image getH(Image_ img, int id);
Image filterCol(Image_ img, int col);
Image colShape(Image_ shape, int col);
Image compress(Image_ img, Image_ bg = Col(0));
Image Fill(Image_ a);
Image interior(Image_ a);
Image border(Image_ a);
Image center(Image_ img);
//Image transform(Image_ img, int A00, int A01, int A10, int A11);
Image rigid(Image_ img, int id);
Image invert(Image img);
Image interior2(Image_ a);
Image count(Image_ img, int id, int outType);
//Image smear(Image_ base, int id);

Image hull0(Image_ img);
Image getSize0(Image_ img);
Image getRegular(Image_ img);
vImage cut(Image_ img);

//One and a bit of the other
Image Move(Image img, Image_ p);
Image embed(Image_ img, Image_ shape);
Image extend(Image_ img, Image_ room);
Image wrap(Image_ line, Image_ area);

//Two inputs
Image filterCol(Image_ img, Image_ palette);
Image colShape(Image_ col, Image_ shape);
Image broadcast(Image_ col, Image_ shape, int include0 = 1);
Image replaceCols(Image_ base, Image_ cols);
Image smear(Image_ base, Image_ room, int id);
//Image alignx(Image_ a, Image_ b, int id);
//Image aligny(Image_ a, Image_ b, int id);
Image align(Image_ a, Image_ b, int idx, int idy);
Image align(Image_ a, Image_ b);
//Image compose(Image_ a, Image_ b, const function<int(int,int)>&f, int overlap_only);
Image compose(Image_ a, Image_ b, int id = 0);
Image outerProductIS(Image_ a, Image_ b);
Image outerProductSI(Image_ a, Image_ b);
Image myStack(Image_ a, Image b, int orient);


//Image pickMax(const vector<Image>& v, function<int(Image_)> f);
Image pickMax(vImage_ v, int id);
//vector<Image> cut(Image_ img, Image_ a);
//vector<Image> splitCols(Image_ img, int include0 = 0);
//Image compose(const vector<Image>&imgs, int id);
//void getRegular(vector<int>&col);
//Image getRegular(Image_ img);
Image cutPickMax(Image_ a, int id);
//Image regularCutPickMax(Image_ a, int id);
Image splitPickMax(Image_ a, int id, int include0 = 0);
//Image cutCompose(Image_ a, Image_ b, int id);
//Image regularCutCompose(Image_ a, int id);
//Image splitCompose(Image_ a, int id, int include0 = 0);
//Image cutIndex(Image_ a, Image_ b, int ind);
Image splitPickMaxes(Image_ a, int id);
Image cutIndex(Image_ a, int ind);
Image cutPickMaxes(Image_ a, int id);
Image heuristicCut(Image_ a);

vImage pickNotMaxes(vImage_ v, int id);

Image cutPickMax(Image_ a, Image_ b, int id);
Image cutPickMaxes(Image_ a, Image_ b, int id);
vImage pickMaxes(vImage_ v, int id);
Image compose(vImage_ imgs, int id);
vImage cut(Image_ img, Image_ a);
vImage splitCols(Image_ img, int include0 = 0);

Image repeat(Image_ a, Image_ b, int pad = 0);
Image mirror(Image_ a, Image_ b, int pad = 0);
Image majCol(Image_ img);
struct Loader {
  long long n;
  string text;
  long long counter;
  long long prev;
  int keep_title;
  Loader(long long n_, string text_ = "") {
    n = n_;
    text = text_;
    counter = 0;
    prev = -1;
    keep_title = 0;
  }
  void operator()();
  ~Loader();
};
struct Simplifier {
  function<Image(Image_)> in;
  function<Image(Image_,Image_)> out, rec;
  pair<Image,Image> operator()(Image_ a, Image_ b);
};

Simplifier normalizeCols(const vector<pair<Image,Image>>&train);
Simplifier normalizeDummy(const vector<pair<Image,Image>>&train);

//inline vector<double> shapeFeatures(Image_ img, int col);
//inline Image remapCols(Image_ img, int cols[10]);
void remapCols(const vector<pair<Image,Image>>&train, vector<Simplifier>&sims);
//inline Image listCols(Image_ img, int extra);
void normalizeCols(vector<Sample>&sample);

void normalizeRigid(const vector<pair<Image,Image>>&train, vector<Simplifier>&sims);
void evalNormalizeRigid();
/*struct Piece2 {
  vector<int> ind;
  int depth;
  };*/
struct Piece3 {
  int memi, depth;
};

struct Pieces {
  vector<DAG> dag;
  vector<Piece3> piece;
  //vector<vector<int>> seen;
  vector<int> mem;
};

Pieces makePieces2(vector<DAG>&dag, vector<pair<Image,Image>> train, vector<point> out_sizes);
#include <vector>
#include <unordered_map>
#include <cassert>
#include <iostream>
#include <set>
#include <random>
#include <algorithm>
#include <tuple>
#include <functional>
#include <regex>
#include <queue>

//#include <bits/stdc++.h>

struct Sample {
  vector<pair<Image,Image>> train, test;
  Image test_in, test_out;
  string id;
  int id_ind;
  FILE*fp;
  Sample(string filename);
  char mygetc();
  int end(char endc);
  void expect(char c);
  string getQuote();
  vector<int> readRow();
  Image readImage();
  vector<Sample> split();
};

vector<Sample> readAll(string path, int maxn);

struct Writer {
  FILE*fp;
  map<string,int> seen;
  Writer(string filename = "submission_part.csv");
  void operator()(const Sample& s, vector<Image> imgs);
  ~Writer();
};

void writeAnswersWithScores(const Sample&s, string fn, vector<Image> imgs, vector<double> scores);
void run(int only_sid = -1, int maxdepth = -1);
struct Candidate;
int scorePieces(const vector<Piece>&piece, Image_ test_in, Image_ test_out, const vector<pair<Image,Image>>&train);
int scoreCands(const vector<Candidate>&cands, Image_ test_in, Image_ test_out);
int scoreAnswers(vImage_ answers, Image_ test_in, Image_ test_out);

struct Spec {
  bool bad = 0, anypos = 0;
  union {
    point p;
    struct {
      int x, y;
    };
  };
  union {
    point sz;
    struct {
      int w, h;
    };
  };
  vector<int> mask;
  inline int& operator()(int i, int j) {
#if defined CHECK_BOUNDS
    assert(i >= 0 && i < h && j >= 0 && j < w);
#endif
    return mask[i*w+j];
  }
  inline int operator()(int i, int j) const {
#if defined CHECK_BOUNDS
    assert(i >= 0 && i < h && j >= 0 && j < w);
#endif
    return mask[i*w+j];
  }
  bool check(Image_ img) const {
    if (bad) return 0;
    if (img.sz != sz || (!anypos && img.p != p)) return 0;
    for (int i = 0; i < mask.size(); i++)
      if ((mask[i]>>img.mask[i]&1) == 0) return 0;
    return 1;
  }
};

typedef const Spec& Spec_;
const int allCols = (1<<10)-1;
void storePieces(vector<vector<Piece>>&pieces);
vector<vector<Piece>> loadPieces();
void storeCands(vector<vector<Image>>&cands);
vector<vector<Image>> loadCands();

void storePieces(vector<Piece>&vp, string id, int id_ind);
vector<Piece> loadPieces(string id, int id_ind);
void evalTasks();
#include <chrono>

struct Timer {
  const int skips = 100, brute_thres = 1000;
  bool running = false;
  double cnt = 0, brute_sum = 0;
  int sample_counter = 0, target = 0;
  double samples = 0, sample_sum = 0, sample_var = 0;
  mt19937 mrand;
  double now() {
    return chrono::high_resolution_clock::now().time_since_epoch().count()*1e-9;
  }
  double start_time;
  void start() {
    assert(!running);
    running = true;
    if (cnt < brute_thres || sample_counter == target)
      start_time = now();
  }
  void stop() {
    assert(running);
    running = false;

    double dt;
    if (cnt < brute_thres || sample_counter == target)
      dt = now()-start_time;

    if (cnt < brute_thres) {
      brute_sum += dt;
    }
    if (sample_counter == target) {
      samples++;
      sample_sum += dt;
      sample_var += dt*dt;
      target = mrand()%skips;
    }

    if (++sample_counter == skips)
      sample_counter = 0;
    cnt++;
  }
  tuple<double,double,double> read() {
    if (cnt <= brute_thres)
      return {cnt, brute_sum, 0};
    double mean = sample_sum/samples;
    double var = (sample_var/samples-mean*mean)/(samples-1);
    return {cnt, mean*cnt, sqrt(var)*cnt};
  }
  void print(const string& label) {
    auto [cnt,sum,std] = read();
    printf("%5.1f ± %4.1f ms - %s\n", sum*1e3, std*1e3, label.c_str());
  }
};
//const int MAXSIDE = 100, MAXAREA = 40*40, MAXPIXELS = 40*40*5;
extern int MAXSIDE, MAXAREA, MAXPIXELS;

//#define CHECK_BOUNDS

struct point {
  int x, y;
};
inline point operator+(const point& a, const point& b) {
  return {a.x+b.x, a.y+b.y};
}
inline point operator-(const point& a, const point& b) {
  return {a.x-b.x, a.y-b.y};
}
inline point operator*(const point& a, int f) {
  return {a.x*f, a.y*f};
}
inline point operator/(const point& a, int f) {
  assert(a.x%f == 0 && a.y%f == 0);
  return {a.x/f, a.y/f};
}
inline int operator*(const point& a, const point& b) {
  return a.x*b.x+a.y*b.y;
}
inline int operator%(const point& a, const point& b) {
  return a.x*b.y-a.y*b.x;
}
inline bool operator==(const point& a, const point& b) {
  return a.x == b.x && a.y == b.y;
}
inline bool operator!=(const point& a, const point& b) {
  return a.x != b.x || a.y != b.y;
}



struct Image {
  union {
    point p;
    struct {
      int x, y;
    };
  };
  union {
    point sz;
    struct {
      int w, h;
    };
  };
  vector<char> mask;
  inline char& operator()(int i, int j) {
#if defined CHECK_BOUNDS
    assert(i >= 0 && i < h && j >= 0 && j < w);
#endif
    return mask[i*w+j];
  }
  inline char operator()(int i, int j) const {
#if defined CHECK_BOUNDS
    assert(i >= 0 && i < h && j >= 0 && j < w);
#endif
    return mask[i*w+j];
  }
  inline char safe(int i, int j) const {
    return (i<0 || j<0 || i>=h || j>=w ? (char)0 : mask[i*w+j]);
  }
};

using Image_ = const Image&;
using vImage = vector<Image>;
using vImage_ = const vector<Image>&;

inline bool operator==(Image_ a, Image_ b) {
  return tie(a.p,a.sz,a.mask) == tie(b.p,b.sz,b.mask);
}
inline bool operator!=(Image_ a, Image_ b) {
  return tie(a.p,a.sz,a.mask) != tie(b.p,b.sz,b.mask);
}
inline bool operator<(Image_ a, Image_ b) {
  if (a.sz != b.sz) return tie(a.w,a.h) < tie(b.w,b.h);
  return a.mask < b.mask;
}

using ll = long long;
using ull = unsigned long long;
inline ull hashImage(Image_ img) {
  const ull base = 137;
  ull r = 1543;
  r = r*base+img.w;
  r = r*base+img.h;
  r = r*base+img.x;
  r = r*base+img.y;
  for(char c : img.mask) {
    //assert(c >= 0 && c < 10);
    r = r*base+c;
  }
  return r;
}

struct Piece {
  vector<Image> imgs;
  double node_prob;
  int keepi, knowi;
};

const Image badImg = {{0,0},{0,0},{}};
const Image dummyImg = {{0,0},{1,1},{0}};

using Piece_ = const Piece&;

template<class T, class F>
int checkAll(const vector<T>&v, F f) {
  int all = 1;
  for (const T& it : v)
    all &= f(it);
  return all;
}
template<class T, class F>
int allEqual(const vector<T>&v, F f) {
  int need = f(v[0]);
  for (const T& it : v) {
    if (f(it) != need) return 0;
  }
  return 1;
}
struct Visu {
  FILE*fp;
  Visu();
  ~Visu();
  void next(string s);
  void add(Image in, Image out);
};

void plot(const vector<vector<int>>&inp, const char*filename = "out.ppm");
void print(Image img);
#include "precompiled_stl.hpp"
#include <chrono>
using namespace std;
#include "utils.hpp"
#include "read.hpp"
#include "normalize.hpp"
#include "core_functions.hpp"
#include "image_functions.hpp"
#include "image_functions2.hpp"

#include "visu.hpp"

#include "brute2.hpp"
#include "pieces.hpp"

#include "timer.hpp"

extern int MAXDEPTH, print_nodes;

//double build_f_time = 0, apply_f_time = 0;
//double real_f_time = 0;

double now() {
  ll t = chrono::high_resolution_clock::now().time_since_epoch().count();
  static ll time0 = 0;
  if (time0 == 0) time0 = t;
  return (t-time0)*1e-9;
}
//double now() { return chrono::steady_clock::now().time_since_epoch().count()*1e-9;}

Timer build_f_time, apply_f_time, real_f_time, add_time, find_child_time, add_child_time, hash_time, map_time, total_time;
Timer state_time;

void Functions3::add(const string& name, int cost_, const function<bool(const State&,State&)>&func, int list) {
  //if (cost_ != 10) cout << name << endl;
  //assert(cost_ == 10);
  if (list) listed.push_back(names.size());
  names.push_back(name);
  f_list.push_back(func);
  cost.push_back(cost_);
}

void Functions3::add(string name, int cost, const function<Image(Image_)>&f, int list) { //list = 1
  auto func = [f](const State& cur, State& nxt) {

    //if (cur.isvec) return false;

    nxt.vimg.resize(cur.vimg.size());
    nxt.isvec = cur.isvec;

    int area = 0;
    for (int i = 0; i < cur.vimg.size(); i++) {
      real_f_time.start();
      nxt.vimg[i] = f(cur.vimg[i]);
      real_f_time.stop();

      area += nxt.vimg[i].w*nxt.vimg[i].h;
      if (area > MAXPIXELS) return false;
    }
    return true;
  };
  add(name, cost, func, list);
}

void Functions3::add(string name, int cost, const function<vImage(Image_)>&f, int list) { //list = 1
  const int buffer = 5;
  auto func = [f,cost](const State& cur, State& nxt) {
    if (cur.isvec || cur.depth+cost+buffer > MAXDEPTH) return false;
    real_f_time.start();
    nxt.vimg = f(cur.vimg[0]);
    real_f_time.stop();
    nxt.isvec = true;
    return true;
  };
  add(name, cost, func, list);
}

void Functions3::add(string name, int cost, const function<Image(vImage_)>&f, int list) { //list = 1
  auto func = [f](const State& cur, State& nxt) {
    if (!cur.isvec) return false;
    nxt.vimg.resize(1);
    real_f_time.start();
    nxt.vimg[0] = f(cur.vimg);
    real_f_time.stop();
    nxt.isvec = false;
    return true;
  };
  add(name, cost, func, list);
}

void Functions3::add(string name, int cost, const function<vImage(vImage_)>&f, int list) { //list = 1
  auto func = [f](const State& cur, State& nxt) {
    if (!cur.isvec) return false;
    real_f_time.start();
    nxt.vimg = f(cur.vimg);
    real_f_time.stop();
    nxt.isvec = true;
    return true;
  };
  add(name, cost, func, list);
}

void Functions3::add(const vector<point>&sizes, string name, int cost, const function<Image(Image_,Image_)>&f, int list) { //list = 1
  int szi = 0;
  for (point sz : sizes) {
    Image arg2 = core::empty(sz);
    auto func = [f,arg2](const State& cur, State& nxt) {

      if (cur.isvec) return false;

      nxt.vimg.resize(cur.vimg.size());

      int area = 0;
      for (int i = 0; i < cur.vimg.size(); i++) {
	real_f_time.start();
	nxt.vimg[i] = f(cur.vimg[i], arg2);
	real_f_time.stop();

	area += nxt.vimg[i].w*nxt.vimg[i].h;
	if (area > MAXPIXELS) return false;
      }
      nxt.isvec = cur.isvec;
      return true;
    };
    add(name+" "+to_string(szi++), cost, func, list);
  }
}

string Functions3::getName(int fi) {
  assert(fi >= 0 && fi < names.size());
  return names[fi];
}
int Functions3::findfi(string name) {
  int fi = find(names.begin(), names.end(), name)-names.begin();
  if (fi == names.size()) {
    cerr << name << " is not a known function" << endl;
    assert(0);
  }
  return fi;
}


Functions3 initFuncs3(const vector<point>&sizes) {
  Functions3 funcs;

  // Unary

  //invert is filterCol(img, 0)
  for (int c = 0; c < 10; c++)
    funcs.add("filterCol "+to_string(c), 10, [c](Image_ img) {return filterCol(img, c);});
  for (int c = 1; c < 10; c++)
    funcs.add("eraseCol "+to_string(c), 10,
	      [c](Image_ img) {return eraseCol(img, c);});

  for (int c = 1; c < 10; c++)
    funcs.add("colShape "+to_string(c), 10,
	      [c](Image_ img) {return colShape(img, c);}, 0);

  funcs.add("compress", 10, [](Image_ img) {return compress(img);});
  funcs.add("getPos", 10, getPos);
  funcs.add("getSize0", 10, getSize0);
  funcs.add("getSize", 10, getSize);
  funcs.add("hull0", 10, hull0);
  funcs.add("hull", 10, hull);
  funcs.add("toOrigin", 10, toOrigin);
  funcs.add("Fill", 10, Fill);
  funcs.add("interior", 10, interior);
  funcs.add("interior2", 10, interior2);
  funcs.add("border", 10, border);
  funcs.add("center", 10, center);
  funcs.add("majCol", 10, majCol);

  //funcs.add("greedyFillBlack", 10, [](Image_ img) {return greedyFillBlack(img);});
  //funcs.add("greedyFillBlack2", 10, [](Image_ img) {return greedyFillBlack2(img);});

  for (int i = 1; i < 9; i++)
    funcs.add("rigid "+to_string(i), 10,
	      [i](Image_ img) {return rigid(img, i);});
  for (int a = 0; a < 3; a++)
    for (int b = 0; b < 3; b++)
      funcs.add("count "+to_string(a)+" "+to_string(b), 10,
		[a,b](Image_ img) {return count(img, a, b);});
  for (int i = 0; i < 15; i++)
    funcs.add("smear "+to_string(i), 10,
	      [i](Image_ img) {return smear(img, i);});


  funcs.add("makeBorder", 10,
	    [](Image_ img) {return makeBorder(img, 1);});

  for (int id : {0,1})
    funcs.add("makeBorder2 "+to_string(id), 10,
	      [id](Image_ img) {return makeBorder2(img, id);});
  funcs.add("compress2", 10, compress2);
  funcs.add("compress3", 10, compress3);

  for (int id = 0; id < 3; id++)
    funcs.add("connect "+to_string(id), 10,
	      [id](Image_ img) {return connect(img,id);});

  for (int id : {0,1})
    funcs.add("spreadCols "+to_string(id), 10,
	      [id](Image_ img) {return spreadCols(img, id);});

  for (int id = 0; id < 4; id++)
    funcs.add("half "+to_string(id), 10,
	      [id](Image_ img) {return half(img, id);});


  for (int dy = -2; dy <= 2; dy++) {
    for (int dx = -2; dx <= 2; dx++) {
      funcs.add("Move "+to_string(dx)+" "+to_string(dy), 10,
		[dx,dy](Image_ img) {return Move(img, Pos(dx,dy));}, 0);
    }
  }

  // Binary
  funcs.add(sizes, "embed", 10, embed);
  funcs.add(sizes, "wrap", 10, wrap);
  funcs.add(sizes, "broadcast", 10, [](Image_ a, Image_ b) {return broadcast(a,b);});
  funcs.add(sizes, "repeat 0",  10, [](Image_ a, Image_ b) {return repeat(a,b);});
  funcs.add(sizes, "repeat 1",  10, [](Image_ a, Image_ b) {return repeat(a,b,1);});
  funcs.add(sizes, "mirror 0",  10, [](Image_ a, Image_ b) {return mirror(a,b);});
  funcs.add(sizes, "mirror 1",  10, [](Image_ a, Image_ b) {return mirror(a,b,1);});


  //Split
  funcs.add("cut",       10, [](Image_ img) {return cut(img);});
  funcs.add("splitCols", 10, [](Image_ img) {return splitCols(img);});
  funcs.add("splitAll",     10, splitAll);
  funcs.add("splitColumns", 10, splitColumns);
  funcs.add("splitRows",    10, splitRows);
  funcs.add("insideMarked", 10, insideMarked);
  for (int id = 0; id < 4; id++)
    funcs.add("gravity "+to_string(id), 10,
	      [id](Image_ img) {return gravity(img,id);});


  //Join
  for (int id = 0; id < 14; id++)
    funcs.add("pickMax "+to_string(id), 10,
	      [id](vImage_ v) {return pickMax(v,id);});
  for (int id = 0; id < 1; id++)
    funcs.add("pickUnique "+to_string(id), 10,
	      [id](vImage_ v) {return pickUnique(v,id);});

  funcs.add("composeGrowing", 10, composeGrowing);
  funcs.add("stackLine", 10, stackLine);
  for (int id = 0; id < 2; id++) //consider going to 4
    funcs.add("myStack "+to_string(id), 10,
	      [id](vImage_ v) {return myStack(v,id);}); //


  //Vector
  for (int id = 0; id < 14; id++)
    funcs.add("pickMaxes "+to_string(id), 10,
	      [id](vImage_ v) {return pickMaxes(v,id);});
  for (int id = 0; id < 14; id++)
    funcs.add("pickNotMaxes "+to_string(id), 10,
	      [id](vImage_ v) {return pickNotMaxes(v,id);});


  static int said = 0;
  if (!said) {
    cout << "Function count: " << funcs.listed.size() << endl;
    said = 1;
  }


  //funcs.add("smear",    [](Image_ a, Image_ b) {return smear(a,b,6);});

  //funcs.add(insideMarked); //only do once at depth 0

  // Smear diagonals?

  // outerProducts

  //for (int id = 0; id < 4; id++)
  //  funcs.add([id](Image_ img) {return gravity(img, id);});

  // Image makeBorder(Image_ img, int bcol = 1);
  // Image makeBorder2(Image_ img, Image_ bord);
  // Image greedyFillBlack(Image_ img, int N = 3);
  // Image extend2(Image_ img, Image_ room);
  // Image replaceTemplate(Image_ in, Image_ need_, Image_ marked_, int overlapping = 0, int rigids = 0);
  // Image swapTemplate(Image_ in, Image_ a, Image_ b, int rigids = 0);

  // funcs.add("heuristicCut", heuristicCut);

  return funcs;
}

Image DAG::getImg(int ni) {
  return tiny_node.getImg(ni);
  //assert(tiny_node.getImg(ni) == node[ni].vimg[0]);
  //return node[ni].state.vimg[0];
  /*
  //cout << nodei << endl;
  assert(nodei >= 0 && nodei < (int)node.size());
  assert(!node[nodei].isvec);
  if (node[nodei].pfi == embed1fi) {
    assert(funcs.f_list[embed1fi](node[nodei], tmp_node));
    //assert(tmp_node.vimg == node[nodei].vimg);
    return tmp_node.vimg[0];
  } else {
    assert(node[nodei].vimg.size());
    return node[nodei].vimg[0];
    }*/
}


int DAG::add(const State&nxt, bool force) { //force = false
  hash_time.start();
  ull h = nxt.hash();
  hash_time.stop();
  int nodes = tiny_node.size();
  map_time.start();
  auto [nodei,inserted] = hashi.insert(h,nodes);
  //auto [it,inserted] = hashi.insert({h,nodes}); int nodei = it->second;
  //assert(inserted == tiny_inserted);
  //assert(nodei == tiny_nodei);

  map_time.stop();

  add_time.start();
  if (inserted || force) {
    bool ispiece = !nxt.isvec;
    if (!nxt.isvec && target_size != point{-1,-1})
      ispiece &= (nxt.vimg[0].p == point{0,0} && nxt.vimg[0].sz == target_size);
    /*{
      Node n;
      n.state = nxt;
      n.ispiece = ispiece;
      n.freed = false;
      node.push_back(n);
      }*/
    tiny_node.append(nxt, ispiece);
  }
  add_time.stop();
  return nodei;
}


void DAG::build() {
  build_f_time.start();

  for (int curi = 0; curi < tiny_node.size(); curi++) {
    int depth = tiny_node[curi].depth;
    if (depth+1 > MAXDEPTH) continue;

    //vector<pair<int,int>> child;
    State nxt;
    state_time.start();
    State cur_state = tiny_node.getState(curi);
    state_time.stop();
    for (int fi : funcs.listed) {
      nxt.depth = depth+funcs.cost[fi];
      if (nxt.depth > MAXDEPTH) continue;
      if (funcs.f_list[fi](cur_state, nxt)) {
	int newi = add(nxt);
	//child.emplace_back(fi, newi);
	tiny_node.addChild(curi, fi, newi);
      } else {
	tiny_node.addChild(curi, fi, -1);
	//child.emplace_back(fi, -1);
      }

    }
    //node[curi].child = child;
  }

  build_f_time.stop();
}

void DAG::initial(Image_ test_in, const vector<pair<Image,Image>>&train, vector<point> sizes, int ti) {
  if (sizes.size() > 1)
    target_size = sizes[1];
  else
    target_size = point{-1,-1};

  Image in = ti < train.size() ? train[ti].first : test_in;

  add(State({in}, false, 0), true);

  //Output sizes
  for (point sz : sizes)
    add(State({core::empty(sz)}, false, 10), true);

  // Outputs of other trains
  for (int tj = 0; tj < train.size(); tj++)
    add(State({ti != tj ? train[tj].second : core::empty(train[tj].second.sz)}, false, 10), true);

  //add(State({greedyFillBlack2(in)}, false, 10), true);

  //filterCol?

  givens = tiny_node.size();
}

//time each function
void DAG::benchmark() {
  vector<pair<double,int>> v;
  for (int fi : funcs.listed) {
    double start_time = now();
    State nxt;
    for (int i = 0; i < tiny_node.size(); i++) {
      funcs.f_list[fi](tiny_node.getState(i), nxt);
    }
    double elapsed = now()-start_time;
    v.emplace_back(elapsed, fi);
  }
  sort(v.begin(), v.end());
  for (auto [t,fi] : v)
    printf("%.1f ms - %s\n", t*1e3, funcs.getName(fi).c_str());
}

int DAG::applyFunc(int curi, int fi, const State&state) {

  find_child_time.start();
  //auto it = lower_bound(node[curi].child.begin(), node[curi].child.end(), make_pair(fi,-1));
  int it2 = tiny_node.getChild(curi, fi);
  find_child_time.stop();
  if (it2 != TinyChildren::None) {//it != node[curi].child.end() && it->first == fi) {
    //if (it2 != it->second) cout << it2 << ' ' << it->second << endl;
    //assert(it2 == it->second);
    return it2;//it->second;
  }
  //assert(it2 == -2);


  State nxt;
  nxt.depth = tiny_node[curi].depth+funcs.cost[fi];
  //nxt.par = curi;

  int newi = -1;

  apply_f_time.start();
  bool ok = funcs.f_list[fi](state, nxt);
  apply_f_time.stop();

  if (ok) {
    //nxt.pfi = fi;
    newi = add(nxt);
  }

  add_child_time.start();
  tiny_node.addChild(curi, fi, newi);
  //node[curi].child.emplace_back(fi, newi);
  //sort(node[curi].child.begin(), node[curi].child.end());
  add_child_time.stop();
  return newi;
}

int DAG::applyFunc(int curi, int fi) {
  state_time.start();
  State state = tiny_node.getState(curi);
  state_time.stop();
  return applyFunc(curi, fi, state);
}

void DAG::applyFunc(string name, bool vec) {
  int fi = funcs.findfi(name);

  int start_nodes = tiny_node.size();
  for (int curi = 0; curi < start_nodes; curi++) {
    if (tiny_node[curi].isvec == vec) applyFunc(curi, fi);
  }
}

void DAG::applyFuncs(vector<pair<string,int>> names, bool vec) {
  vector<pair<int,int>> fis;
  for (auto [name,id] : names) {
    fis.emplace_back(funcs.findfi(name), id);
  }

  vector<int> parid(tiny_node.size(),-1);
  for (int curi = 0; curi < tiny_node.size(); curi++) {
    if (tiny_node[curi].isvec != vec) continue;
    state_time.start();
    State state = tiny_node.getState(curi);
    state_time.stop();

    for (int i = 0; i < fis.size(); i++) {
      auto [fi,id] = fis[i];
      if (id <= parid[curi]) continue;

      if (applyFunc(curi, fi, state) == parid.size()) {
	parid.push_back(id);
	assert(parid.size() == tiny_node.size());
      }
    }
  }
}





void DAG::buildBinary() {
  int fis = *max_element(funcs.listed.begin(), funcs.listed.end())+1;
  binary.assign(fis*fis, -1);
  vector<State> state(fis);
  vector<int> active(fis), memi(fis);
  for (int fi : funcs.listed) {
    int curi = tiny_node.getChild(0, fi);
    if (curi >= 0) {
      active[fi] = 1;
      state[fi] = tiny_node.getState(curi);
      memi[fi] = curi;
    }
  }
  for (int fa : funcs.listed) {
    if (!active[fa]) continue;
    for (int fb : funcs.listed) {
      if (!active[fb]) continue;

      if (state[fa].isvec || state[fb].isvec) continue;
      State nxt;
      nxt.vimg = {align(state[fa].vimg[0], state[fb].vimg[0])};
      nxt.depth = 2; //TODO
      nxt.isvec = false;
      binary[fa*fis+fb] = add(nxt);
      /*if (binary[fa*fis+fb] != memi[fa]) {
	cout << binary[fa*fis+fb] << ' ' << memi[fa] << ' ' << tiny_node.size() << endl;
      }
      assert(binary[fa*fis+fb] == memi[fa]);*/
    }
  }
}



vector<DAG> brutePieces2(Image_ test_in, const vector<pair<Image,Image>>&train, vector<point> out_sizes) {
  int print = 1;

  vector<DAG> dag(train.size()+1);

  int all_train_out_mask = 0, and_train_out_mask = ~0;
  for (int ti = 0; ti < train.size(); ti++)
    and_train_out_mask &= core::colMask(train[ti].second);

  for (int ti = 0; ti <= train.size(); ti++) {
    vector<point> sizes;
    if (ti < train.size())
      sizes.push_back(train[ti].first.sz);
    else
      sizes.push_back(test_in.sz);

    if (out_sizes.size())
      sizes.push_back(out_sizes[ti]);

    dag[ti].funcs = initFuncs3(sizes);

    dag[ti].initial(test_in, train, sizes, ti);

    total_time.start();

    double start_time = now();
    dag[ti].build();
    if (print) cout << now()-start_time << endl;
    //dag[ti].buildBinary();
    //if (print) cout << now()-start_time << endl;
    dag[ti].applyFunc("composeGrowing", 1);
    if (print) cout << now()-start_time << endl;

    if (sizes.size() > 1) {
      vector<pair<string,int>> toapply;
      toapply.emplace_back("toOrigin",0);
      for (int c = 1; c <= 5; c++)
	if (and_train_out_mask>>c&1)
	  toapply.emplace_back("colShape "+to_string(c),1);
      toapply.emplace_back("embed 1",2);
      dag[ti].applyFuncs(toapply, 0);
      /*
      dag[ti].applyFunc("toOrigin", 0);
      if (print) cout << now()-start_time << endl;

      int mask;
      if (ti < train.size()) {
	mask = core::colMask(train[ti].second);
	all_train_out_mask |= mask;
      } else {
	mask = all_train_out_mask;
      }

      for (int c = 1; c <= 5; c++)
	if (and_train_out_mask>>c&1)
	dag[ti].applyFunc("colShape "+to_string(c), 0);
      if (print) cout << now()-start_time << endl;

      if (ti < train.size())
	deducePositions(dag[ti], train[ti].second);
      if (print) cout << now()-start_time << endl;

      dag[ti].applyFunc("embed 1", 0);
      */
      if (print) cout << now()-start_time << endl;

      /*if (ti < train.size())
	deducePositions(dag[ti], train[ti].second);

	if (print) cout << now()-start_time << endl;*/

      total_time.stop();
      total_time.print("Total time");
      build_f_time.print("Build f time");
      apply_f_time.print("Apply f time");
      real_f_time .print("Real f time ");

      add_time.print("Add time");
      find_child_time.print("Find child");
      add_child_time.print("Add child");
      hash_time.print("Hash");
      map_time.print("Map");

      state_time.print("getState");
      //exit(0);
      /*FILE*fp = fopen("images.txt", "w");
      for (Node&n : dag[ti].node) {
	for (Image_ img : n.vimg) {
	  fprintf(fp, "%d %d %d %d\n", img.x, img.y, img.w, img.h);
	  for (char c : img.mask)
	    fprintf(fp, "%c", '0'+c);
	  fprintf(fp, "\n");
	}
      }
      exit(0);*/
      //dag[ti].freeAll();
    } else total_time.stop();
    //dag[ti].benchmark();
    //exit(0);

    /*
    for (Node&n : dag[ti].node) {
      if (!n.isvec && n.pfi == dag[ti].embed1fi) {
	n.vimg.clear();
	n.vimg.shrink_to_fit();
      }
    }
    */

    /*if (ti < train.size()) {
      for (Node&n : dag[ti].node) {
	if (n.par > -1 && (n.isvec || n.img[0].w > 30 || n.img[0].h > 30)) {// || n.img[0].p != point{0,0} || n.img[0].sz != given_sizes[ti][1])) {
	  n.img.clear();
	  n.img.shrink_to_fit();
	}
      }
      dag[ti].hashi.clear();
      }*/
  }


  if (out_sizes.size() && print_nodes) {
    cout << "Dag sizes: ";
    for (int i = 0; i < dag.size(); i++)
      cout << dag[i].tiny_node.size() << " ";
    cout << endl;
  }

  return dag;
}
#include "precompiled_stl.hpp"
using namespace std;
#include "utils.hpp"
#include "core_functions.hpp"
#include "image_functions.hpp"
#include "image_functions2.hpp"
#include "visu.hpp"

#include "brute2.hpp"
#include "pieces.hpp"
#include "compose2.hpp"

extern int MAXDEPTH;

bool operator<(const point a, const point b) {
  if (a.x != b.x) return a.x < b.x;
  return a.y < b.y;
}

pair<vector<int>,double> solveSingle(vector<vector<int>>&seeds, const vector<int>& target) {
  int n = target.size()+1;
  vector<int> ans(n,1);
  pair<int,double> best = {-1,1e9};

  auto add = [&](const vector<int>&szs, double loss) {
    int oks = 0;
    for (int ti = 0; ti < target.size(); ti++)
      oks += (szs[ti] == target[ti]);
    pair<int,double> cand = {oks, -loss-10};
    int sz = szs.back();
    if (sz >= 1 && sz <= 30 &&
	cand > best) {
      best = cand;
      ans = szs;
    }
  };

  for (int w = 1; w <= 30; w++) {
    add(vector<int>(n,w), w);
  }

  vector<int> szs(n);
  for (int i = 0; i < seeds.size(); i++) {
    double a = i+1;
    for (int w = 1; w < 6; w++) {
      for (int x = -3; x <= 3; x++) {
	for (int k = 0; k < n; k++)
	  szs[k] = seeds[i][k]*w+x;
	add(szs, a*w*(abs(x)+1));
      }
    }
  }

  return {ans,best.second};
}

point solveSize(vector<vector<point>>&seeds, const vector<point>& target) {
  point ans = {1,1};
  pair<int,double> best = {-1,1e9};

  auto add = [&](const vector<point>&szs, double loss) {
    int oks = 0;
    for (int ti = 0; ti < target.size(); ti++)
      oks += (szs[ti] == target[ti]);
    pair<int,double> cand = {oks, -loss};
    point sz = szs.back();
    if (sz.x >= 1 && sz.x <= 30 &&
	sz.y >= 1 && sz.y <= 30 &&
	cand > best) {
      best = cand;
      ans = sz;
    }
  };

  int n = target.size()+1;
  vector<point> szs(n);

  for (int i = 0; i < seeds.size(); i++) {
    double a = i+1;
    for (int h = 1; h < 6; h++) {
      for (int w = 1; w < 6; w++) {
	for (int y = -3; y <= 3; y++) {
	  for (int x = -3; x <= 3; x++) {
	    for (int k = 0; k < n; k++)
	      szs[k] = {seeds[i][k].x*w+x, seeds[i][k].y*h+y};
	    add(szs, a*w*h*(abs(x)+1)*(abs(y)+1));
	  }
	}
      }
    }
  }

  if (1) {//best.first < target.size()) {
    for (int i = 0; i < seeds.size(); i++) {
      double a = i+1;
      for (int j = 0; j < i; j++) {
	double b = j+1;
	for (int d = 0; d < 3; d++) {
	  for (int k = 0; k < n; k++) {
	    szs[k] = seeds[i][k];
	    if (d == 0 || d == 2) szs[k].x = szs[k].x+seeds[j][k].x;
	    if (d == 1 || d == 2) szs[k].y = szs[k].y+seeds[j][k].y;
	  }
	  add(szs, a*b);
	}
      }
    }
  }

  {
    vector<vector<int>> seedsx, seedsy;
    for (vector<point>&seed : seeds) {
      vector<int> seedx, seedy;
      for (point p : seed) {
	seedx.push_back(p.x);
	seedy.push_back(p.y);
      }
      seedsx.push_back(seedx);
      seedsy.push_back(seedy);
    }
    vector<int> targetx, targety;
    for (point p : target) {
      targetx.push_back(p.x);
      targety.push_back(p.y);
    }
    auto [bestx, scorex] = solveSingle(seedsx, targetx);
    auto [besty, scorey] = solveSingle(seedsy, targety);

    vector<point> combined;
    for (int i = 0; i < n; i++) {
      combined.push_back({bestx[i], besty[i]});
    }
    double combloss = scorex*scorey;
    add(combined, combloss);
  }

  //cout << best.first << ' ' << target.size() << ' ' << best.second << endl;
  //assert(best.first == target.size());
  return ans;
}

vector<point> bruteSize(Image_ test_in, vector<pair<Image,Image>> train) {
  vector<point> out_sizes;
  for (auto [in,out] : train) {
    out_sizes.push_back(out.sz);
  }
  int cp = MAXDEPTH;
  MAXDEPTH = min(MAXDEPTH, 30);
  Pieces pieces;
  {
    vector<DAG> dags = brutePieces2(test_in, train, {});
    pieces = makePieces2(dags, train, {});
  }
  int dags = pieces.dag.size();

  MAXDEPTH = cp;

  vector<point> target;
  for (auto [in,out] : train) target.push_back(out.sz);

  //cout << pieces.piece.size() << endl;
  //cout << pieces.seen.size() << endl;
  vector<vector<point>> seeds;
  set<vector<point>> seen;
  for (Piece3&p : pieces.piece) {
    vector<point> sz;
    int ok = 1;
    int*ind = &pieces.mem[p.memi];
    for (int ti = 0; ti <= train.size(); ti++) {
      if (pieces.dag[ti].tiny_node[ind[ti]].isvec) ok = 0;
      else {
	sz.push_back(pieces.dag[ti].getImg(ind[ti]).sz);
      }
    }
    if (ok) {
      if (seen.insert(sz).second)
	seeds.push_back(sz);
    }
  }

  point ans = solveSize(seeds, target);
  out_sizes.push_back(ans);
  return out_sizes;
}

vector<point> cheatSize(Image_ test_out, vector<pair<Image,Image>> train) {
  vector<point> out_sizes;
  for (auto [in,out] : train) {
    out_sizes.push_back(out.sz);
  }
  out_sizes.push_back(test_out.sz);
  return out_sizes;
}
#include "precompiled_stl.hpp"
using namespace std;
#include "utils.hpp"
#include "core_functions.hpp"
#include "image_functions.hpp"
#include "image_functions2.hpp"
#include "visu.hpp"

#include "brute2.hpp"
#include "pieces.hpp"
#include "compose2.hpp"

#include "timer.hpp"

/*
struct Candidate {
  vector<Image> imgs;
  double score;
};
*/


extern int print_times;


struct mybitset {
  vector<ull> data;
  mybitset(int n) {
    data.resize((n+63)/64);
  }
  int operator[](int i) {
    return data[i>>6]>>(i&63)&1;
  }
  void set(int i, ull v) {
    int bit = i&63;
    data[i>>6] &= ~(1ull<<bit);
    data[i>>6] |= (v<<bit);
  }
  ull hash() {
    ull r = 1;
    for (ull h : data) {
      r = r*137139+h;
    }
    return r;
  }
};



int popcount64c(ull x) {
  const uint64_t m1  = 0x5555555555555555; //binary: 0101...
  const uint64_t m2  = 0x3333333333333333; //binary: 00110011..
  const uint64_t m4  = 0x0f0f0f0f0f0f0f0f; //binary:  4 zeros,  4 ones ...
  const uint64_t m8  = 0x00ff00ff00ff00ff; //binary:  8 zeros,  8 ones ...
  const uint64_t m16 = 0x0000ffff0000ffff; //binary: 16 zeros, 16 ones ...
  const uint64_t m32 = 0x00000000ffffffff; //binary: 32 zeros, 32 ones
  const uint64_t h01 = 0x0101010101010101; //the sum of 256 to the power of 0,1,2,3...
  x -= (x >> 1) & m1;             //put count of each 2 bits into those 2 bits
  x = (x & m2) + ((x >> 2) & m2); //put count of each 4 bits into those 4 bits
  x = (x + (x >> 4)) & m4;        //put count of each 8 bits into those 8 bits
  return (x * h01) >> 56;  //returns left 8 bits of x + (x<<8) + (x<<16) + (x<<24) + ...
}

int popcount64d(ull x) {
  int pop = 0;
  while (x) {
    x &= x-1;
    pop++;
  }
  return pop;
}



vector<Candidate> greedyCompose2(Pieces&pieces, vector<Image>&target, vector<point> out_sizes) {
  if (pieces.piece.empty()) return {};

  Timer greedy_fill_time;

  {
    int d = -1;
    for (Piece3&p : pieces.piece) {
      assert(p.depth >= d);
      d = p.depth;
    }
  }

  vector<Image> init;
  vector<int> sz;
  {
    for (int i = 0; i < pieces.dag.size(); i++) {
      if (i < target.size()) assert(out_sizes[i] == target[i].sz);
      init.push_back(core::full(out_sizes[i], 10));
      sz.push_back(init.back().mask.size());
    }
  }

  vector<Candidate> rets;

  int n = pieces.piece.size();

  int M = 0;
  for (int s : sz) M += s;

  const int M64 = (M+63)/64;
  vector<ull> bad_mem, active_mem;
  //vector<mybitset> bad_mem, active_mem;
  vector<int> img_ind, bad_ind, active_ind;

  {
    active_mem.reserve(n*M64);
    bad_mem.reserve(n*M64);
    TinyHashMap seen;
    mybitset badi(M), blacki(M);
    for (int i = 0; i < n; i++) {
      int x = 0, y = 0;
      for (int j = 0; j < sz.size(); j++) {
	int*ind = &pieces.mem[pieces.piece[i].memi];
	Image_ img = pieces.dag[j].getImg(ind[j]);
	const vector<char>&p = img.mask;
	const vector<char>&t = j < target.size() ? target[j].mask : init[j].mask;
	assert(p.size() == sz[j]);
	assert(t.size() == sz[j]);
	for (int k = 0; k < sz[j]; k++) {
	  badi.set  (x++, (p[k] != t[k]));
	  blacki.set(y++, (p[k] == 0));
	}
      }
      img_ind.push_back(i);

      active_ind.push_back(active_mem.size());
      for (ull v : blacki.data)
	active_mem.push_back(v);

      bad_ind.push_back(bad_mem.size());
      for (ull v : badi.data)
	bad_mem.push_back(v);
    }
  }

  //cout << active_mem.size() << endl;
  //cout << bad_mem.size() << endl;
  //exit(0);

  int max_piece_depth = 0;
  for (int i = 0; i < n; i++)
    max_piece_depth = max(max_piece_depth, pieces.piece[i].depth);

  //mt19937 mrand(0);



  auto greedyComposeCore = [&](mybitset&cur, const mybitset&careMask, const int piece_depth_thres, vImage&ret) {
    vector<int> sparsej;
    for (int j = 0; j < M64; j++) {
      if (~cur.data[j] & careMask.data[j]) sparsej.push_back(j);
    }

    vector<ull> best_active(M64), tmp_active(M64);
    int besti = -1;
    pair<int,int> bestcnt = {0,0};
    //for (int i : order) {
    //  if (skip[i] || pieces.piece[img_ind[i]].depth > piece_depth_thres) continue;
    for (int i = 0; i < img_ind.size(); i++) {
      if (pieces.piece[img_ind[i]].depth > piece_depth_thres) continue;

      for (int k : {0,1,2}) {
	const ull*active_data = &active_mem[active_ind[i]];

	ull flip = (k == 0 ? ~0 : 0);
	ull full = (k == 2 ? ~0 : 0);

	const ull*bad_data = &bad_mem[bad_ind[i]];
	int cnt = 0, covered = 0, ok = 1;
	for (int j = 0; j < M64; j++) {
	  ull active = ((active_data[j]^flip) | full);
	  if (~cur.data[j] & bad_data[j] & active) {
	    ok = 0;
	    break;
	  }
	}
	if (!ok) continue;
	for (int j : sparsej) {
	  ull active = ((active_data[j]^flip) | full);
	  cnt += popcount64d(active & ~cur.data[j] & careMask.data[j]);
	  //covered += popcount64d(bad.data[j] & cur.data[j] & careMask.data[j]);
	}
	if (ok && make_pair(cnt,-covered) > bestcnt) {
	  bestcnt = make_pair(cnt,-covered);
	  besti = i;

	  if (k == 0) {
	    for (int j = 0; j < M64; j++)
	      tmp_active[j] = ~active_data[j];
	  } else if (k == 1) {
	    for (int j = 0; j < M64; j++)
	      tmp_active[j] = active_data[j];
	  } else {
	    for (int j = 0; j < M64; j++)
	      tmp_active[j] = ~0;
	  }
	  best_active = tmp_active;
	}
      }
    }

    if (besti == -1) return -1;

    {
      int i = img_ind[besti], x = 0;

      int depth = pieces.piece[i].depth;

      for (int l = 0; l < ret.size(); l++) {
	int*ind = &pieces.mem[pieces.piece[i].memi];
	const vector<char>&mask = pieces.dag[l].getImg(ind[l]).mask;
	for (int j = 0; j < sz[l]; j++) {
	  if ((best_active[x>>6]>>(x&63)&1) && ret[l].mask[j] == 10) {
	    //assert(cur[x-1] == 0);
	    ret[l].mask[j] = mask[j];
	  }

	  x++;
	}
      }
      for (int j = 0; j < M; j++) {
	if (best_active[j>>6]>>(j&63)&1) cur.set(j, 1);
      }

      return depth;
    }
  };









  map<ull,Image> greedy_fill_mem;

  int maxiters = 10;


  //vector<int> skip(img_ind.size());
  //vector<int> order;
  //for (int i = 0; i < img_ind.size(); i++) order.push_back(i);

  for (int pdt = max_piece_depth%10; pdt <= max_piece_depth; pdt += 10) {
    int piece_depth_thres = pdt;

    for (int it0 = 0; it0 < 10; it0++) {
      for (int mask = 1; mask < min(1<<target.size(), 1<<5); mask++) {
	vector<int> maskv;
	for (int j = 0; j < target.size(); j++)
	  if (mask>>j&1) maskv.push_back(j);

	int caremask;
	if (it0 < maskv.size()) {
	  caremask = 1<<maskv[it0];
	} else {
	  continue;
	  /*caremask = 1<<maskv[mrand()%maskv.size()];
	    for (int i = 0; i < n; i++)
	    skip[i] = mrand()%3;
	    random_shuffle(order.begin(), order.end());*/
	}

	mybitset cur(M), careMask(M);
	{
	  int base = 0;
	  for (int j = 0; j < sz.size(); j++) {
	    if (!(mask>>j&1))
	      for (int k = 0; k < sz[j]; k++)
		cur.set(base+k, 1);
	    if ((caremask>>j&1))
	      for (int k = 0; k < sz[j]; k++)
		careMask.set(base+k, 1);
	    base += sz[j];
	  }
	}


	int cnt_pieces = 0;
	vector<int> piece_depths;
	int sum_depth = 0, max_depth = 0;

	vector<Image> ret = init;
	for (int it = 0; it < maxiters; it++) {

	  int depth = greedyComposeCore(cur, careMask, piece_depth_thres, ret);
	  if (depth == -1) break;
	  piece_depths.push_back(depth);
	  cnt_pieces++;
	  sum_depth += depth;
	  max_depth = max(max_depth, depth);


	  {
	    greedy_fill_time.start();
	    vImage cp = ret;
	    int carei = 31-__builtin_clz(caremask);
	    assert(caremask == 1<<carei);
	    int ok = 1;
	    {
	      Image& img = cp[carei];
	      for (char&c : img.mask) if (c == 10) c = 0;
	      img = greedyFillBlack(img);
	      if (img != target[carei]) ok = 0;
	    }
	    if (ok) {
	      for (int i = 0; i < cp.size(); i++) {
		if (i == carei) continue;
		Image& img = cp[i];
		for (char&c : img.mask) if (c == 10) c = 0;
		ull h = hashImage(img);
		if (!greedy_fill_mem.count(h)) {
		  greedy_fill_mem[h] = greedyFillBlack(img);
		}
		img = greedy_fill_mem[h];
		if (img.w*img.h <= 0) ok = 0;
	      }
	      if (ok)
		rets.emplace_back(cp, cnt_pieces+1, sum_depth, max_depth);
	    }
	    greedy_fill_time.stop();
	  }
	}

	/*for (Image&img : ret)
	  for (char&c : img.mask)
	  if (c == 10) c = 0;
	*/

	rets.emplace_back(ret, cnt_pieces, sum_depth, max_depth);
      }
    }
  }

  if (print_times)
    greedy_fill_time.print("Greedy fill time");

  return rets;
}



vector<Candidate> composePieces2(Pieces&pieces, vector<pair<Image, Image>> train, vector<point> out_sizes) {
  vector<Candidate> cands;

  vector<Image> target;
  for (auto [in,out] : train)
    target.push_back(out);

  /*
  for (Piece3&p : pieces.piece) {
    vector<Image> imgs;
    //assert(p.ind.size() == pieces.dag.size());
    int*ind = &pieces.mem[p.memi];
    for (int i = 0; i < pieces.dag.size(); i++) {
      //assert(p.ind[i] >= 0 && p.ind[i] < (int)pieces.dag[i].node.size());
      imgs.push_back(pieces.dag[i].getImg(ind[i]));
    }
    cands.emplace_back(imgs, 1, p.depth, p.depth);
  }
  */

  for (const Candidate&cand : greedyCompose2(pieces, target, out_sizes)) {
    cands.push_back(cand);
  }
  return cands;
}


vector<Candidate> evaluateCands(const vector<Candidate>&cands, vector<pair<Image,Image>> train) {
  vector<Candidate> ret;
  for (const Candidate& cand : cands) {
    vImage_ imgs = cand.imgs;
    assert(cand.max_depth >= 0 && cand.max_depth < 100);
    assert(cand.cnt_pieces >= 0 && cand.cnt_pieces < 100);
    //cout << cand.max_depth << ' ' << cand.cnt_pieces << endl;
    double prior = cand.max_depth+cand.cnt_pieces*1e-3;//cnt_pieces;

    int goods = 0;
    for (int i = 0; i < train.size(); i++) {
      goods += (imgs[i] == train[i].second);
    }
    double score = goods-prior*0.01;

    Image answer = imgs.back();
    if (answer.w > 30 || answer.h > 30 || answer.w*answer.h == 0) goods = 0;
    for (int i = 0; i < answer.h; i++)
      for (int j = 0; j < answer.w; j++)
	if (answer(i,j) < 0 || answer(i,j) >= 10) goods = 0;

    if (goods)
      ret.emplace_back(imgs, score);
  }
  sort(ret.begin(), ret.end());
  //printf("%.20f\n\n", ret[0].score);
  return ret;
}
/*#include <vector>
#include <cassert>
#include <iostream>
#include <tuple>
#include <functional>*/
#include "precompiled_stl.hpp"
using namespace std;

#include "utils.hpp"
#include "core_functions.hpp"

namespace core {
  int colMask(Image_ img) {
    int mask = 0;
    for (int i = 0; i < img.h; i++)
      for (int j = 0; j < img.w; j++)
	mask |= 1<<img(i,j);
    return mask;
  }
  int countCols(Image_ img, int include0) {//include0 = 0
    int mask = colMask(img);
    if (!include0) mask = mask&~1;
    return __builtin_popcount(mask);
  }
  int count(Image_ img) {
    int ans = 0;
    for (int i = 0; i < img.h; i++)
      for (int j = 0; j < img.w; j++)
	ans += img(i,j) > 0;
    return ans;
  }


  Image full(point p, point sz, int filling) {//filling = 1
    Image ret;
    ret.p = p;
    ret.sz = sz;
    ret.mask.assign(ret.h*ret.w, filling);
    return ret;
  }

  Image empty(point p, point sz) {
    return full(p, sz, 0);
  }

  Image full(point sz, int filling) {//filling = 1
    Image ret;
    ret.p = {0,0};
    ret.sz = sz;
    ret.mask.assign(ret.h*ret.w, filling);
    return ret;
  }

  Image empty(point sz) {
    return full(point{0,0}, sz, 0);
  }


  bool isRectangle(Image_ a) {
    return count(a) == a.w*a.h;
  }

  void countComponents_dfs(Image&img, int r, int c) {
    img(r,c) = 0;
    for (int nr = r-1; nr <= r+1; nr++)
      for (int nc = c-1; nc <= c+1; nc++)
	if (nr >= 0 && nr < img.h && nc >= 0 && nc < img.w && img(nr,nc))
	  countComponents_dfs(img,nr,nc);
  }

  int countComponents(Image img) {
    int ans = 0;
    for (int i = 0; i < img.h; i++) {
      for (int j = 0; j < img.w; j++) {
	if (img(i,j)) {
	  countComponents_dfs(img,i,j);
	  /*function<void(int,int)> dfs = [&](int r, int c) {
	    if (r < 0 || r >= img.h || c < 0 || c >= img.w || !img(r,c)) return;
	    img(r,c) = 0;
	    for (int nr : {r-1,r,r+1})
	      for (int nc : {c-1,c,c+1})
		dfs(nr,nc);
	  };
	  dfs(i,j);*/
	  ans++;
	}
      }
    }
    return ans;
  }


  char majorityCol(Image_ img, int include0) { //include0 = 0
    int cnt[10] = {};
    for (int i = 0; i < img.h; i++)
      for (int j = 0; j < img.w; j++) {
	char c = img(i,j);
	if (c >= 0 && c < 10)
	  cnt[c]++;
      }
    if (!include0) cnt[0] = 0;
    int ret = 0;
    int ma = cnt[ret];
    for (int c = 1; c < 10; c++) {
      if (cnt[c] > ma) {
	ma = cnt[c];
	ret = c;
      }
    }
    return ret;
  }
  Image subImage(Image_ img, point p, point sz) {
    assert(p.x >= 0 && p.y >= 0 && p.x+sz.x <= img.w && p.y+sz.y <= img.h && sz.x >= 0 && sz.y >= 0);
    Image ret;
    ret.p = p+img.p;
    ret.sz = sz;
    ret.mask.resize(ret.w*ret.h);
    for (int i = 0; i < ret.h; i++)
      for (int j = 0; j < ret.w; j++)
	ret(i,j) = img(i+p.y, j+p.x);
    return ret;
  }

  vector<pair<Image,int>> splitCols(Image_ img, int include0) { //include0 = 0
    vector<pair<Image,int>> ret;
    int mask = colMask(img);
    for (char c = !include0; c < 10; c++) {
      if (mask>>c&1) {
	Image s = img;
	for (int i = 0; i < s.h; i++)
	  for (int j = 0; j < s.w; j++)
	    s(i,j) = s(i,j) == c;
	ret.emplace_back(s, c);
      }
    }
    return ret;
  }
};
#include "precompiled_stl.hpp"

using namespace std;

#include "utils.hpp"
#include "core_functions.hpp"
#include "image_functions.hpp"
#include "visu.hpp"
#include "read.hpp"

int main() {
  vector<Sample> sample = readAll("test", -1);
  cout << sample.size() << endl;
}
#include "precompiled_stl.hpp"
using namespace std;
#include "utils.hpp"
#include "core_functions.hpp"
#include "image_functions.hpp"

#include "visu.hpp"
#include "brute2.hpp"
#include "pieces.hpp"
#include "compose2.hpp"

#include "deduce_op.hpp"

pair<Image,Image> iOuterProductSI(Image_ img, int w, int h) {
  if (img.w*img.h <= 0 || img.w%w || img.h%h) return {badImg,badImg};
  Image big = core::full({img.w/w,img.h/h},-1);
  Image small = core::full({w,h},-1);

  for (int ii = 0; ii < img.h/h; ii++) {
    for (int jj = 0; jj < img.w/w; jj++) {
      int all0 = 1;
      for (int i = 0; i < h; i++)
	for (int j = 0; j < w; j++)
	  if (img(ii*h+i,jj*w+j)) all0 = 0;

      big(ii,jj) = !all0;

      if (!all0) {
	for (int i = 0; i < h; i++) {
	  for (int j = 0; j < w; j++) {
	    char& a = small(i,j);
	    char b = img(ii*h+i,jj*w+j);
	    if (a != -1 && a != b) return {badImg,badImg};
	    a = b;
	  }
	}
      }
    }
  }
  return {big, small};
}


pair<Image,Image> iOuterProductIS(Image_ img, int w, int h) {
  if (img.w*img.h <= 0 || img.w%w || img.h%h) return {badImg,badImg};
  Image big = core::full({img.w/w,img.h/h},-1);
  Image small = core::full({w,h},-1);

  for (int ii = 0; ii < img.h/h; ii++) {
    for (int jj = 0; jj < img.w/w; jj++) {
      int mask = 0;
      for (int i = 0; i < h; i++)
	for (int j = 0; j < w; j++)
	  mask |= 1<<img(ii*h+i,jj*w+j);

      if (__builtin_popcount(mask&~1) > 1) return {badImg,badImg};
      big(ii,jj) = 31-__builtin_clz(mask);
      if (big(ii,jj)) {
	for (int i = 0; i < h; i++) {
	  for (int j = 0; j < w; j++) {
	    char& a = small(i,j);
	    char b = img(ii*h+i,jj*w+j) > 0;
	    if (a != -1 && a != b) return {badImg,badImg};
	    a = b;
	  }
	}
      }
    }
  }
  return {big, small};
}


deduceOuterProduct::deduceOuterProduct(vector<pair<Image,Image>> train) {

  auto score = [](vector<pair<Image,Image>> pa, int fi) {
    double ans = 0;
    for (int k : {0,1}) {
      vector<Image> imgs;
      int allequal = 1;
      for (int ti = 0; ti < pa.size(); ti++) {
	Image img = k ? pa[ti].second : pa[ti].first;
	if (img.w*img.h <= 0) return 1e9;

	imgs.push_back(img);
	if (imgs[0] != imgs.back()) allequal = 0;
      }
      if (allequal && imgs.size() > 1) continue;

      for (Image_ img : imgs) {
	int cols = __builtin_popcount(core::colMask(img)&~1);
	if (cols <= 1 && core::isRectangle(img)) {
	  ans += log(img.w+1)+log(img.h+1);
	} else if (cols <= 1) {
	  ans += log(2)*img.w*img.h;
	} else {
	  ans += log(10)*img.w*img.h;
	}
      }
      //ans *= 1-1e-5*1;//(fi*2-1);
    }
    return ans;
  };


  int minw = 1e9, minh = 1e9;
  for (auto [in,out] : train) {
    minw = min(minw, out.w);
    minh = min(minh, out.h);
  }

  /*{
    Image img = train[0].second;
    auto [a,b] = iOuterProductSI(img,img.w,img.h);
    assert(reconstruct(a,b) == img);
    exit(0);
    }*/

  rec_funci = -1;
  double best_score = 1e9;
  for (int h = 1; h <= minh; h++) {
    for (int w = 1; w <= minw; w++) {
      for (int l : {0,1}) {
	for (int k : {0,1}) {
	  auto f = k ? iOuterProductSI : iOuterProductIS;
	  vector<pair<Image,Image>> is;
	  int bad = 0;
	  for (auto [in,out] : train) {
	    int sw = w, sh = h;
	    if (l) {
	      if (out.w%w || out.h%h) bad = 1;
	      sw = out.w/w;
	      sh = out.h/h;
	    }
	    is.push_back(f(out, sw, sh));
	  }
	  double entropy = score(is,k);
	  if (entropy < best_score) {
	    best_score = entropy;
	    rec_funci = k;
	    train_targets = is;
	  }
	}
      }
    }
  }
  assert(rec_funci != -1);

  for (int k : {0,1}) {
    auto f = k ? iOuterProductSI : iOuterProductIS;
    vector<double> best_at(train.size(), 1e9);
    vector<pair<Image,Image>> best_single(train.size(), {badImg,badImg});
    for (int ti = 0; ti < train.size(); ti++) {
      Image target = train[ti].second;
      for (int h = 1; h <= target.h; h++) {
	for (int w = 1; w <= target.w; w++) {
	  auto is = f(target, w, h);
	  double entropy = score({is},k);
	  if (entropy < best_at[ti]) {
	    best_at[ti] = entropy;
	    best_single[ti] = is;
	  }
	}
      }
    }
    double entropy = score(best_single,k);
    if (entropy < best_score) {
      best_score = entropy;
      rec_funci = k;
      train_targets = best_single;
    }
  }

  assert(rec_funci != -1);
  assert(train_targets.size() == train.size());
  for (int ti = 0; ti < train.size(); ti++) {
    Image a, b;
    tie(a,b) = train_targets[ti];
    //print(a);
    //print(b);
    //print(train[ti].second);
    assert(reconstruct(a,b) == train[ti].second);
    //cout << "OK" << endl;
    }
}


Image deduceOuterProduct::reconstruct(Image_ a, Image_ b) {
  auto f = rec_funci ? outerProductSI : outerProductIS;
  return f(a,b);
}

extern int MAXDEPTH;

void addDeduceOuterProduct(Pieces&pieces, vector<pair<Image,Image>> train, vector<Candidate>&cands) {
  deduceOuterProduct deduce_op(train);

  int interestings = 0;
  for (auto [in,out] : deduce_op.train_targets) {
    if (core::count(in) > 1 && core::count(out) > 1) interestings++;
  }
  if (interestings*2 < train.size()) return;

  vImage a, b;
  for (int k : {0,1}) {
    vImage&cand = k ? b : a;
    int best_match = -1;

    auto add = [&](vImage_ vi) {
      int matches = 0;
      for (int i = 0; i < train.size(); i++) {
	Image_ target = k ? deduce_op.train_targets[i].second : deduce_op.train_targets[i].first;
	matches += (vi[i] == target);
      }
      if (matches > best_match) {
	best_match = matches;
	cand = vi;
      }
    };

    for (int pi = 0; pi < pieces.mem.size(); pi += pieces.dag.size()) {
      int*ind = &pieces.mem[pi];
      //TODO: Use hashes to compare instead of full images
      vImage imgs;
      for (int i = 0; i <= train.size(); i++) {
	if (pieces.dag[i].tiny_node[ind[i]].isvec) continue;
	Image_ img = pieces.dag[i].getImg(ind[i]);
	imgs.push_back(img);
	imgs.back().p = point{0,0};
      }
      if (imgs.size() == train.size()+1)
	add(imgs);
    }
    for (auto [x,y] : deduce_op.train_targets) {
      add(vImage(train.size()+1, (k ? y : x)));
    }
  }

  {
    assert(a.size() == train.size()+1);
    assert(b.size() == train.size()+1);

    // TODO: Use correct depths
    vImage imgs;
    for (int i = 0; i <= train.size(); i++)
      imgs.push_back(deduce_op.reconstruct(a[i],b[i]));
    cands.emplace_back(imgs, 2, MAXDEPTH, MAXDEPTH*2);
  }

  /*
  visu.next(to_string(si));
  for (auto [in,out] : train)
    visu.add(in,out);

  visu.next(to_string(si)+"!");
  for (auto [in,out] : deduce_op.train_targets) {
    visu.add(in,out);
    }*/
}
#include "precompiled_stl.hpp"
using namespace std;
#include "utils.hpp"
#include "brute2.hpp"
#include "visu.hpp"

pair<int,bool> TinyHashMap::insert(ull key, int value) {
  if (table.size() <= data.size()*sparse_factor) {
    table.resize(max((int)(table.size() * resize_step), minsz));
    assert((table.size()&(table.size()-1)) == 0);
    mask = table.size()-1;

    fill(table.begin(), table.end(), -1);
    for (int i = 0; i < data.size(); i++) {
      int&head = table[data[i].key&mask];
      data[i].next = head;
      head = i;
    }
  }
  int&head = table[key&mask];
  int previ = head;
  while (1) {
    if (previ == -1) {
      data.push_back({key, value, head});
      head = data.size()-1;
      return {value, true};
    } else if (data[previ].key == key) {
      return {data[previ].value, false};
    }
    previ = data[previ].next;
  }
}




void TinyChildren::add(int fi, int to) {
  assert(fi >= 0);
  if (sz+1 == dense_thres) {
    //Convert to dense
    cap = max(sparse[sz-1].first,fi)+1; //Max fi
    pair<int,int>*old = sparse;
    dense = new int[cap];
    fill_n(dense, cap, None);
    dense[fi] = to;
    for (int i = 0; i < sz; i++) {
      auto [fi, to] = old[i];
      assert(fi >= 0);
      assert(fi < cap);
      dense[fi] = to;
    }
    assert(old);
    delete[]old;
    sz = dense_thres;
  }

  if (sz < dense_thres) {
    //Sparse
    if (sz+1 > cap) {
      cap = max((cap+1)*3/2,4);
      pair<int,int>*old = sparse;
      sparse = new pair<int,int>[cap];
      copy_n(old, sz, sparse);
      if (old) delete[]old;
    }
    {
      int p = sz++;
      while (p && sparse[p-1].first > fi) {
	sparse[p] = sparse[p-1];
	p--;
      }
      sparse[p] = {fi, to};
    }
  } else {
    //Dense
    if (cap <= fi) {
      int oldcap = cap;
      int*old = dense;
      cap = (fi+1)*3/2;
      dense = new int[cap];
      fill_n(dense+oldcap, cap-oldcap, None);
      copy_n(old, oldcap, dense);
      assert(old);
      delete[]old;
    }
    dense[fi] = to;
  }
}
int TinyChildren::get(int fi) {
  assert(fi >= 0);
  if (sz < dense_thres) {
    int low = 0, high = sz-1;
    while (low <= high) {
      int mid = (low+high)>>1;
      int cfi = sparse[mid].first;
      if (cfi == fi) return sparse[mid].second;
      else if (cfi > fi) high = mid-1;
      else low = mid+1;
    }
    return None;
  } else {
    if (fi >= cap) return None;
    return dense[fi];
  }
}
void TinyChildren::legacy(vector<pair<int,int>>&ret) {
  if (sz < dense_thres) {
    //Sparse
    ret.resize(sz);
    for (int i = 0; i < sz; i++)
      ret[i] = sparse[i];
  } else {
    //Dense
    ret.resize(0);
    for (int i = 0; i < cap; i++) {
      if (dense[i] != None)
	ret.emplace_back(i, dense[i]);
    }
  }
}







TinyImage::TinyImage(Image_ img, TinyBank&bank) {
  for (int c : {img.x, img.y, img.w, img.h}) {
    assert(c >= -128 && c < 128);
  }
  x = img.x, y = img.y, w = img.w, h = img.h;

  int freq[10] = {};
  for (char c : img.mask) {
    assert(c >= 0 && c < 10);
    freq[c]++;
  }

  priority_queue<pair<int,int>> pq;
  for (int d = 0; d < 10; d++) {
    if (freq[d]) {
      pq.emplace(-freq[d], -d);
      //cout << d << ": " << freq[d] << endl;
    }
  }
  while (pq.size() < 2) pq.emplace(0,0);

  int nodes = pq.size()-1;
  int pos = nodes-1;
  auto convert = [](int a, int p) {
    if (a <= 0) return -a;
    else {
      assert(9+a-p >= 10 && 9+a-p < 16);
      return 9+a-p;
    }
  };
  while (pq.size() > 1) {
    auto [mfa,a] = pq.top(); pq.pop();
    auto [mfb,b] = pq.top(); pq.pop();
    tree[pos] = convert(a,pos) | convert(b,pos) << 4;
    pq.emplace(mfa+mfb, pos--);
  }
  int code[10] = {}, codelen[10] = {};
  int path[10] = {}, pathlen[10] = {};
  for (int p = 0; p < nodes; p++) {
    for (int k : {0,1}) {
      int child = tree[p]>>k*4&15;
      int newpath = path[p] | k<<pathlen[p];
      if (child < 10) {
	code[child] = newpath;
	codelen[child] = pathlen[p]+1;
      } else {
	child += p-9;
	path[child] = newpath;
	pathlen[child] = pathlen[p]+1;
      }
    }
    //cout << p << ": " << (int)tree[p][0] << ' ' << (int)tree[p][1] << endl;
  }
  /*for (int d = 0; d < 10; d++) {
    cout << d << ": " << bitset<4>(code[d]) << ' ' << codelen[d] << endl;
    }*/

  assert((bank.curi+align-1)/align < 1ll<<32);
  memi = (bank.curi+align-1)/align;
  sz = 0;
  ll memstart = (ll)memi*align;
  bank.alloc();
  for (char c : img.mask) {
    bank.set(memstart+ sz, code[c], codelen[c]);
    sz += codelen[c];
    //for (int i = 0; i < codelen[c]; i++)
    //bank.set(memstart+ sz++, code[c]>>i&1);
  }
  /*for (int it = 0; it < 10; it++)
    cout << (int)img.mask[it] << ' ';
    cout << endl;
    for (int it = 0; it < 20; it++)
    cout << bank.get(memi+it);
    cout << endl;*/
  bank.curi = memstart+sz;
}


Image TinyImage::decompress(TinyBank&bank) {
  Image ret;
  ret.x = x, ret.y = y, ret.w = w, ret.h = h;
  ret.mask.resize(ret.w*ret.h);
  int treep = 0, maski = 0;
  ll memstart = (ll)memi*align;
  for (ll i = memstart; i < memstart+sz; i++) {
    int bit = bank.get(i);
    int child = tree[treep]>>bit*4&15;
    if (child < 10) {
      ret.mask[maski++] = child;
      treep = 0;
    } else
      treep += child-9;
  }
  //cout << maski << ' ' << ret.w*ret.h << endl;
  assert(maski == ret.w*ret.h);
  assert(treep == 0);
  return ret;
}
#include "precompiled_stl.hpp"
using namespace std;
#include "utils.hpp"
#include "read.hpp"
#include "visu.hpp"
#include "core_functions.hpp"
#include "image_functions.hpp"
#include "normalize.hpp"
#include "spec.hpp"
#include "image_functions2.hpp"



int rcDiff(Image_ img) {
  vector<int> row(img.h,1), col(img.w,1);
  for (int i = 0; i < img.h; i++)
    for (int j = 0; j < img.w; j++)
      if (img(i,j) == 0) row[i] = col[j] = 0;
  int ans = 0;
  for (int i = 0; i < img.h; i++)
    ans += row[i];
  for (int j = 0; j < img.w; j++)
    ans -= col[j];
  return ans;
}


Simplifier normalizeOrient() {
  Simplifier sim;
  sim.in = [](Image_ in) {
    return rcDiff(in) >= 0 ? in : rigid(in,6);
  };
  sim.out = [](Image_ in, Image_ out) {
    return rcDiff(in) >= 0 ? out : rigid(out,6);
  };
  sim.rec = [](Image_ in, Image_ out) {
    return rcDiff(in) >= 0 ? out : rigid(out,6);
  };
  return sim;
}


#define VECIFY(func)				\
  vImage func(vImage_ vimg) {			\
    vImage ret(vimg.size());			\
    for (int i = 0; i < vimg.size(); i++) 	\
      ret[i] = func(vimg[i]);			\
    return ret;					\
  }
VECIFY(hull);

#define FOREACH(content)			\
  vImage ret(vi.size());			\
  for (int i = 0; i < vi.size(); i++)		\
    ret[i] = content;				\
  return ret;

vImage count(vImage_ vi, int a, int b) {
  FOREACH(count(vi[i],a,b));
}





Image solveEval(Image in, vector<pair<Image,Image>> train, int taski) {
  //Skipped  23,40,53,58,90
  //Error in train  7, 27, 44/45, 77
  auto sim = normalizeCols(train);
  auto in0 = in;

  if (taski == 0) {
    vImage out = gravity(in, 0);
    return embed(composeGrowing(out),in);

  } else if (taski == 1) {
    in = sim.in(in);

    vImage inside = insideMarked(in);
    Image out = pickMax(inside,0);
    return sim.rec(in0,out);

  } else if (taski == 2) {
    in = eraseCol(in, 2);
    return greedyFillBlack(in);

  } else if (taski == 3) {
    Image a = eraseCol(cutIndex(in,0), 2);
    Image b = cutIndex(in,1);
    Image c = cutIndex(in,2);
    Image out = broadcast(b,c);
    out = replaceTemplate(out,hull(a),a);
    return compose(c, out);

  } else if (taski == 4) {
    Image part = rigid(cutPickMax(in, 4), 4);
    return replaceTemplate(in, invert(part), part);

  } else if (taski == 5) {
    in = sim.in(in);

    Image dot = compress(filterCol(in, 1));
    Image out = repeat(dot, in, 1);
    return sim.rec(in0, compose(out, filterCol(in, 2)));

  } else if (taski == 6) {
    Simplifier orient = normalizeOrient();
    in = orient.in(in);
    Image base = colShape(interior(filterCol(in, 3)),1);
    base = smear(base, 5);
    Image cross = compose(filterCol(in,2), base, 2);
    cross = makeBorder(cross);
    Image out = compose(compose(compose(base, in), cross), filterCol(in,3));
    return orient.rec(in0, out);

  } else if (taski == 7) {
    Image out = repeat(in, core::full({in.w*3,in.h*3}));
    Image green = colShape(out, 3);
    Image blue = colShape(smear(out, 4), 1);
    Image a = Move(green, Pos(1,1));
    Image b = Move(green, Pos(-1,-1));
    return compose(blue, compose(out, compose(a, b), 3));

  } else if (taski == 8) {
    in = sim.in(in);
    Image squares = interior(filterCol(in,1));
    Image light = composeGrowing(pickNotMaxes(cut(in), 11));
    Image border = makeBorder(filterCol(in,1));
    Image out = composeGrowing({in, colShape(border,2), colShape(light,5), colShape(squares,4)});
    return sim.rec(in0,out);

  } else if (taski == 9) {
    Image out = in;
    for (Image_ img : cut(in)) {
      Image dot = embed(img, out);
      out = compose(smear(dot, img.x < in.w-img.x ? 1 : 0), out);
      out = compose(smear(dot, img.y < in.h-img.y ? 3 : 2), out);
    }
    return out;

  } else if (taski == 10) {
    Image sz = train[0].second;//core::full(in.w*5,in.h);
    return mirror(in, sz);

  } else if (taski == 11) {
    return spreadCols(in,1);

  } else if (taski == 12) {
    in = sim.in(in);

    Image swapcol = compose(colShape(filterCol(in,1),3),
			    colShape(filterCol(in,3),1));

    Image mirror = align(rigid(in, 8), in);
    Image out = compose(swapcol, mirror, 3);
    return sim.rec(in0, out);

  } else if (taski == 13) {
    return greedyFillBlack(in);

  } else if (taski == 14) {
    Image out = badImg;
    for (Image img : splitCols(in)) {
      img = compress(img);
      Image add = border(count(img,5,0));
      out = compose(out, align(add, out, 2, 2));
    }
    return out;

  } else if (taski == 15) {
    in = eraseCol(in, 8);
    return compress3(compress2(in));

  } else if (taski == 16) {
    Image out = in;
    Image pat = cutIndex(train[0].second,0);
    pat = spreadCols(eraseCol(pat,4));
    for (Image_ img : cut(in)) {
      Image base = broadcast(pat,img);
      base = compose(base, colShape(interior2(interior2(img)), 4));
      out = compose(out, base);
    }
    return out;

  } else if (taski == 17) {
    in = sim.in(in);
    point ins = compress(interior(filterCol(in, 2))).sz;
    int borders = ins.x == ins.y ? ins.x : 2;
    for (int i = 0; i < borders; i++)
      in = compose(in,makeBorder(in, 1));
    return sim.rec(in0, in);

  } else if (taski == 18) {
    return compress(compose(rigid(in,2),in,4));

  } else if (taski == 19 || taski == 20) {
    in = sim.in(in);
    Image base = sim.out(train[0].first, train[0].second);
    Image out = Move(base, outerProductIS(Move(compress(in), Pos(-1,-1)),Square(4)));
    out = extend2(out, base);
    return sim.rec(in0,out);

  } else if (taski == 21) {
    Image parts = smear(connect(in, 0), 3);
    Image bottom = smear(embed(pickMax(splitRows(in),6),in), 3);
    parts = compose(parts,bottom,2);
    return compose(in, compose(bottom, parts));

  } else if (taski == 22) {
    Image marked = compose(in, hull(compress(filterCol(in,4))), 2);
    Image need = eraseCol(marked, 4);
    return replaceTemplate(in, need, marked);

  } else if (taski == 23) { //Wrong
    in = eraseCol(in, 5);
    Image ret = invert(hull(in));
    vImage s = splitAll(in);
    for (Image_ p : s) {
      for (Image_ q : s) {
	if (p.sz == q.sz && !core::count(p) && core::isRectangle(q)) {
	  ret = compose(ret, align(q,p,2,2));
	}
      }
      if (p.sz != in.sz && core::count(interior(p)))
	ret = compose(ret, p);
    }
    return ret;

  } else if (taski == 24) {
    Image a = myStack(in,rigid(in,1),0);
    Image b = myStack(rigid(in,2),rigid(in,3),0);
    return myStack(a,b,1);

  } else if (taski == 25) {
    in = sim.in(in);
    in = eraseCol(in,1);
    vImage cols = splitCols(in);
    vImage lens = count(cols, 2,1);
    Image out = myStack(lens, 1);
    return sim.rec(in0,out);

  } else if (taski == 26) {
    in = sim.in(in);
    Image out = eraseCol(eraseCol(in,1),2);
    out = connect(out,2);
    out = compose(out,in);
    return sim.rec(in0, out);

  } else if (taski == 27) {
    in = sim.in(in);
    Image out = compose(in, filterCol(rigid(in,4),1));
    return sim.rec(in0, out);

  } else if (taski == 28) {
    return compress3(in);

  } else if (taski == 29) {
    in = sim.in(in);

    int blue = core::majorityCol(in);
    int red = 3-blue;
    Image out = eraseCol(in,blue);
    vImage to = cut(filterCol(in,3));
    for (Image img : to) {
      img = compress(img);
      out = replaceTemplate(out,colShape(img,red),img,2);
    }
    out = compose(out,filterCol(in,blue));
    return sim.rec(in0,out);

  } else if (taski == 30) {
    return sim.rec(in0,Col(1));

  } else if (taski == 31) {
    in = sim.in(in);
    in = filterCol(in,2);
    Image out = compose(colShape(smear(in,2),1), in);
    return sim.rec(in0,out);

  } else if (taski == 32) {
    Image gray = cutPickMax(in,0);
    Image cols = eraseCol(gray,5);

    Image shapes = compress(compose(in, gray, 4));

    cols = spreadCols(broadcast(cols,shapes));
    return embed(compose(replaceCols(shapes,cols), gray), in);

  } else if (taski == 33) {
    return compose(border(colShape(compose(smear(in,4), smear(in,5), 2), 8)), in);

  } else if (taski == 34) {
    Image blue = filterCol(in,8);
    Image squares = Fill(blue);

    Image dots = eraseCol(in,8);
    Image out = blue;
    for (Image_ img : splitCols(dots)) {
      Image part = compress(compose(hull(compress(img)), squares, 2));
      part = makeBorder2(part, 1);
      out = compose(part, out);
    }
    return out;

  } else if (taski == 35) {
    return compose(colShape(hull(in),5),Move(in,Pos(-1,0)),3);

  } else if (taski == 36) {
    in = eraseCol(in,3);
    in = compose(in,rigid(in,2));
    in = compose(in,rigid(in,4));
    return in;

  } else if (taski == 37) {
    in = eraseCol(in,3);

    Image out = compose(in,rigid(in,2));
    out = compose(out,rigid(out,4));
    return compress(compose(out,invert(in),2));

  } else if (taski == 38) {
    return outerProductIS(in,count(in,1,0));

  } else if (taski == 39) {
    Image out = in;
    for (Image_ img : cut(colShape(in,1))) {
      Image part = compose(in, makeBorder2(img,0), 2);
      part = eraseCol(part, 5);
      out = compose(broadcast(majCol(part), img), out);
    }
    return out;

  } else if (taski == 40) { //Wrong

    return in;

  } else if (taski == 41) {
    return outerProductSI(hull(in),in);

  } else if (taski == 42) {
    Image marked = compress(filterCol(in,8));
    Image out = replaceTemplate(in, colShape(marked,1), marked);
    Image marks = filterCol(out,8);
    Image orange = compose(colShape(connect(marks,2),7), in, 2);
    return compose(out, compose(orange, marks));

  } else if (taski == 43) {
    in = sim.in(in);
    Image bg = compress(filterCol(in,1));
    Image out = bg;
    for (Image_ img : cut(in)) {
      Image need = makeBorder2(colShape(invert(img),1),0);
      out = replaceTemplate(out, need, makeBorder2(img,0));
    }
    out = compose(out,bg);
    return sim.rec(in0, out);

  } else if (taski == 44 || taski == 45) {
    Image shape = compress(filterCol(in,8));
    Image room = invert(Square(3));
    shape = embed(align(shape,room,1,3), room);
    return outerProductSI(count(filterCol(in,4),2,1),shape);

  } else if (taski == 46) {
    Image squares = colShape(repeat(hull(cutPickMax(in,0)), in, 1),2);
    return compose(squares,in,4);

  } else if (taski == 47 || taski == 48) {
    Image a = compress(filterCol(in,6));
    Image b = toOrigin(compress(filterCol(in,5)));
    return colShape(invert(compose(a,b)),4);

  } else if (taski == 49) {
    vImage cols = splitCols(in);
    vImage lens = count(cols, 2,1);
    Image out = myStack(lens,1);
    return rigid(out,2);

  } else if (taski == 50) {
    Image shapes = stackLine(cut(filterCol(in,6)));
    Image cols = stackLine(cut(eraseCol(in,6)));
    return compose(broadcast(cols, shapes), shapes, 2);

  } else if (taski == 51) {
    Image out = replaceTemplate(in, invert(Square(3)), Square(3));
    out = replaceTemplate(out, invert(Square(2)), Square(2));
    return out;

  } else if (taski == 52) {
    Image green = filterCol(in,3);
    vImage hulls = hull( cut(green) );
    Image filled = embed(compose(hulls,0),in);
    Image specials = compose(filterCol(in,2), filled, 2);
    return compose(in, makeBorder(specials));

  } else if (taski == 53) { //Wrong
    Image center = interior(in);
    Image base = train[1].second;
    base = compose(base, interior(base));
    base = embed(align(base,center),in);
    return greedyFillBlack(base);

  } else if (taski == 54) {
    Image out = toOrigin(cutPickMax(in,0));
    for (Image_ pa : cut(cutPickMaxes(in,1))) {
      Image a = half(pa,0);
      Image b = half(pa,1);
      out = swapTemplate(out,a,b);
    }
    return out;

  } else if (taski == 55 || taski == 56) {
    Image a = half(in, 2);
    Image b = toOrigin(half(in, 3));
    Image or_ = compose(a,b);
    Image and_= compose(a,b,2);
    return colShape(compose(or_,and_,4),6);

  } else if (taski == 57) {
    Image out = toOrigin(cutPickMaxes(in,0));
    for (Image_ img : pickNotMaxes(cut(in),0)) {
      out = compose(out,align(img,out));
    }
    return out;

  } else if (taski == 58) { //Wrong


  } else if (taski == 59) {
    return compose(smear(eraseCol(in,5),0),in,2);

  } else if (taski == 60) {
    in = eraseCol(in,6);
    in = compose(in,rigid(in,6));
    in = greedyFillBlack(in);
    return in;

  } else if (taski == 61) {
    in = sim.in(in);
    Image a = filterCol(in,2);
    Image b = filterCol(in,1);

    Image out = Square(3);
    a = compress(wrap(count(interior(a),0,1),out));
    b = compress(wrap(count(interior(b),0,1),out));
    out = embed(toOrigin(myStack(a, b, 1)), out);
    return sim.rec(in0,out);

  } else if (taski == 62) {
    Image out = in;
    int col = 1;
    for (Image_ img : pickNotMaxes(splitColumns(in),0)) {
      out = compose(colShape(hull(img), col++), out);
    }
    return out;

  } else if (taski == 63) {
    Image b = half(compress(in), 3);
    return compose(in, colShape(b,2));

  } else if (taski == 64) {
    in = sim.in(in);

    int flip = 0;
    if (core::count(half(in,2)) < core::count(half(in,3))) flip = 1;
    if (flip) in = rigid(in,5);

    Image both = compose(half(in,2),rigid(in,5),2);
    Image out = smear(filterCol(both,2),2);
    out = compose(colShape(out,1), in);

    if (flip) out = rigid(out,5);
    return sim.rec(in0, out);

  } else if (taski == 65) {
    Image out = in;
    for (Image img : cut(in)) {
      Image piece = img;
      img = eraseCol(img, 4);
      piece = makeBorder2(piece, count(img,0,0));
      out = compose(out, piece);
    }
    return out;

  } else if (taski == 66) {
    Image marked = compress(cutPickMax(in, filterCol(in,5), 1));
    Image out = replaceTemplate(in, marked, Col(0));
    return compose(marked, out);

  } else if (taski == 67) {
    Image marked = compose(pickNotMaxes(splitAll(invert(in)),1),0);
    return compose(colShape(marked,8),in);

  } else if (taski == 68 || taski == 69) {
    Image center = interior2(in);
    return compose(spreadCols(in), smear(center,14), 2);

  } else if (taski == 70) {
    vImage outs = cut(in);
    Image out = stackLine(outs);
    return out;

  } else if (taski == 71) {
    Image to = colShape(Square(4),4);
    for (int c = 1; c < 10; c++) {
      in = replaceTemplate(in,colShape(to,c),to, 1);
    }
    return in;

  } else if (taski == 72) {
    Image a = compress(in);
    int other_col = 6+3-core::majorityCol(a);
    Image b = toOrigin(colShape(rigid(a,6),other_col));
    Image c = myStack(a,b,2);
    c = myStack(myStack(c,c,2),c,2);

    a = colShape(a,1);
    c = compose(c,a);
    c = compose(c,align(rigid(c,4),c));
    c = compose(c,align(rigid(c,5),c));
    return embed(compose(c,in),in);

  } else if (taski == 73) {
    in = rigid(in,2);
    return mirror(in,outerProductIS(in,Square(2)));

  } else if (taski == 74) {
    in = sim.in(in);
    Image shapes = repeat(pickMax(cut(in),0),in,1);
    Image grid = filterCol(in,1);
    Image cols = spreadCols(eraseCol(eraseCol(in,1),2));
    Image out = compose(compose(cols, shapes, 2), grid);
    return sim.rec(in0,out);

  } else if (taski == 75) {
    Image out = in;
    for (Image_ img : cut(in)) {
      int ccs = core::countComponents(invert(img));
      int cols[] = {0,1,2,3,7};
      out = compose(out, colShape(img,cols[ccs]));
    }
    return out;

  } else if (taski == 76) {
    Image inside = cutPickMax(in,filterCol(in,5),1);
    Image out = in;
    for (Image img : splitCols(inside)) {
      img = compress(img);
      Image green = colShape(img,3);
      out = replaceTemplate(out,green,img,2, 1);
    }
    return out;

  } else if (taski == 77) {
    Image a = compress(in);
    for (int i = 0; i < a.h; i++) {
      for (int j = 0; j < a.w; j++) {
	int d = min({i,j,a.h-1-i,a.w-1-j});
	a(i,j) = d%2 == 0 ? 5 : d%4 == 1 ? 2 : 0;
      }
    }
    return embed(a,in);

  } else if (taski == 78) {
    int W = cutIndex(in,0).w;

    Image out = in;
    for (Image_ img : cut(in)) {
      map<Image,int> cnt;
      for (int d = 0; d < 4; d++) {
	int nx = img.x+(W+1)*((d==0)-(d==1));
	int ny = img.y+(W+1)*((d==2)-(d==3));

	if (nx >= 0 && ny >= 0 && nx+4 <= in.w && ny+4 <= in.h) {
	  Image r = core::subImage(in,{nx,ny},{W,W});
	  if (core::count(r) > 0) {
	    int rs[] = {4,4,5,5};
	    Image here = rigid(r,rs[d]);
	    here.p = img.p;
	    ++cnt[here];
	  }
	}
      }
      for (auto [here,c] : cnt) {
	if (c == 2)
	  out = compose(out, here);
      }
    }
    return out;

  } else if (taski == 79) {
    Image out = in;
    for (Image_ img : cut(in)) {
      out = compose(out, repeat(compress(img),img));
    }
    return out;

  } else if (taski == 80) {
    Image pat = compress(compose(pickNotMaxes(splitAll(in),0),0));
    return extend2(Move(pat,Pos(-1,0)),in);

  } else if (taski == 81) {
    Image dots = cutPickMaxes(in,1);
    Image out = in;
    for (Image_ dot : cut(dots)) {
      for (Image_ img : cut(in)) {
	if (img.w > 1) {
	  out = compose(out, align(img, dot), 3);
	}
      }
    }
    return out;

  } else if (taski == 82) {
    return outerProductSI(in,invert(in));

  } else if (taski == 83) {
    Image out = hull0(in);
    for (Image_ img : splitCols(in)) {
      Image top = cutPickMax(img,5);
      Image side = cutPickMax(img,6);
      Image add;
      if (top.p == side.p)
	add = smear(embed(top,in),2);
      else {
	add = compose(smear(embed(top,in),2),
		      smear(embed(side,in),4));
	add = compose(add, hull(compress(img)),2);
      }
      out = compose(add, out);
    }
    return out;

  } else if (taski == 84) {
    Image out = in;
    for (Image_ img : cut(in)) {
      out = compose(out, colShape(majCol(eraseCol(img,1)), interior2(img)));
    }
    return out;

  } else if (taski == 85) {
    Image cols = rigid(cutPickMax(in,1), 3);
    Image shape = compress(filterCol(in,8));
    return compose(in,compose(broadcast(cols,shape), shape, 2));

  } else if (taski == 86) {
    Image out = hull0(in);
    for (Image_ img : cut(in)) {
      Image yellow = filterCol(img,4);
      Image line = cutPickMax(yellow,invert(yellow),0);
      vImage splitted = cut(compose(img,invert(embed(line,img)),2));
      Image c = compose(pickUnique(splitted,0), line);
      out = compose(out, line);
      out = compose(out,c);
      out = compose(out,mirror2(c,line));
    }
    return out;

  } else if (taski == 87) {
    return outerProductSI(filterCol(in,5),in);

  } else if (taski == 88) {
    Image from = Square(2);
    in = replaceTemplate(in,from,invert(from),1);
    return greedyFillBlack(in);

  } else if (taski == 89) {
    Image black = badImg;
    Image out = badImg;
    for (Image_ img : pickMaxes(splitAll(in),0)) {
      Image plus = smear(embed(interior2(img),in),6);
      Image both = compose(plus,out,2);
      black = compose(black,both);
      out = compose(out,plus);
    }
    return compose(compose(in,out),black,4);

  } else if (taski == 90) { //Wrong

  } else if (taski == 91 || taski == 92) {
    Image shape = compress(filterCol(in,1));
    return outerProductIS(compress2(eraseCol(in,1)),shape);

  } else if (taski == 93 || taski == 94) {
    int col = core::majorityCol(in);
    if (col == 1) return train[0].second;
    else if (col == 2) return train[1].second;
    else if (col == 3) return train[2].second;
    else return badImg;

  } else if (taski == 95) {
    Image cols = eraseCol(in,5);
    Image gray = filterCol(in,5);
    cols = compose(cols,rigid(cols,2));
    cols = spreadCols(cols);
    return compose(colShape(cols, Fill(in)),gray);

  } else if (taski == 96) {
    Image side = cutPickMax(in,0);
    Image out = in;
    Image red = colShape(Square(5),2);
    for (Image_ img : cut(side)) {
      Image a = colShape(majCol(img),red);
      Image b = compose(red,align(colShape(img,1),red,2,2));
      out = swapTemplate(out,a,b,1);
    }
    return out;

  } else if (taski == 97) {
    Image a = half(in,2);
    Image b = toOrigin(half(in,3));
    return colShape(compose(a,b),4);

  } else if (taski == 98) {
    Image out = hull0(in);
    for (Image img : splitCols(in)) {
      img = compress(img);
      int col = core::majorityCol(img);
      if (col == 8)
	img.x = 1;
      else if (col == 2)
	img.x = 2;
      else if (col == 4)
	img.x = 3;
      else if (col == 3)
	img.x = 4;
      out = compose(out,img);
    }
    return out;

  } else if (taski == 99) {
    in = sim.in(in);
    Image out = filterCol(in,2);
    for (Image_ img : insideMarked(in)) {
      Image a = compose(rigid(img,4),img);
      Image b = compose(rigid(img,5),img);
      out = compose(out, img.w > img.h ? a : b);
    }
    return sim.rec(in0,out);
  }

  return in;
}


void evalEvals(int print = 1) {
  vector<Sample> sample = readAll("evaluation", -1);
  int samples = sample.size();
  sample = vector<Sample>(sample.begin()+samples-100,sample.end());

  Visu visu;

  int corrects = 0;
  for (int si = 0; si < sample.size(); si++) {
    Sample&s = sample[si];
    visu.next(s.id);
    int ok = 1;
    for (auto [in,out] : s.train) {
      Image pred = toOrigin(solveEval(in, s.train, si));
      ok &= (pred == out);
      auto sim = normalizeCols(s.train);
      auto in0 = in;
      tie(in,out) = sim(in,out);
      visu.add(in, out);
      visu.add(in, sim(in0,pred).second);
    }
    corrects += ok;
    if (print)
      cout << "Task " << si << ": " << (ok ? "OK" : "Failed") << endl;
  }

  if (print) {
    cout << corrects << " / " << sample.size() << endl;
    exit(0);
  }
  if (!print)
    assert(corrects == 95);
}
#include "precompiled_stl.hpp"
using namespace std;
#include "utils.hpp"
#include "core_functions.hpp"
#include "image_functions.hpp"
#include "spec.hpp"
#include "read.hpp"
#include "normalize.hpp"


vImage splitAll(Image_ img) {
  vector<Image> ret;
  Image done = core::empty(img.p,img.sz);
  for (int i = 0; i < img.h; i++) {
    for (int j = 0; j < img.w; j++) {
      if (!done(i,j)) {
	Image toadd = core::empty(img.p,img.sz);
	function<void(int,int,int)> dfs = [&](int r, int c, int col) {
	  if (r < 0 || r >= img.h || c < 0 || c >= img.w || img(r,c) != col || done(r,c)) return;
	  toadd(r,c) = img(r,c)+1;
	  done(r,c) = 1;
	  for (int d = 0; d < 4; d++)
	    dfs(r+(d==0)-(d==1),c+(d==2)-(d==3),col);
	};
	dfs(i,j,img(i,j));
	toadd = compress(toadd);
	for (int i = 0; i < toadd.h; i++) {
	  for (int j = 0; j < toadd.w; j++) {
	    toadd(i,j) = max(0, toadd(i,j)-1);
	  }
	}
	//Image i = interior(toadd);
	//if (!core::count(compose(img,i,2)))
	if (core::count(toadd))
	  ret.push_back(toadd);
      }
    }
  }
  return ret;
}


Image eraseCol(Image img, int col) {
  for (int i = 0; i < img.h; i++)
    for (int j = 0; j < img.w; j++)
      if (img(i,j) == col) img(i,j) = 0;
  return img;
}


// Looks for 4 corners
vImage insideMarked(Image_ in) {
  vector<Image> ret;
  for (int i = 0; i+1 < in.h; i++) {
    for (int j = 0; j+1 < in.w; j++) {
      for (int h = 1; i+h+1 < in.h; h++) {
	for (int w = 1; j+w+1 < in.w; w++) {
	  char col = in(i,j);
	  if (!col) continue;
	  int ok = 1;
	  for (int k = 0; k < 4; k++) {
	    int x = j+k%2*w, y = i+k/2*h;
	    for (int d = 0; d < 4; d++) {
	      if ((d != 3-k) == (in(y+d/2,x+d%2) != col)) {
		ok = 0;
		goto fail;
	      }
	    }
	  }
	fail:
	  if (ok) {
	    Image inside = invert(core::full(point{j+1,i+1}, point{w,h}));
	    ret.push_back(compose(inside,in,3));
	  }
	}
      }
    }
  }
  return ret;
}

Image makeBorder(Image_ img, int bcol = 1) {
  Image ret = hull0(img);
  for (int i = 0; i < ret.h; i++) {
    for (int j = 0; j < ret.w; j++) {
      if (img(i,j) == 0) {
	int ok = 0;
	for (int ni : {i-1,i,i+1}) {
	  for (int nj : {j-1,j,j+1}) {
	    if (img.safe(ni,nj)) {
	      ok = 1;
	      break;
	    }
	  }
	}
	if (ok) {
	  ret(i,j) = bcol;
	}
      }
    }
  }
  return ret;
}


Image makeBorder2(Image_ img, int usemaj = 1) {
  int bcol = 1;
  if (usemaj) bcol = core::majorityCol(img);

  point rsz = img.sz+point{2,2};
  if (max(rsz.x,rsz.y) > MAXSIDE || rsz.x*rsz.y > MAXAREA) return badImg;
  Image ret = core::full(img.p-point{1,1}, rsz, bcol);
  for (int i = 0; i < img.h; i++)
    for (int j = 0; j < img.w; j++)
      ret(i+1,j+1) = img(i,j);
  return ret;
}

Image makeBorder2(Image_ img, Image_ bord) {
  int bcol = core::majorityCol(bord);
  point rsz = img.sz+bord.sz+bord.sz;
  if (max(rsz.x,rsz.y) > MAXSIDE || rsz.x*rsz.y > MAXAREA) return badImg;
  Image ret = core::full(img.p-bord.sz, rsz, bcol);
  for (int i = 0; i < img.h; i++)
    for (int j = 0; j < img.w; j++)
      ret(i+bord.h,j+bord.w) = img(i,j);
  return ret;
}


//Delete black rows / cols
Image compress2(Image_ img) {
  vector<int> row(img.h), col(img.w);
  for (int i = 0; i < img.h; i++)
    for (int j = 0; j < img.w; j++)
      if (img(i,j)) row[i] = col[j] = 1;
  vector<int> rows, cols;
  for (int i = 0; i < img.h; i++) if (row[i]) rows.push_back(i);
  for (int j = 0; j < img.w; j++) if (col[j]) cols.push_back(j);
  Image ret = core::empty(point{(int)cols.size(), (int)rows.size()});
  for (int i = 0; i < ret.h; i++)
    for (int j = 0; j < ret.w; j++)
      ret(i,j) = img(rows[i],cols[j]);
  return ret;
}


//Group single color rectangles
Image compress3(Image_ img) {
  if (img.w*img.h <= 0) return badImg;
  vector<int> row(img.h), col(img.w);
  row[0] = col[0] = 1;
  for (int i = 0; i < img.h; i++) {
    for (int j = 0; j < img.w; j++) {
      if (i && img(i,j) != img(i-1,j)) row[i] = 1;
      if (j && img(i,j) != img(i,j-1)) col[j] = 1;
    }
  }
  vector<int> rows, cols;
  for (int i = 0; i < img.h; i++) if (row[i]) rows.push_back(i);
  for (int j = 0; j < img.w; j++) if (col[j]) cols.push_back(j);
  Image ret = core::empty(point{(int)cols.size(), (int)rows.size()});
  for (int i = 0; i < ret.h; i++)
    for (int j = 0; j < ret.w; j++)
      ret(i,j) = img(rows[i],cols[j]);
  return ret;
}


//TODO: return badImg if fail

Image greedyFill(Image& ret, vector<pair<int,vector<int>>>&piece, Spec&done, int bw, int bh, int&donew) {
  sort(piece.rbegin(), piece.rend());

  const int dw = ret.w-bw+1, dh = ret.h-bh+1;
  if (dw < 1 || dh < 1) return badImg;

  vector<int> dones(dw*dh, -1);
  priority_queue<tuple<int,int,int>> pq;
  auto recalc = [&](int i, int j) {
    int cnt = 0;
    for (int y = 0; y < bh; y++)
      for (int x = 0; x < bw; x++)
	cnt += done(i+y,j+x);
    if (cnt != dones[i*dw+j]) {
      dones[i*dw+j] = cnt;
      pq.emplace(cnt,j,i);
    }
  };
  for (int i = 0; i+bh <= ret.h; i++)
    for (int j = 0; j+bw <= ret.w; j++)
      recalc(i,j);

  while (pq.size()) {
    auto [ds,j,i] = pq.top();
    pq.pop();
    if (ds != dones[i*dw+j]) continue;
    int found = 0;
    for (auto [cnt,mask] : piece) {
      int ok = 1;
      for (int y = 0; y < bh; y++)
	for (int x = 0; x < bw; x++)
	  if (done(i+y,j+x) && ret(i+y,j+x) != mask[y*bw+x])
	    ok = 0;
      if (ok) {
	for (int y = 0; y < bh; y++) {
	  for (int x = 0; x < bw; x++) {
	    if (!done(i+y,j+x)) {
	      done(i+y,j+x) = donew;
	      if (donew > 1) donew--;
	      ret(i+y,j+x) = mask[y*bw+x];
	    }
	  }
	}
	for (int y = max(i-bh+1,0); y < min(i+bh, dh); y++)
	  for (int x = max(j-bw+1,0); x < min(j+bw, dw); x++)
	    recalc(y,x);
	found = 1;
	break;
      }
    }
    if (!found)
      return badImg;//ret;
  }
  return ret;
}




Image greedyFillBlack(Image_ img, int N = 3) {
  Image ret = core::empty(img.p, img.sz);
  Spec done;
  done.sz = img.sz;
  done.mask.assign(done.w*done.h,0);

  int donew = 1e6;
  for (int i = 0; i < ret.h; i++) {
    for (int j = 0; j < ret.w; j++) {
      if (img(i,j)) {
	ret(i,j) = img(i,j);
	done(i,j) = donew;
      }
    }
  }

  map<vector<int>,int> piece_cnt;
  vector<int> mask;
  const int bw = N, bh = N;
  for (int r = 0; r < 8; r++) {
    Image rot = rigid(img,r);
    for (int i = 0; i+bh <= rot.h; i++) {
      for (int j = 0; j+bw <= rot.w; j++) {
	mask.reserve(bw*bh);
	mask.resize(0);
	int ok = 1;
	for (int y = 0; y < bh; y++)
	  for (int x = 0; x < bw; x++) {
	    char c = rot(i+y,j+x);
	    mask.push_back(c);
	    if (!c) ok = 0;
	  }
	if (ok)
	  ++piece_cnt[mask];
      }
    }
  }
  vector<pair<int,vector<int>>> piece;
  for (auto&[p,c] : piece_cnt)
    piece.emplace_back(c,p);

  return greedyFill(ret, piece, done, bw, bh, donew);
}

Image greedyFillBlack2(Image_ img, int N = 3) {
  Image filled = greedyFillBlack(img, N);
  return compose(filled, img, 4);
}

Image extend2(Image_ img, Image_ room) {
  Image ret = core::empty(room.p, room.sz);
  Spec done;
  done.sz = room.sz;
  done.mask.assign(done.w*done.h,0);

  point d = room.p-img.p;
  int donew = 1e6;
  for (int i = 0; i < ret.h; i++) {
    for (int j = 0; j < ret.w; j++) {
      int x = j+d.x, y = i+d.y;
      if (x >= 0 && y >= 0 && x < img.w && y < img.h) {
	ret(i,j) = img(y,x);
	done(i,j) = donew;
      }
    }
  }

  map<vector<int>,int> piece_cnt;
  vector<int> mask;
  const int bw = 3, bh = 3;
  for (int r = 0; r < 8; r++) {
    Image rot = rigid(img,r);
    for (int i = 0; i+bh <= rot.h; i++) {
      for (int j = 0; j+bw <= rot.w; j++) {
	mask.reserve(bw*bh);
	mask.resize(0);
	for (int y = 0; y < bh; y++)
	  for (int x = 0; x < bw; x++)
	    mask.push_back(rot(i+y,j+x));
	++piece_cnt[mask];
      }
    }
  }
  vector<pair<int,vector<int>>> piece;
  for (auto&[p,c] : piece_cnt)
    piece.emplace_back(c,p);

  return greedyFill(ret, piece, done, bw, bh, donew);
}



Image connect(Image_ img, int id) {
  assert(id >= 0 && id < 3);
  Image ret = core::empty(img.p, img.sz);

  //Horizontal
  if (id == 0 || id == 2) {
    for (int i = 0; i < img.h; i++) {
      int last = -1, lastc = -1;
      for (int j = 0; j < img.w; j++) {
	if (img(i,j)) {
	  if (img(i,j) == lastc) {
	    for (int k = last+1; k < j; k++)
	      ret(i,k) = lastc;
	  }
	  lastc = img(i,j);
	  last = j;
	}
      }
    }
  }

  //Vertical
  if (id == 1 || id == 2) {
    for (int j = 0; j < img.w; j++) {
      int last = -1, lastc = -1;
      for (int i = 0; i < img.h; i++) {
	if (img(i,j)) {
	  if (img(i,j) == lastc) {
	    for (int k = last+1; k < i; k++)
	      ret(k,j) = lastc;
	  }
	  lastc = img(i,j);
	  last = i;
	}
      }
    }
  }

  return ret;
}

Image replaceTemplate(Image_ in, Image_ need_, Image_ marked_, int overlapping = 0, int rigids = 0) {
  if (marked_.sz != need_.sz) return badImg;
  if (need_.w*need_.h <= 0) return in;

  const int rots = rigids ? 8 : 1;
  vector<Image> needr(rots), markedr(rots);
  for (int r = 0; r < rots; r++) {
    needr[r] = rigid(need_,r);
    markedr[r] = rigid(marked_,r);
  }

  Image ret = in;
  for (int r = 0; r < rots; r++) {
    Image_ need = needr[r];
    Image_ marked = markedr[r];

    for (int i = 0; i+need.h <= ret.h; i++) {
      for (int j = 0; j+need.w <= ret.w; j++) {
	int ok = 1;
	for (int y = 0; y < need.h; y++)
	  for (int x = 0; x < need.w; x++)
	    if ((overlapping ? in : ret)(i+y,j+x) != need(y,x)) ok = 0;

	if (overlapping == 2) {
	  for (int y = -1; y <= need.h; y++) {
	    for (int x = -1; x <= need.w; x++) {
	      if (x >= 0 && y >= 0 && x < need.w && y < need.h) continue;

	      char nn = need(clamp(y,0,need.h-1),
			     clamp(x,0,need.w-1));
	      if (nn && nn == in.safe(i+y,j+x)) ok = 0;
	    }
	  }
	}

	if (ok) {
	  for (int y = 0; y < need.h; y++)
	    for (int x = 0; x < need.w; x++)
	      ret(i+y,j+x) = marked(y,x);
	}
      }
    }
  }
  return ret;
}



Image swapTemplate(Image_ in, Image_ a, Image_ b, int rigids = 0) {
  if (a.sz != b.sz) return badImg;
  if (a.w*a.h <= 0) return in;

  const int rots = rigids ? 8 : 1;
  vector<Image> ar(rots), br(rots);
  for (int r = 0; r < rots; r++) {
    ar[r] = rigid(a,r);
    br[r] = rigid(b,r);
  }
  Image done = hull0(in), ret = in;
  for (int k : {0,1}) {
    for (int r = 0; r < rots; r++) {
      Image_ need = k ? ar[r] : br[r];
      Image_ to   = k ? br[r] : ar[r];

      for (int i = 0; i+need.h <= ret.h; i++) {
	for (int j = 0; j+need.w <= ret.w; j++) {

	  int ok = 1;
	  for (int y = 0; y < need.h; y++)
	    for (int x = 0; x < need.w; x++)
	      if (done(i+y,j+x) || ret(i+y,j+x) != need(y,x)) ok = 0;
	  if (ok) {
	    for (int y = 0; y < need.h; y++) {
	      for (int x = 0; x < need.w; x++) {
		ret(i+y,j+x) = to(y,x);
		done(i+y,j+x) = 1;
	      }
	    }
	  }
	}

      }
    }
  }
  return ret;
}


Image spreadCols(Image img, int skipmaj = 0) {
  int skipcol = -1;
  if (skipmaj)
    skipcol = core::majorityCol(img);

  Image done = hull0(img);
  queue<tuple<int,int,int>> q;
  for (int i = 0; i < img.h; i++) {
    for (int j = 0; j < img.w; j++) {
      if (img(i,j)) {
	if (img(i,j) != skipcol)
	  q.emplace(j,i,img(i,j));
	done(i,j) = 1;
      }
    }
  }
  while (q.size()) {
    auto [j,i,c] = q.front();
    q.pop();
    for (int d = 0; d < 4; d++) {
      int ni = i+(d==0)-(d==1);
      int nj = j+(d==2)-(d==3);
      if (ni >= 0 && nj >= 0 && ni < img.h && nj < img.w && !done(ni,nj)) {
	img(ni,nj) = c;
	done(ni,nj) = 1;
	q.emplace(nj,ni,c);
      }
    }
  }
  return img;
}


vImage splitColumns(Image_ img) {
  if (img.w*img.h <= 0) return {};
  vector<Image> ret(img.w);
  for (int j = 0; j < img.w; j++) {
    ret[j].p = {j,0};
    ret[j].sz = {1,img.h};
    ret[j].mask.resize(img.h);
    for (int i = 0; i < img.h; i++)
      ret[j].mask[i] = img(i,j);
  }
  return ret;
}
vImage splitRows(Image_ img) {
  if (img.w*img.h <= 0) return {};
  vector<Image> ret(img.h);
  for (int i = 0; i < img.h; i++) {
    ret[i].p = {0,i};
    ret[i].sz = {img.w,1};
    ret[i].mask.resize(img.w);
    for (int j = 0; j < img.w; j++)
      ret[i].mask[j] = img(i,j);
  }
  return ret;
}


Image half(Image_ img, int id) {
  assert(id >= 0 && id < 4);
  if (id == 0) {
    return core::subImage(img, {0,0}, {img.w/2, img.h});
  } else if (id == 1) {
    return core::subImage(img, {img.w-img.w/2,0}, {img.w/2, img.h});
  } else if (id == 2) {
    return core::subImage(img, {0,0}, {img.w, img.h/2});
  } else if (id == 3) {
    return core::subImage(img, {0,img.h-img.h/2}, {img.w, img.h/2});
  }
  return badImg;
}


Image smear(Image_ img, int id) {
  assert(id >= 0 && id < 15);
  const pair<int,int> R = {1,0}, L = {-1,0}, D = {0,1}, U = {0,-1};
  const pair<int,int> X = {1,1}, Y = {-1,-1}, Z = {1,-1}, W = {-1,1};
  const vector<pair<int,int>> d[15] = {{R},{L},{D},{U},
				       {R,L},{D,U},
				       {R,L,D,U},
				       {X},{Y},{Z},{W},
				       {X,Y},{Z,W},
				       {X,Y,Z,W},
				       {R,L,D,U,X,Y,Z,W}};
  const int w = img.w;
  Image ret = img;


  for (auto [dx,dy] : d[id]) {
    int di = dy*w+dx;

    for (int i = 0; i < ret.h; i++) {
      int step = i == 0 || i == ret.h-1 ? 1 : max(ret.w-1,1);
      for (int j = 0; j < ret.w; j += step) {
	if (i-dy < 0 || j-dx < 0 || i-dy >= img.h || j-dx >= img.w) {
	  int steps = MAXSIDE;
	  if (dx ==-1) steps = min(steps, j+1);
	  if (dx == 1) steps = min(steps, img.w-j);
	  if (dy ==-1) steps = min(steps, i+1);
	  if (dy == 1) steps = min(steps, img.h-i);

	  int ind = i*w+j;
	  int end_ind = ind+steps*di;
	  int c = 0;
	  for (; ind != end_ind; ind += di) {
	    if (img.mask[ind]) c = img.mask[ind];
	    if (c) ret.mask[ind] = c;
	  }
	}
      }
    }
  }

  return ret;
}


Image mirror2(Image_ a, Image_ line) {
  Image ret;
  if (line.w > line.h) {
    ret = rigid(a,5);
    ret.x = a.x;
    ret.y = line.y*2+line.h-a.y-a.h;
  } else {
    ret = rigid(a,4);
    ret.y = a.y;
    ret.x = line.x*2+line.w-a.x-a.w;
  }
  return ret;
}

vImage gravity(Image_ in, int d) {
  vImage pieces = splitAll(in);
  Image room = hull0(in);
  int dx = (d==0)-(d==1);
  int dy = (d==2)-(d==3);

  vImage ret;
  Image out = room;
  sort(pieces.begin(), pieces.end(), [dx,dy](Image_ a, Image_ b) {
      return (a.x-b.x)*dx+(a.y-b.y)*dy > 0;});
  for (Image p : pieces) {
    while (1) {
      p.x += dx;
      p.y += dy;
      for (int i = 0; i < p.h; i++) {
	for (int j = 0; j < p.w; j++) {
	  if (p(i,j) == 0) continue;
	  int x = j+p.x-out.x;
	  int y = i+p.y-out.y;
	  if (x < 0 || y < 0 || x >= out.w || y >= out.h || out(y,x)) {
	    p.x -= dx;
	    p.y -= dy;
	    goto done;
	  }
	}
      }
    }
  done:
    ret.push_back(p);
    out = compose(out, p, 3);
  }
  return ret;
}



Image myStack(vImage_ lens, int id) {
  int n = lens.size();
  if (!n) return badImg;
  vector<pair<int,int>> order(n);
  for (int i = 0; i < n; i++) {
    order[i] = {lens[i].w*lens[i].h,i};
  }
  sort(order.begin(), order.end());

  Image out = lens[order[0].second];
  for (int i = 1; i < n; i++)
    out = myStack(out,lens[order[i].second],id);
  return out;
}

Image stackLine(vImage_ shapes) {
  int n = shapes.size();
  if (!n) return badImg;
  else if (n == 1) return shapes[0];
  vector<int> xs(n), ys(n);
  for (int i = 0; i < n; i++) {
    xs[i] = shapes[i].x;
    ys[i] = shapes[i].y;
  }
  sort(xs.begin(), xs.end());
  sort(ys.begin(), ys.end());
  int xmin = 1e9, ymin = 1e9;
  for (int i = 1; i < n; i++) {
    xmin = min(xmin, xs[i]-xs[i-1]);
    ymin = min(ymin, ys[i]-ys[i-1]);
  }
  int dx = 1, dy = 0;
  if (xmin < ymin) dx = 0, dy = 1;

  vector<pair<int,int>> order(n);
  for (int i = 0; i < shapes.size(); i++) {
    order[i] = {shapes[i].x*dx+shapes[i].y*dy,i};
  }
  sort(order.begin(), order.end());

  Image out = shapes[order[0].second];
  for (int i = 1; i < n; i++)
    out = myStack(out,shapes[order[i].second],dy);
  return out;
}


Image composeGrowingSlow(vImage_ imgs) {
  int n = imgs.size();
  if (!n) return badImg;

  vector<pair<int,int>> order(n);
  for (int i = 0; i < n; i++) {
    order[i] = {core::count(imgs[i]), i};
  }
  sort(order.rbegin(), order.rend());

  Image ret = imgs[order[0].second];
  for (int i = 1; i < n; i++)
    ret = compose(ret, imgs[order[i].second], 0);
  return ret;
}


Image composeGrowing(vImage_ imgs) {
  int n = imgs.size();
  if (!n) return badImg;
  if (n == 1) return imgs[0];

  int minx = 1e9, miny = 1e9, maxx = -1e9, maxy = -1e9;
  for (Image_ img : imgs) {
    minx = min(minx, img.x);
    miny = min(miny, img.y);
    maxx = max(maxx, img.x+img.w);
    maxy = max(maxy, img.y+img.h);
  }

  point rsz = {maxx-minx, maxy-miny};
  if (max(rsz.x, rsz.y) > MAXSIDE || rsz.x*rsz.y > MAXAREA || rsz.x <= 0 || rsz.y <= 0)
    return badImg;

  vector<pair<int,int>> order(n);
  for (int i = 0; i < n; i++) {
    order[i] = {core::count(imgs[i]), i};
  }
  sort(order.rbegin(), order.rend());

  Image ret = core::empty(point{minx, miny}, rsz);
  for (auto [cnt,imgi] : order) {
    Image_ img = imgs[imgi];
    int dx = img.x-ret.x, dy = img.y-ret.y;
    for (int i = 0; i < img.h; i++) {
      for (int j = 0; j < img.w; j++) {
	if (img(i,j))
	  ret(i+dy,j+dx) = img(i,j);
      }
    }
  }
  //assert(ret == composeGrowingSlow(imgs));
  return ret;
}


Image pickUnique(vImage_ imgs, int id) {
  assert(id == 0);

  int n = imgs.size();
  if (!n) return badImg;

  //Pick the one with the unique color
  vector<int> mask(n);
  vector<int> cnt(10);
  for (int i = 0; i < n; i++) {
    mask[i] = core::colMask(imgs[i]);
    for (int c = 0; c < 10; c++) {
      if (mask[i]>>c&1) cnt[c]++;
    }
  }
  int reti = -1;
  for (int i = 0; i < n; i++) {
    for (int c = 0; c < 10; c++) {
      if (mask[i]>>c&1) {
	if (cnt[c] == 1) {
	  if (reti == -1) reti = i;
	  else return badImg;
	}
      }
    }
  }
  if (reti == -1) return badImg;
  return imgs[reti];
}
#include "precompiled_stl.hpp"
/*#include <vector>
#include <cassert>
#include <queue>
#include <tuple>
#include <functional>*/
using namespace std;

#include "utils.hpp"
#include "core_functions.hpp"
#include "image_functions.hpp"

Image Col(int id) {
  assert(id >= 0 && id < 10);
  return core::full({0,0}, {1,1}, id);
}
Image Pos(int dx, int dy) {
  return core::full({dx,dy},{1,1});
}
Image Square(int id) {
  assert(id >= 1);
  return core::full({0,0}, {id,id});
}
Image Line(int orient, int id) {
  assert(id >= 1);
  int w = id, h = 1;
  if (orient) swap(w,h);
  return core::full({0,0}, {w,h});
}

Image getPos(Image_ img) {
  return core::full(img.p, {1,1}, core::majorityCol(img));
}
Image getSize(Image_ img) {
  return core::full({0,0}, img.sz, core::majorityCol(img));
}
Image hull(Image_ img) {
  return core::full(img.p, img.sz, core::majorityCol(img));
}
Image toOrigin(Image img) {
  img.p = {0,0};
  return img;
}

Image getW(Image_ img, int id) {
  return core::full({0,0}, {img.w,id ? img.w : 1}, core::majorityCol(img));
}
Image getH(Image_ img, int id) {
  return core::full({0,0}, {id ? img.h : 1, img.h}, core::majorityCol(img));
}

Image hull0(Image_ img) {
  return core::full(img.p, img.sz, 0);
}
Image getSize0(Image_ img) {
  return core::full({0,0}, img.sz, 0);
}



Image Move(Image img, Image_ p) {
  img.x += p.x;
  img.y += p.y;
  return img;
}

Image filterCol(Image_ img, Image_ palette) {
  Image ret = img;
  int palMask = core::colMask(palette);
  for (int i = 0; i < img.h; i++)
    for (int j = 0; j < img.w; j++)
      if ((palMask>>img(i,j)&1) == 0)
	ret(i,j) = 0;
  return ret;
}

Image filterCol(Image_ img, int id) {
  assert(id >= 0 && id < 10);
  if (id == 0) return invert(img);
  else return filterCol(img, Col(id));
}



Image broadcast(Image_ col, Image_ shape, int include0) { //include0 = 1
  if (col.w*col.h == 0 || shape.w*shape.h == 0) return badImg;

  if (shape.w%col.w == 0 && shape.h%col.h == 0) {
    Image ret = shape;
    int dh = shape.h/col.h, dw = shape.w/col.w;
    for (int ii = 0; ii < col.h; ii++) {
      for (int jj = 0; jj < col.w; jj++) {
	int c = col(ii,jj);
	for (int i = ii*dh; i < ii*dh+dh; i++)
	  for (int j = jj*dw; j < jj*dw+dw; j++)
	    ret(i,j) = c;
      }
    }
    return ret;
  }

  Image ret = shape;
  double fh = col.h*1./shape.h, fw = col.w*1./shape.w;

  const double eps = 1e-9;
  double w0[10] = {};
  for (int c : col.mask) w0[c] += 1e-6;

  double tot = fh*fw;
  double weight[10];
  for (int i = 0; i < shape.h; i++) {
    for (int j = 0; j < shape.w; j++) {
      copy_n(w0, 10, weight);

      double r0 = i*fh+eps, r1 = (i+1)*fh-eps;
      double c0 = j*fw+eps, c1 = (j+1)*fw-eps;

      int guess = !include0;
      for (int y = r0; y < r1; y++) {
	double wy = min((double)y+1,r1)-max((double)y,r0);
	for (int x = c0; x < c1; x++) {
	  double wx = min((double)x+1,c1)-max((double)x,c0);
	  char c = col(y,x);
	  weight[c] += wx*wy;
	  guess = c;
	}
      }

      if (weight[guess]*2 > tot) {
	ret(i,j) = guess;
	continue;
      }

      int maj = !include0;
      double w = weight[maj];
      for (int c = 1; c < 10; c++) {
	if (weight[c] > w) maj = c, w = weight[c];
      }
      ret(i,j) = maj;
      //point sz = {max(c1-c0, 1), max(r1-r0, 1)};
      //ret(i,j) = core::majorityCol(core::subImage(col, {c0,r0}, sz), include0);
    }
  }
  return ret;
}

Image colShape(Image_ col, Image_ shape) {
  if (shape.w*shape.h == 0 || col.w*col.h == 0) return badImg;
  Image ret = broadcast(col, getSize(shape));
  ret.p = shape.p;
  for (int i = 0; i < ret.h; i++)
    for (int j = 0; j < ret.w; j++)
      if (!shape(i,j)) ret(i,j) = 0;
  return ret;
}
Image colShape(Image_ shape, int id) {
  assert(id >= 0 && id < 10);
  Image ret = shape;
  for (char&c : ret.mask)
    c = c ? id : 0;
  return ret;
}



Image compress(Image_ img, Image_ bg) { // bg = Col(0)
  int bgmask = core::colMask(bg);

  int xmi = 1e9, xma = 0, ymi = 1e9, yma = 0;
  for (int i = 0; i < img.h; i++) {
    for (int j = 0; j < img.w; j++) {
      if ((bgmask>>img(i,j)&1) == 0) {
	xmi = min(xmi, j);
	xma = max(xma, j);
	ymi = min(ymi, i);
	yma = max(yma, i);
      }
    }
  }
  Image ret;
  if (xmi == 1e9) {
    ret.p = {0,0};
    ret.sz = {0,0};
    return ret;
  }
  ret.p = img.p + point{xmi, ymi};
  ret.sz = {xma-xmi+1, yma-ymi+1};
  ret.mask.resize(ret.h*ret.w);
  for (int i = ymi; i <= yma; i++) {
    for (int j = xmi; j <= xma; j++) {
      ret(i-ymi,j-xmi) = img(i,j);
    }
  }
  return ret;
}



Image embedSlow(Image_ img, Image_ shape) {
  Image ret = core::empty(shape.p, shape.sz);
  point d = shape.p-img.p;
  for (int i = 0; i < ret.h; i++)
    for (int j = 0; j < ret.w; j++)
      ret(i,j) = img.safe(i+d.y, j+d.x);
  return ret;
}

Image embed(Image_ img, Image_ shape) {
  Image ret = core::empty(shape.p, shape.sz);
  point d = shape.p-img.p;
  int sx = max(0,-d.x);
  int sy = max(0,-d.y);
  int ex = min(ret.w,img.w-d.x);
  int ey = min(ret.h,img.h-d.y);

  int retw = ret.w, imgw = img.w, off = d.y*img.w+d.x;
  for (int i = sy; i < ey; i++)
    for (int j = sx; j < ex; j++)
      ret.mask[i*retw+j] = img.mask[i*imgw+j+off];
  //assert(ret == embedSlow(img,shape));
  return ret;
}


Image compose(Image_ a, Image_ b, const function<int(int,int)>&f, int overlap_only) {
  Image ret;
  if (overlap_only == 1) {
    ret.p = {max(a.p.x,b.p.x),
	     max(a.p.y,b.p.y)};
    point ra = a.p+a.sz, rb = b.p+b.sz;
    ret.sz = {min(ra.x,rb.x),min(ra.y,rb.y)};
    ret.sz = ret.sz-ret.p;
    if (ret.w <= 0 || ret.h <= 0) return badImg;
  } else if (overlap_only == 0) {
    ret.p = {min(a.p.x,b.p.x),
	     min(a.p.y,b.p.y)};
    point ra = a.p+a.sz, rb = b.p+b.sz;
    ret.sz = {max(ra.x,rb.x),max(ra.y,rb.y)};
    ret.sz = ret.sz-ret.p;
  } else if (overlap_only == 2) {
    ret.p = a.p;
    ret.sz = a.sz;
  } else assert(0);
  if (ret.w > MAXSIDE || ret.h > MAXSIDE || ret.w*ret.h > MAXAREA) return badImg;
  ret.mask.assign(ret.w*ret.h, 0);
  point da = ret.p-a.p;
  point db = ret.p-b.p;
  for (int i = 0; i < ret.h; i++) {
    for (int j = 0; j < ret.w; j++) {
      int ca = a.safe(i+da.y, j+da.x);
      int cb = b.safe(i+db.y, j+db.x);
      ret(i,j) = f(ca,cb);
    }
  }
  return ret;
}


Image compose(Image_ a, Image_ b, int id) { //id = 0
  if (id == 0) {
    return compose(a, b, [](int a, int b) {return b ? b : a;}, 0); //a then b, inside either
  } else if (id == 1) {
    return compose(a, b, [](int a, int b) {return b ? b : a;}, 1); //a then b, inside both
  } else if (id == 2) {
    return compose(a, b, [](int a, int b) {return b ? a : 0;}, 1); //a masked by b
  } else if (id == 3) {
    return compose(a, b, [](int a, int b) {return b ? b : a;}, 2); //a then b, inside of a
  } else if (id == 4) {
    return compose(a, b, [](int a, int b) {return b ? 0 : a;}, 2); //a masked by inverse of b, inside of a
  } else assert(id >= 0 && id < 5);
  return badImg;
}

Image outerProductIS(Image_ a, Image_ b) {
  if (a.w*b.w > MAXSIDE || a.h*b.h > MAXSIDE || a.w*b.w*a.h*b.h > MAXAREA) return badImg;
  point rpos = {a.p.x*b.w+b.p.x,
		a.p.y*b.h+b.p.y};
  Image ret = core::empty(rpos, {a.w*b.w, a.h*b.h});
  for (int i = 0; i < a.h; i++)
    for (int j = 0; j < a.w; j++)
      for (int k = 0; k < b.h; k++)
	for (int l = 0; l < b.w; l++)
	  ret(i*b.h+k, j*b.w+l) = a(i,j) * !!b(k,l);
  return ret;
}
Image outerProductSI(Image_ a, Image_ b) {
  if (a.w*b.w > MAXSIDE || a.h*b.h > MAXSIDE || a.w*b.w*a.h*b.h > MAXAREA) return badImg;
  point rpos = {a.p.x*b.w+b.p.x,
		a.p.y*b.h+b.p.y};
  Image ret = core::empty(rpos, {a.w*b.w, a.h*b.h});
  for (int i = 0; i < a.h; i++)
    for (int j = 0; j < a.w; j++)
      for (int k = 0; k < b.h; k++)
	for (int l = 0; l < b.w; l++)
	  ret(i*b.h+k, j*b.w+l) = (a(i,j)>0) * b(k,l);
  return ret;
}



Image Fill(Image_ a) {
  Image ret = core::full(a.p, a.sz, core::majorityCol(a));
  vector<pair<int,int>> q;
  for (int i = 0; i < a.h; i++)
    for (int j = 0; j < a.w; j++)
      if ((i == 0 || j == 0 || i == a.h-1 || j == a.w-1) && !a(i,j)) {
	q.emplace_back(i,j);
	ret(i,j) = 0;
      }
  while (q.size()) {
    auto [r,c] = q.back();
    q.pop_back();
    for (int d = 0; d < 4; d++) {
      int nr = r+(d==2)-(d==3);
      int nc = c+(d==0)-(d==1);
      if (nr >= 0 && nr < a.h && nc >= 0 && nc < a.w && !a(nr,nc) && ret(nr,nc)) {
	q.emplace_back(nr,nc);
	ret(nr,nc) = 0;
      }
    }
  }
  return ret;
}

Image interior(Image_ a) {
  return compose(Fill(a), a, [](int x, int y) {return y ? 0 : x;}, 0);
}

Image border(Image_ a) {
  Image ret = core::empty(a.p, a.sz);
  vector<pair<int,int>> q;
  for (int i = 0; i < a.h; i++)
    for (int j = 0; j < a.w; j++)
      if (i == 0 || j == 0 || i == a.h-1 || j == a.w-1) {
	if (!a(i,j))
	  q.emplace_back(i,j);
	ret(i,j) = 1;
      }
  while (q.size()) {
    auto [r,c] = q.back();
    q.pop_back();
#define DO(nr,nc) {if (!ret(nr,nc)) { ret(nr,nc) = 1; if (!a(nr,nc)) q.emplace_back(nr,nc); }}
    if (r > 0) {
      if (c > 0) DO(r-1,c-1);
      DO(r-1,c);
      if (c+1 < a.w) DO(r-1,c+1);
    }
    if (r+1 < a.h) {
      if (c > 0) DO(r+1,c-1);
      DO(r+1,c);
      if (c+1 < a.w) DO(r+1,c+1);
    }

    if (c > 0) DO(r,c-1);
    if (c+1 < a.w) DO(r,c+1);
#undef DO
    /*for (int nr : {r-1,r,r+1}) {
      for (int nc : {c-1,c,c+1}) {
	if (nr >= 0 && nr < a.h && nc >= 0 && nc < a.w && !ret(nr,nc)) {
    if (!a(nr,nc))
	  q.emplace_back(nr,nc);
	  ret(nr,nc) = 1;
	}
      }
      }*/
  }
    for (int i = 0; i < a.mask.size(); i++)
      ret.mask[i] = ret.mask[i]*a.mask[i];
  return ret;
}


Image alignx(Image_ a, Image_ b, int id) {
  assert(id >= 0 && id < 5);
  Image ret = a;
  if (id == 0)      ret.x = b.x-a.w;
  else if (id == 1) ret.x = b.x;
  else if (id == 2) ret.x = b.x+(b.w-a.w)/2;
  else if (id == 3) ret.x = b.x+b.w-a.w;
  else if (id == 4) ret.x = b.x+b.w;
  return ret;
}
Image aligny(Image_ a, Image_ b, int id) {
  assert(id >= 0 && id < 5);
  Image ret = a;
  if (id == 0)      ret.y = b.y-a.h;
  else if (id == 1) ret.y = b.y;
  else if (id == 2) ret.y = b.y+(b.h-a.h)/2;
  else if (id == 3) ret.y = b.y+b.h-a.h;
  else if (id == 4) ret.y = b.y+b.h;
  return ret;
}
Image align(Image_ a, Image_ b, int idx, int idy) {
  assert(idx >= 0 && idx < 6);
  assert(idy >= 0 && idy < 6);
  Image ret = a;
  if      (idx == 0) ret.x = b.x-a.w;
  else if (idx == 1) ret.x = b.x;
  else if (idx == 2) ret.x = b.x+(b.w-a.w)/2;
  else if (idx == 3) ret.x = b.x+b.w-a.w;
  else if (idx == 4) ret.x = b.x+b.w;

  if      (idy == 0) ret.y = b.y-a.h;
  else if (idy == 1) ret.y = b.y;
  else if (idy == 2) ret.y = b.y+(b.h-a.h)/2;
  else if (idy == 3) ret.y = b.y+b.h-a.h;
  else if (idy == 4) ret.y = b.y+b.h;
  return ret;
}



Image align(Image_ a, Image_ b) {
  //Find most matching color and align a to b using it
  Image ret = a;
  int match_size = 0;
  for (int c = 1; c < 10; c++) {
    Image ca = compress(filterCol(a, c));
    Image cb = compress(filterCol(b, c));
    if (ca.mask == cb.mask) {
      int cnt = core::count(ca);
      if (cnt > match_size) {
	match_size = cnt;
	ret.p = a.p+cb.p-ca.p;
      }
    }
  }
  if (match_size == 0) return badImg;
  return ret;
}

Image replaceCols(Image_ base, Image_ cols) {
  Image ret = base;
  Image done = core::empty(base.p,base.sz);
  point d = base.p-cols.p;
  for (int i = 0; i < base.h; i++) {
    for (int j = 0; j < base.w; j++) {
      if (!done(i,j) && base(i,j)) {
	int acol = base(i,j);
	int cnt[10] = {};
	vector<pair<int,int>> path;
	function<void(int,int)> dfs = [&](int r, int c) {
	  if (r < 0 || r >= base.h || c < 0 || c >= base.w || base(r,c) != acol || done(r,c)) return;
	  cnt[cols.safe(r+d.y,c+d.x)]++;
	  path.emplace_back(r,c);
	  done(r,c) = 1;
	  for (int nr : {r-1,r,r+1})
	    for (int nc : {c-1,c,c+1})
	      dfs(nr,nc);
	};
	dfs(i,j);
	pair<int,int> maj = {0,0};
	for (int c = 1; c < 10; c++) {
	  maj = max(maj, make_pair(cnt[c], -c));
	}
	for (auto [r,c] : path)
	  ret(r,c) = -maj.second;
      }
    }
  }
  return ret;
}

//Exploit symmetries?
Image center(Image_ img) {
  point sz = {(img.w+1)%2+1,
	      (img.h+1)%2+1};
  return core::full(img.p+(img.sz-sz)/2, sz);
}

Image transform(Image_ img, int A00, int A01, int A10, int A11) {
  if (img.w*img.h == 0) return img;
  Image c = center(img);
  point off = point{1-c.w,1-c.h}+(img.p-c.p)*2;
  auto t = [&](point p) {
    p = p*2+off;
    p = {A00*p.x+A01*p.y,
	 A10*p.x+A11*p.y};
    p = p-off;
    p.x >>= 1;
    p.y >>= 1;
    return p;
  };
  point corner[4] = {t({0,0}),t({img.w-1,0}),t({0,img.h-1}),t({img.w-1,img.h-1})};
  point a = corner[0], b = corner[0];
  for (int i = 1; i < 4; i++) {
    a.x = min(a.x, corner[i].x);
    a.y = min(a.y, corner[i].y);
    b.x = max(b.x, corner[i].x);
    b.y = max(b.y, corner[i].y);
  }
  Image ret = core::empty(img.p, b-a+point{1,1});
  for (int i = 0; i < img.h; i++) {
    for (int j = 0; j < img.w; j++) {
      point go = t({j,i})-a;
      ret(go.y,go.x) = img(i,j);
    }
  }
  return ret;
}

int mirrorHeuristic(Image_ img) {
  //Meant to be used for mirroring, flip either x or y, depending on center of gravity
  int cnt = 0, sumx = 0, sumy = 0;
  for (int i = 0; i < img.h; i++) {
    for (int j = 0; j < img.w; j++) {
      if (img(i,j)) {
	cnt++;
	sumx += j;
	sumy += i;
      }
    }
  }
  return abs(sumx*2-(img.w-1)*cnt) < abs(sumy*2-(img.h-1)*cnt);
}

Image rigid(Image_ img, int id) {
  if (id == 0) return img;
  else if (id == 1) return transform(img, 0, 1,-1, 0); //CCW
  else if (id == 2) return transform(img,-1, 0, 0,-1); //180
  else if (id == 3) return transform(img, 0,-1, 1, 0); //CW
  else if (id == 4) return transform(img,-1, 0 ,0, 1); //flip x
  else if (id == 5) return transform(img, 1, 0, 0,-1); //flip y
  else if (id == 6) return transform(img, 0, 1, 1, 0); //swap xy
  else if (id == 7) return transform(img, 0,-1,-1, 0); //swap other diagonal
  else if (id == 8) return rigid(img, 4+mirrorHeuristic(img));
  else assert(id >= 0 && id < 9);
  return badImg;
}

Image invert(Image img) {
  if (img.w*img.h == 0) return img;
  int mask = core::colMask(img);
  int col = 1;
  while (col < 10 && (mask>>col&1) == 0) col++;
  if (col == 10) col = 1;

  for (int i = 0; i < img.h; i++)
    for (int j = 0; j < img.w; j++)
      img(i,j) = img(i,j) ? 0 : col;
  return img;
}


Image interior2(Image_ a) {
  return compose(a, invert(border(a)), 2);
}



Image count(Image_ img, int id, int outType) {
  assert(id >= 0 && id < 7);
  assert(outType >= 0 && outType < 3);
  int num;
  if (id == 0)      num = core::count(img);
  else if (id == 1) num = core::countCols(img);
  else if (id == 2) num = core::countComponents(img);
  else if (id == 3) num = img.w;
  else if (id == 4) num = img.h;
  else if (id == 5) num = max(img.w,img.h);
  else if (id == 6) num = min(img.w,img.h);
  else assert(0);

  point sz;
  if (outType == 0) sz = {num,num};
  else if (outType == 1) sz = {num,1};
  else if (outType == 2) sz = {1,num};
  else assert(0);

  if (max(sz.x,sz.y) > MAXSIDE || sz.x*sz.y > MAXAREA) return badImg;
  return core::full(sz, core::majorityCol(img));
}


Image myStack(Image_ a, Image b, int orient) {
  assert(orient >= 0 && orient <= 3);
  b.p = a.p;
  if (orient == 0) { //Horizontal
    b.x += a.w;
  } else if (orient == 1) { //Vertical
    b.y += a.h;
  } else if (orient == 2) { //Diagonal
    b.x += a.w;
    b.y += a.h;
  } else { //Other diagonal, bottom-left / top-right
    Image c = a;
    c.y += b.h;
    b.x += a.w;
    return compose(c,b);
  }
  return compose(a,b);
}


Image wrap(Image_ line, Image_ area) {
  if (line.w*line.h == 0 || area.w*area.h == 0) return badImg;
  Image ans = core::empty(area.sz);
  for (int i = 0; i < line.h; i++) {
    for (int j = 0; j < line.w; j++) {
      int x = j, y = i;
      x += y/area.h * line.w;
      y %= area.h;

      y += x/area.w * line.h;
      x %= area.w;

      if (x >= 0 && y >= 0 && x < ans.w && y < ans.h)
	ans(y,x) = line(i,j);
    }
  }
  return ans;
}

Image smear(Image_ base, Image_ room, int id) {
  assert(id >= 0 && id < 7);
  const int arr[] = {1,2,4,8,3,12,15};
  int mask = arr[id];

  point d = room.p-base.p;

  Image ret = embed(base, hull(room));
  if (mask&1) {
    for (int i = 0; i < ret.h; i++) {
      char c = 0;
      for (int j = 0; j < ret.w; j++) {
	if (!room(i,j)) c = 0;
	else if (base.safe(i+d.y,j+d.x)) c = base(i+d.y,j+d.x);
	if (c) ret(i,j) = c;
      }
    }
  }

  if (mask>>1&1) {
    for (int i = 0; i < ret.h; i++) {
      char c = 0;
      for (int j = ret.w-1; j >= 0; j--) {
	if (!room(i,j)) c = 0;
	else if (base.safe(i+d.y,j+d.x)) c = base(i+d.y,j+d.x);
	if (c) ret(i,j) = c;
      }
    }
  }

  if (mask>>2&1) {
    for (int j = 0; j < ret.w; j++) {
      char c = 0;
      for (int i = 0; i < ret.h; i++) {
	if (!room(i,j)) c = 0;
	else if (base.safe(i+d.y,j+d.x)) c = base(i+d.y,j+d.x);
	if (c) ret(i,j) = c;
      }
    }
  }

  if (mask>>3&1) {
    for (int j = 0; j < ret.w; j++) {
      char c = 0;
      for (int i = ret.h-1; i >= 0; i--) {
	if (!room(i,j)) c = 0;
	else if (base.safe(i+d.y,j+d.x)) c = base(i+d.y,j+d.x);
	if (c) ret(i,j) = c;
      }
    }
  }

  return ret;
}

/*
Image smear(Image_ base, int id) {
  return smear(base, hull(base), id);
}
*/

Image extend(Image_ img, Image_ room) {
  if (img.w*img.h == 0) return badImg;
  Image ret = room;
  for (int i = 0; i < ret.h; i++) {
    for (int j = 0; j < ret.w; j++) {
      point p = point{j,i}+room.p-img.p;
      p.x = clamp(p.x, 0, img.w-1);
      p.y = clamp(p.y, 0, img.h-1);
      ret(i,j) = img(p.y,p.x);
    }
  }
  return ret;
}








Image pickMax(vImage_ v, const function<int(Image_)>& f) {
  if (v.empty()) return badImg;
  int ma = f(v[0]), maxi = 0;
  for (int i = 1; i < v.size(); i++) {
    int score = f(v[i]);
    if (score > ma) {
      ma = score;
      maxi = i;
    }
  }
  return v[maxi];
}

int maxCriterion(Image_ img, int id) {
  assert(id >= 0 && id < 14);
  switch (id) {
  case 0: return  core::count(img);
  case 1: return -core::count(img);
  case 2: return  img.w*img.h;
  case 3: return -img.w*img.h;
  case 4: return core::countCols(img);
  case 5: return -img.p.y;
  case 6: return  img.p.y;
  case 7: return  core::countComponents(img);
  case 8:
    {
      Image comp = compress(img);
      return comp.w*comp.h-core::count(comp);
    }
  case 9:
    {
      Image comp = compress(img);
      return -(comp.w*comp.h-core::count(comp));
    }
  case 10: return  core::count(interior(img));
  case 11: return -core::count(interior(img));
  case 12: return -img.p.x;
  case 13: return  img.p.x;
  }
  return -1;
}


Image pickMax(vImage_ v, int id) {
  return pickMax(v, [id](Image_ img) {return maxCriterion(img,id);});
}

vImage cut(Image_ img, Image_ a) {
  vector<Image> ret;
  Image done = core::empty(img.p,img.sz);
  point d = img.p-a.p;
  for (int i = 0; i < img.h; i++) {
    for (int j = 0; j < img.w; j++) {
      if (!done(i,j) && !a.safe(i+d.y,j+d.x)) {
	Image toadd = core::empty(img.p,img.sz);
	function<void(int,int)> dfs = [&](int r, int c) {
	  if (r < 0 || r >= img.h || c < 0 || c >= img.w || a.safe(r+d.y,c+d.x) || done(r,c)) return;
	  toadd(r,c) = img(r,c)+1;
	  done(r,c) = 1;
	  for (int nr : {r-1,r,r+1})
	    for (int nc : {c-1,c,c+1})
	      dfs(nr,nc);
	};
	dfs(i,j);
	toadd = compress(toadd);
	for (int i = 0; i < toadd.h; i++) {
	  for (int j = 0; j < toadd.w; j++) {
	    toadd(i,j) = max(0, toadd(i,j)-1);
	  }
	}
	ret.push_back(toadd);
      }
    }
  }
  return ret;
}


vImage splitCols(Image_ img, int include0) { //include0 = 0
  vector<Image> ret;
  int mask = core::colMask(img);
  for (int c = !include0; c < 10; c++) {
    if (mask>>c&1) {
      Image s = img;
      for (int i = 0; i < s.h; i++)
	for (int j = 0; j < s.w; j++)
	  s(i,j) = (s(i,j) == c ? c : 0);
      ret.push_back(s);
    }
  }
  return ret;
}

Image compose(vImage_ imgs, int id) {
  if (imgs.empty()) return badImg;
  Image ret = imgs[0];
  for (int i = 1; i < imgs.size(); i++)
    ret = compose(ret, imgs[i], id);
  return ret;
}

void getRegular(vector<int>&col) {
  int colw = col.size();

  for (int w = 1; w < colw; w++) {
    int s = -1;
    if (colw%(w+1) == w) { //No outer border
      s = w;
    } else if (colw%(w+1) == 1) { //Outer border
      s = 0;
    }
    if (s != -1) {
      int ok = 1;
      for (int i = 0; i < colw; i++) {
	if (col[i] != (i%(w+1) == s)) {
	  ok = 0;
	  break;
	}
      }
      if (ok) {
	return;
      }
    }
  }
  fill(col.begin(), col.end(), 0);
}

Image getRegular(Image_ img) {
  //Look for regular grid division in single color
  Image ret = img;
  vector<int> col(img.w,1), row(img.h,1);
  for (int i = 0; i < img.h; i++) {
    for (int j = 0; j < img.w; j++) {
      if (img(i,j) != img(i,0)) row[i] = 0;
      if (img(i,j) != img(0,j)) col[j] = 0;
    }
  }
  getRegular(col);
  getRegular(row);
  for (int i = 0; i < img.h; i++) {
    for (int j = 0; j < img.w; j++) {
      ret(i,j) = row[i] || col[j];
    }
  }
  return ret;
}


Image cutPickMax(Image_ a, Image_ b, int id) {
  return pickMax(cut(a,b), id);
}
Image regularCutPickMax(Image_ a, int id) {
  Image b = getRegular(a);
  return pickMax(cut(a,b), id);
}
Image splitPickMax(Image_ a, int id, int include0) { //include0 = 0
  return pickMax(splitCols(a, include0), id);
}

Image cutCompose(Image_ a, Image_ b, int id) {
  auto v = cut(a,b);
  for (Image& img : v) img = toOrigin(img);
  return compose(v, id);
}
Image regularCutCompose(Image_ a, int id) {
  Image b = getRegular(a);
  auto v = cut(a,b);
  for (Image& img : v) img = toOrigin(img);
  return compose(v, id);
}
Image splitCompose(Image_ a, int id, int include0) { //include0 = 0
  auto v = splitCols(a, include0);
  for (Image& img : v) img = toOrigin(compress(img));
  return compose(v, id);
}

Image cutIndex(Image_ a, Image_ b, int ind) {
  auto v = cut(a,b);
  if (ind < 0 || ind >= (int)v.size()) return badImg;
  return v[ind];
}


vImage pickMaxes(vImage_ v, function<int(Image_)> f, int invert = 0) {
  int n = v.size();
  if (!n) return {};
  vector<int> score(n);
  int ma = -1e9;
  for (int i = 0; i < n; i++) {
    score[i] = f(v[i]);
    if (!i || score[i] > ma)
      ma = score[i];
  }
  vector<Image> ret_imgs;
  for (int i = 0; i < n; i++)
    if ((score[i] == ma) ^ invert)
      ret_imgs.push_back(v[i]);
  return ret_imgs;
}

vImage pickMaxes(vImage_ v, int id) {
  return pickMaxes(v, [id](Image_ img){return maxCriterion(img, id);}, 0);
}
vImage pickNotMaxes(vImage_ v, int id) {
  return pickMaxes(v, [id](Image_ img){return maxCriterion(img, id);}, 1);
}

Image cutPickMaxes(Image_ a, Image_ b, int id) {
  return compose(pickMaxes(cut(a,b), id), 0);
}
Image splitPickMaxes(Image_ a, int id) {
  return compose(pickMaxes(splitCols(a), id), 0);
}

//Return a single color
//Cut into at least 2 pieces
//No nested pieces
//Must touch at least 2 opposite sides
//Smallest piece should be as big as possible

Image heuristicCut(Image_ img) {
  int ret = core::majorityCol(img, 1);
  int ret_score = -1;

  int mask = core::colMask(img);
  Image done = core::empty(img.p,img.sz);
  for (int col = 0; col < 10; col++) {
    if ((mask>>col&1) == 0) continue;
    fill(done.mask.begin(), done.mask.end(), 0);
    function<void(int,int)> edgy = [&](int r, int c) {
      if (r < 0 || r >= img.h || c < 0 || c >= img.w || img(r,c) != col || done(r,c)) return;
      done(r,c) = 1;
      for (int nr : {r-1,r,r+1})
	for (int nc : {c-1,c,c+1})
	  edgy(nr,nc);
    };
    int top = 0, bot = 0, left = 0, right = 0;
    for (int i = 0; i < img.h; i++) {
      for (int j = 0; j < img.w; j++) {
	if (img(i,j) == col) {
	  if (i == 0) top = 1;
	  if (j == 0) left = 1;
	  if (i == img.h-1) bot = 1;
	  if (j == img.w-1) right = 1;
	}
	if ((i == 0 || j == 0 || i == img.h-1 || j == img.w-1) && img(i,j) == col && !done(i,j)) {
	  edgy(i,j);
	}
      }
    }

    if (!(top && bot || left && right)) continue;

    int score = 1e9, components = 0, nocontained = 1;
    for (int i = 0; i < img.h; i++) {
      for (int j = 0; j < img.w; j++) {
	int cnt = 0, contained = 1;
	if (!done(i,j) && img(i,j) != col) {
	  function<void(int,int)> dfs = [&](int r, int c) {
	    if (r < 0 || r >= img.h || c < 0 || c >= img.w) return;
	    if (img(r,c) == col) {
	      if (done(r,c)) contained = 0;
	      return;
	    }
	    if (done(r,c)) return;
	    cnt++;
	    done(r,c) = 1;
	    for (int nr : {r-1,r,r+1})
	      for (int nc : {c-1,c,c+1})
		dfs(nr,nc);
	  };
	  dfs(i,j);
	  components++;
	  score = min(score, cnt);
	  if (contained) nocontained = 0;
	}
      }
    }
    if (components >= 2 && nocontained && score > ret_score) {
      ret_score = score;
      ret = col;
    }
  }
  return filterCol(img,ret);
}

vImage cut(Image_ img) {
  return cut(img, heuristicCut(img));
}

Image cutPickMax(Image_ a, int id) {
  return cutPickMax(a, heuristicCut(a), id);
}
Image cutIndex(Image_ a, int ind) {
  return cutIndex(a, heuristicCut(a), ind);
}
Image cutPickMaxes(Image_ a, int id) {
  return cutPickMaxes(a, heuristicCut(a), id);
}



Image repeat(Image_ a, Image_ b, int pad) { //pad = 0
  if (a.w*a.h <= 0 || b.w*b.h <= 0) return badImg;
  Image ret;
  ret.p = b.p;
  ret.sz = b.sz;
  ret.mask.resize(ret.w*ret.h,0);

  const int W = a.w+pad, H = a.h+pad;
  int ai  = ((b.y-a.y)%H+H)%H;
  int aj0 = ((b.x-a.x)%W+W)%W;
  for (int i = 0; i < ret.h; i++) {
    int aj = aj0;
    for (int j = 0; j < ret.w; j++) {
      if (ai < a.h && aj < a.w)
	ret(i,j) = a(ai,aj);
      if (++aj == W) aj = 0;
    }
    if (++ai == H) ai = 0;
  }
  return ret;
}

Image mirror(Image_ a, Image_ b, int pad) { //pad = 0
  if (a.w*a.h <= 0 || b.w*b.h <= 0) return badImg;
  Image ret;
  ret.p = b.p;
  ret.sz = b.sz;
  ret.mask.resize(ret.w*ret.h);
  const int W = a.w+pad, H = a.h+pad;
  const int W2 = W*2, H2 = H*2;
  int ai  = ((b.y-a.y)%H2+H2)%H2;
  int aj0 = ((b.x-a.x)%W2+W2)%W2;
  for (int i = 0; i < ret.h; i++) {
    int aj = aj0;
    for (int j = 0; j < ret.w; j++) {
      int x = -1, y = -1;
      if (aj < a.w) x = aj;
      else if (aj >= W && aj < W+a.w) x = W+a.w-1-aj;
      if (ai < a.h) y = ai;
      else if (ai >= H && ai < H+a.h) y = H+a.h-1-ai;
      if (x != -1 && y != -1)
	ret(i,j) = a(y,x);
      if (++aj == W2) aj = 0;
    }
    if (++ai == H2) ai = 0;
  }
  return ret;
}

Image majCol(Image_ img) {
  return Col(core::majorityCol(img));
}
#include <stdio.h>
#include <string>
using std::string;
#include "load.hpp"

const int loading_len = 40;

void Loader::operator()() {
  long long a = counter++, b = n;
  const char*title = text.c_str();

  long long pb = prev*b*2, x = a*2000+b;
  if (pb > x-b*2 && pb <= x) return;

#pragma omp critical
  {
    long long p = prev = x/(2*b);

    if (title) printf("%s ", title);
    printf("|");
    int dist = (a*2LL*loading_len+(b>>1))/b;
    for (int i = 0; i < dist>>1; i++)
      printf("=");
    if (dist&1)
      printf("-");
    for (int i = (dist+1)>>1; i < loading_len; i++)
      printf(" ");
    printf("| %lld / %lld (%lld.%lld%%)\r", a, b, p/10, p%10);
    fflush(stdout);
  }
}

Loader::~Loader() {
#pragma omp critical
  {
    const char*title = text.c_str();
    if (title) {
      for (int i = 0; title[i]; i++)
	printf(" ");
    }
    for (int i = 0; i < loading_len+35+5; i++)
      printf(" ");
    printf("\r");
    fflush(stdout);
    if (title && keep_title) {
      printf("%s \n", title);
    }
  }
}
#include "runner.hpp"
#include <stdio.h>
#include <stdlib.h>

#include <unistd.h>

int main(int argc, char**argv) {
  //rankFeatures();
  //evalNormalizeRigid();
  //evalTasks();
  //bruteSubmission();
  //bruteSolve();
  int only_sid = -1;
  if (argc >= 2) {
    only_sid = atoi(argv[1]);
    printf("Running only task # %d\n", only_sid);
  }
  int maxdepth = -1;
  if (argc >= 3) {
    maxdepth = atoi(argv[2]);
    printf("Using max depth %d\n", maxdepth);
  }
  run(only_sid, maxdepth);
}
#include "precompiled_stl.hpp"
/*#include <cassert>
#include <functional>
#include <vector>
#include <iostream>
#include <map>
#include <cmath>*/
using namespace std;

#include "utils.hpp"
#include "core_functions.hpp"
#include "image_functions.hpp"
#include "visu.hpp"
#include "read.hpp"
#include "normalize.hpp"

vector<double> shapeFeatures(Image_ img, int col) {
  double fill_cnt = core::count(img);
  double comp_cnt = core::countComponents(img);
  double int_cnt = core::count(interior(img));
  vector<double> r =  {fill_cnt, 1./(fill_cnt+1e-3),
		       comp_cnt, 1./(comp_cnt+1e-3),
		       int_cnt, 1./(int_cnt+1e-3)};
  for (int c = 0; c < 10; c++)
    r.push_back((c == col)+2);

  point center2 = img.p*2+img.sz-point{1,1};
  for (int a = 0; a < 2; a++) {
    for (int b = 0; b < 2; b++) {
      for (int c = 0; c < 2; c++) {
	point dir;
	if (c) dir = point{a*2-1, b*2-1}*2;
	else dir = point{(a*2-1)*b, (a*2-1)*!b}*3;
	int ma = -50;
	for (int i = 0; i < img.h; i++)
	  for (int j = 0; j < img.w; j++)
	    if (img(i,j)) ma = max(ma, (point{i*2,j*2}-center2)*dir);
	r.push_back(ma+1000);
      }
    }
  }
  return r;
}

struct UniquePicker {
  vector<int> feature_dim;
  int save;
  UniquePicker(const vector<Image>&ins, int save_) {
    save = save_ | 1;

    int nfeats = -1;
    vector<vector<vector<double>>> feat; // inp, col, feat
    for (Image_ in : ins) {
      vector<vector<double>> features;
      vector<pair<Image, int>> split = core::splitCols(in);
      for (auto&[sh,col] : split) {
	features.push_back(shapeFeatures(sh,col));
	nfeats = features.back().size();
      }
      feat.push_back(features);
    }

    int nins = ins.size();
    vector<vector<int>> done(nins); // inp, cole
    vector<int> cols_left(nins);
    for (int i = 0; i < nins; i++) {
      done[i].assign(feat[i].size(), 0);
      cols_left[i] = feat[i].size();
    }

    if (nfeats == -1) return;

    while (1) {
      vector<double> score(nfeats, 1e3); //feat
      vector<vector<int>> picki(nins); //inp, feat -> col

      int found = 0;
      for (int inpi = 0; inpi < nins; inpi++) {
	if (!cols_left[inpi]) continue;
	found = 1;

	auto&f = feat[inpi];
	int cols = f.size();
	for (int fi = 0; fi < nfeats; fi++) {
	  pair<double,int> best = {-1,-1};
	  for (int i = 0; i < cols; i++) {
	    if (done[inpi][i]) continue;
	    double worst = 1e3;
	    for (int j = 0; j < cols; j++) {
	      if (done[inpi][j] || i == j) continue;
	      worst = min(worst, diff(f[i][fi], f[j][fi]));
	    }
	    best = max(best, make_pair(worst, i));
	  }
	  assert(best.second != -1);
	  score[fi] = min(score[fi], best.first);
	  picki[inpi].push_back(best.second);
	}
      }
      if (!found) break;

      pair<double,int> best = {-1,-1};
      for (int fi = 0; fi < nfeats; fi++) {
	best = max(best, make_pair(score[fi], fi));
	//cout << score[fi] << '-' << fi << "  ";
      }
      //cout << endl;
      //cout << best.first << '-' << best.second << endl;
      int pickf = best.second;
      assert(pickf != -1);
      feature_dim.push_back(pickf);
      for (int inpi = 0; inpi < nins; inpi++) {
	if (!cols_left[inpi]) continue;
	auto&col = picki[inpi];
	done[inpi][col[pickf]] = 1;
	cols_left[inpi]--;
      }
    }

    //for (int i : feature_dim) cout << i << endl;
    //cout << endl;
    //exit(0);
  }

  double diff(double a, double b) const {
    return a/(b+1e-5);
  }

  int getUnique(const vector<vector<double>>&features, const vector<int>&done, int fi) const {
    pair<double,int> best = {-1,-1};
    int n = features.size();
    for (int i = 0; i < n; i++) {
      if (done[i]) continue;
      double score = 0;
      for (int j = 0; j < features[i].size(); j++) {
	if (fi != -1) j = fi;
	double fscore = 1e3;
	for (int k = 0; k < n; k++) {
	  if (k == i || done[k]) continue;
	  fscore = min(fscore, diff(features[i][j], features[k][j]));
	}
	score = max(score, fscore);
	if (fi != -1) break;
      }
      best = max(best, make_pair(score, i));
      //cout << score << ' ';
    }
    //cout << endl;
    //exit(0);
    assert(best.first != -1);
    return best.second;
  }

  void getMap(Image_ in, int cols[10]) const {
    int j = 0, done = 0;
    for (int i = 0; i < 10; i++)
      if (save>>i&1) {
	done |= 1<<i;
	cols[i] = j++;
      }

    vector<vector<double>> features;
    vector<pair<Image, int>> split = core::splitCols(in);
    vector<int> splitcol;
    for (auto&[sh,col] : split) {
      if (save>>col&1) continue;
      features.push_back(shapeFeatures(sh, col));
      splitcol.push_back(col);
    }

    vector<int> order, fdone(features.size());
    for (int it = 0; it < features.size(); it++) {
      int fi = it < feature_dim.size() ? feature_dim[it] : -1;
      int i = getUnique(features, fdone, fi);
      fdone[i] = 1;
      order.push_back(i);
    }
    for (int&i : order) i = splitcol[i];

    for (int i : order) {
      if (done>>i&1) continue;
      cols[i] = j++;
      done |= 1<<i;
    }
    for (int i = 0; i < 10; i++) {
      if ((done>>i&1) == 0) {
	cols[i] = j++;
	done |= 1<<i;
      }
    }
    assert(done == (1<<10)-1);
  }
};






Image remapCols(Image_ img, int cols[10]) {
  Image r = img;
  for (int i = 0; i < r.h; i++)
    for (int j = 0; j < r.w; j++)
      r(i,j) = cols[img(i,j)];
  return r;
}



void remapCols(const vector<pair<Image,Image>>&train, vector<Simplifier>&sims) {
  int orin = 0, orout = 0, andin = ~0, andout = ~0;
  for (auto [in,out] : train) {
    int maskin = core::colMask(in);
    int maskout = core::colMask(out);
    orin |= maskin;
    andin &= maskin;
    orout |= maskout;
    andout &= maskout;
  }

  int save = andout & ~orin;
  if (orout == andout) save = andout;
  save |= andin&~orout;

  vector<Image> train_ins;
  for (auto&[in,out] : train) train_ins.push_back(in);
  UniquePicker up(train_ins, save | (orout&~orin));

  Simplifier ret;

  ret.in = [up](Image_ in) {
    int cols[10];
    up.getMap(in, cols);
    return remapCols(in, cols);
  };
  ret.out = [up](Image_ in, Image_ out) {
    int cols[10];
    up.getMap(in, cols);
    return remapCols(out, cols);
  };
  ret.rec = [up](Image_ in, Image_ out) {
    int cols[10];
    up.getMap(in, cols);
    int icols[10];
    for (int i = 0; i < 10; i++)
      icols[cols[i]] = i;
    return remapCols(out, icols);
  };

  sims.push_back(ret);
}


Image listCols(Image_ img, int extra) {
  int mask = core::colMask(img);
  int w = __builtin_popcount(mask);
  Image ret = core::full({w,2}, 9);
  int j = 0;
  for (int i = 0; i < 10; i++)
    if ((mask>>i&1) && !(extra>>i&1))
      ret(0,j++) = i;
  j = 0;
  for (int i = 0; i < 10; i++)
    if (extra>>i&1)
      ret(1,j++) = i;
  return ret;
}



void normalizeCols(vector<Sample>&sample) {
  Visu visu;

  int count = 0;
  for (Sample&s : sample) {
    auto train = s.train;
    vector<Simplifier> sims;
    remapCols(train, sims);
    if (sims.size()) {
      Simplifier&sim = sims[0];
      for (auto&[in,out] : train) {
	Image cp = out;
	out = sim.out(in, out);
	assert(cp == sim.rec(in, out));
	in = sim.in(in);
      }

      count++;
      visu.next(s.id);
      for (auto [in,out] : s.train)
	visu.add(in,out);
      visu.next(s.id);
      for (auto [in,out] : train) {
	visu.add(in,out);
      }
    }
    continue;
    vector<pair<int,int>> masks;
    int orin = 0, orout = 0, andin = ~0, andout = ~0;
    for (auto [in,out] : s.train) {
      masks.emplace_back(core::colMask(in), core::colMask(out));
      orin |= masks.back().first;
      andin &= masks.back().first;
      orout |= masks.back().second;
      andout &= masks.back().second;
    }
    int eq = core::countCols(s.train[0].first);

    int meaningCols = checkAll(s.train, [&](pair<Image,Image> p) {
	return core::countCols(p.first) == eq;});
    //if (__builtin_popcount(eq&~1) != 1) continue;

    if (meaningCols) {
      count++;
      visu.next(s.id);
      for (auto [in,out] : s.train)
	visu.add(in,out);
      visu.next(s.id);
      for (auto [in,out] : s.train) {
	//visu.add(in,core::embed(core::filterCol(out,10), out.sz));
	visu.add(listCols(in, andin&~orout),listCols(out, andout&~orin));
      }
    }
  }
  cout << count << " tasks" << endl;
}












//Needs rigid:
//0, 2, 12, 24, 52, (63), 66, 76, (91), 96, 130, (134), 138, 176, (182)

struct OrientationPicker {
  int feature_dim;
  OrientationPicker(const vector<Image>&ins) {
    feature_dim = 0;
    double best_score = 1.5;

    {
      double score = 1e3;
      for (Image_ in : ins) {
	score = min(score, max(in.w*1./in.h, in.h*1./in.w));
      }
      if (score > best_score) {
	feature_dim = 1;
	best_score = score;
      }
    }

    for (int c = 0; c < 10; c++) {
      double score = 1e3;
      for (Image_ in : ins) {
	auto [p,q] = inertia(in, c);
	double s = max(p*1./(q+1.), q*1./(p+1.));
	score = min(score, sqrt(s));
      }
      if (score > best_score) {
	feature_dim = 2+c;
	best_score = score;
      }
    }

    //TODO: improve heuristics for when to use
    {
      double scard = 1e3, sdiag = 1e3;
      for (Image_ in : ins) {
	double x[10] = {}, y[10] = {}, cnt[10] = {};
	colorMeans(in, x, y, cnt);

	double sx = 1e3, sy = 1e3, sxy = 1e3, syx = 1e3;
	int found = 0;
	for (int a = 1; a < 10; a++) {
	  for (int b = 1; b < a; b++) {
	    if (cnt[a] && cnt[b]) {
	      double dx = x[a]-x[b];
	      double dy = y[a]-y[b];
	      sx = min(sx, abs(dx)/(abs(dy)+1e-1));
	      sy = min(sy, abs(dy)/(abs(dx)+1e-1));
	      sxy = min(sxy, abs(dx+dy)/(abs(dx-dy)+1e-1));
	      syx = min(syx, abs(dx-dy)/(abs(dx+dy)+1e-1));
	      found = 1;
	      goto outside;
	    }
	  }
	}
      outside:
	if (!found) scard = sdiag = -1;
	scard = min(scard, max(sx, sy));
	sdiag = min(sdiag, max(sxy, syx));
      }
      scard /= 5., sdiag /= 5.;
      if (scard > best_score) {
	best_score = scard;
	feature_dim = 12;
      }
      if (sdiag > best_score) {
	best_score = sdiag;
	feature_dim = 13;
      }
    }
  }



  pair<double,double> inertia(Image_ img, char c) const {
    double x = 0, y = 0, xx = 0, yy = 0, cnt = 0;
    for (int i = 0; i < img.h; i++)
      for (int j = 0; j < img.w; j++)
	if (img(i,j) == c) {
	  x += j, xx += j*j;
	  y += i, yy += i*i;
	  cnt++;
	}
    if (cnt < 4) return {0.,0.};
    return {(xx*cnt-x*x), (yy*cnt-y*y)};
  }

  void colorMeans(Image_ in, double*x, double*y, double*cnt) const {
    for (int i = 0; i < in.h; i++)
      for (int j = 0; j < in.w; j++) {
	char c = in(i,j);
	x[c] += j;
	y[c] += i;
	cnt[c]++;
      }

    for (int c = 0; c < 10; c++) {
      if (cnt[c]) {
	x[c] /= cnt[c];
	y[c] /= cnt[c];
      }
    }
  }

  int getRigid(Image_ in) const {
    if (feature_dim == 0) return 0;
    else if (feature_dim == 1) return in.h > in.w ? 6 : 0;
    else if (feature_dim >= 2 && feature_dim < 12) {
      auto [p,q] = inertia(in, feature_dim-2);
      return p > q ? 6 : 0;
    } else if (feature_dim == 12 || feature_dim == 13) {

      double x[10] = {}, y[10] = {}, cnt[10] = {};
      colorMeans(in, x, y, cnt);

      for (int a = 1; a < 10; a++)
	for (int b = 1; b < a; b++) {
	  if (cnt[a] && cnt[b]) {
	    double dx = x[a]-x[b];
	    double dy = y[a]-y[b];
	    if (feature_dim == 12) {
	      if (abs(dx) > abs(dy))
		return dx < 0 ? 4 : 0;
	      else
		return dy < 0 ? 7 : 6;
	    } else {
	      if (abs(dx+dy) > abs(dx-dy))
		return dx+dy < 0 ? 7 : 0;
	      else
		return dx-dy < 0 ? 4 : 5;
	    }
	  }
	}
      return 0;
    }
    else assert(0);


    //Criteria:
    //Size should be horizontal
    //Colored lines should be vertical
    //2 colors should be top-left and bottom-right diagonally
    //2 colors should be left and right horizontally

    //Preserve:
    //Things touching one side / corner
    //Colored lines in output

  }
};

void normalizeRigid(const vector<pair<Image,Image>>&train, vector<Simplifier>&sims) {
  Simplifier ret;

  vector<Image> train_ins;
  for (auto&[in,out] : train) train_ins.push_back(in);
  OrientationPicker up(train_ins);

  ret.in = [up](Image_ in) {
    int rid = up.getRigid(in);
    return toOrigin(rigid(in, rid));
  };
  ret.out = [up](Image_ in, Image_ out) {
    int rid = up.getRigid(in);
    return toOrigin(rigid(out, rid));
  };
  ret.rec = [up](Image_ in, Image_ out) {
    int rid = up.getRigid(in);
    int inv[] = {0,3,2,1,4,5,6,7};
    return toOrigin(rigid(out, inv[rid]));
  };

  sims.push_back(ret);
}



void evalNormalizeRigid() {
  vector<Sample> sample = readAll("training", 100);//evaluation
  Visu visu;

  int place_count[11] = {};
  //#pragma omp parallel for
  for (int si = 0; si < sample.size(); si++) {
    Sample&s = sample[si];

    {
      vector<Simplifier> sims;
      remapCols(s.train, sims);
      for (auto&[in,out] : s.train)
	tie(in,out) = make_pair(sims[0].in(in), sims[0].out(in,out));
      for (auto&[in,out] : s.test)
	tie(in,out) = make_pair(sims[0].in(in), sims[0].out(in,out));
    }

    if (1) {
      visu.next(s.id);
      for (auto [in,out] : s.train) {
	visu.add(in,out);
      }
      vector<Simplifier> sims;
      normalizeRigid(s.train, sims);
      for (auto&[in,out] : s.train) {
	tie(in,out) = make_pair(sims[0].in(in), sims[0].out(in,out));
      }
      for (auto&[in,out] : s.test) {
	tie(in,out) = make_pair(sims[0].in(in), sims[0].out(in,out));
      }

      visu.next(s.id);
      for (auto [in,out] : s.train) {
	visu.add(in,out);
      }
      continue;
    }
  }
}



pair<Image,Image> Simplifier::operator()(Image_ a, Image_ b) {
  return {in(a), out(a,b)};
}

Simplifier normalizeCols(const vector<pair<Image,Image>>&train) {
  vector<Simplifier> sims;
  remapCols(train, sims);
  return sims[0];
}

Simplifier normalizeDummy(const vector<pair<Image,Image>>&train) {
  Simplifier ret;
  ret.in = [](Image_ in) { return in; };
  ret.out = [](Image_ in, Image_ out) { return out; };
  ret.rec = [](Image_ in, Image_ out) { return out; };
  return ret;
}
#include "precompiled_stl.hpp"
#include <chrono>
using namespace std;
#include "utils.hpp"
#include "read.hpp"
#include "normalize.hpp"
#include "core_functions.hpp"
#include "image_functions.hpp"
#include "image_functions2.hpp"

#include "visu.hpp"

#include "brute2.hpp"
#include "pieces.hpp"
#include "timer.hpp"
/*
struct Piece2 {
  vector<int> ind;
  int depth;
};

struct Pieces {
  vector<DAG> dag;
  vector<Piece2> piece;
  set<vector<int>> seen;
};
*/

extern int print_nodes;

ull hashVec(const vector<int>&vec) {
  ull r = 1;
  for (int v : vec) {
    r = r*1069388789821391921+v;
  }
  return r;
}

Pieces makePieces2(vector<DAG>&dag, vector<pair<Image,Image>> train, vector<point> out_sizes) {
  Timer set_time, piece_time, child_time;

  Pieces pieces;

  vector<int>&mem = pieces.mem;
  vector<int> depth_mem;

  int dags = dag.size();

  TinyHashMap seen;
  //set<vector<int>> seen;
  vector<queue<int>> q;

  auto add = [&](int d, const vector<int>&v) {
    assert(v.size() == dags);

    set_time.start();
    auto [memi, inserted] = seen.insert(hashVec(v), (int)mem.size());
    set_time.stop();

    if (inserted) {
      for (int i : v) {
	mem.push_back(i);
      }
      depth_mem.push_back(d);
    }

    if (inserted || depth_mem[memi/dags] > d) {
      depth_mem[memi/dags] = d;
      while (q.size() <= d) q.push_back({});
      q[d].push(memi);
    }
  };
  for (int i = 0; i < dag[0].givens; i++) {
    vector<int> v(dags,i);
    add(dag[0].tiny_node[i].depth, v);
  }

  /*
  for (int fi2 = 0; fi2 < dag[0].binary.size(); fi2++) {
    vector<int> v;
    v.reserve(dags);
    int ok = 1, depth = -1;
    for (DAG&d : dag) {
      int ni = d.binary[fi2];
      if (ni == -1) {
	ok = 0;
	break;
      }
      assert(ni >= 0 && ni < d.tiny_node.size());
      depth = max(depth, (int)d.tiny_node[ni].depth);
      v.push_back(ni);
    }
    if (ok) {
      pushQueue(depth, mem.size());
      add(v, false);
    }
  }
  */

  vector<vector<pair<int,int>>> slow_child(train.size()+1);
  vector<pair<int,vector<int>>> newi_list;

  piece_time.start();
  for (int depth = 0; depth < q.size(); depth++) {
    while (q[depth].size()) {
      int memi = q[depth].front();
      q[depth].pop();
      if (depth > depth_mem[memi/dags]) continue;
      assert(depth == depth_mem[memi/dags]);

      vector<int> ind(mem.begin()+memi, mem.begin()+memi+dags);
      {
	int ok = 1, maxdepth = -1;
	for (int i = 0; i < dags; i++) {
	  maxdepth = max(maxdepth, (int)dag[i].tiny_node[ind[i]].depth);
	  ok &= dag[i].tiny_node[ind[i]].ispiece;
	}
	if (ok && maxdepth >= depth) {
	  Piece3 p;
	  p.memi = memi;
	  p.depth = depth;
	  pieces.piece.push_back(p);
	}
	if (maxdepth < depth) continue;
      }

      newi_list.clear();

      child_time.start();
      {
	for (int i = 0; i <= train.size(); i++)
	  dag[i].tiny_node[ind[i]].child.legacy(slow_child[i]);
	vector<int> newi(dags), ci(dags); //current index into child[]
	int fi = 0;
	while (1) {
	next_iteration:
	  for (int i = 0; i <= train.size(); i++) {
	    auto&child = slow_child[i];//dag[i].node[ind[i]].child;
	    while (ci[i] < child.size() &&
		   child[ci[i]].first < fi) ci[i]++;
	    if (ci[i] == child.size()) goto finish;

	    int next_available_fi = child[ci[i]].first;
	    if (next_available_fi > fi) {
	      fi = next_available_fi;
	      goto next_iteration;

	    } else {

	      newi[i] = child[ci[i]].second;

	      if (newi[i] == -1) {
		fi++;
		goto next_iteration;
	      }
	    }
	  }
	  newi_list.emplace_back(fi, newi);
	  fi++;
	}
      finish:;
      }
      child_time.stop();

      for (auto&[fi, newi] : newi_list) {
	if (0) {
	  int i = train.size();
	  //auto&child = dag[i].node[ind[i]].child;
	  //auto it = lower_bound(child.begin(), child.end(), make_pair(fi, -1));
	  //if (it == child.end() || it->first != fi) {
	  int to = dag[i].tiny_node.getChild(ind[i], fi);
	  if (to == TinyChildren::None) {
	    string name = dag[i].funcs.getName(fi);
	    if (name.substr(0,4) == "Move") {
	      newi[i] = dag[i].applyFunc(ind[i], fi);
	      if (newi[i] != -1 && out_sizes.size())
		dag[i].applyFunc(newi[i], dag[i].funcs.findfi("embed 1"));
	    } else continue;
	  } else {
	    newi[i] = to; //it->second
	  }
	  if (newi[i] == -1) continue;
	}

	int new_depth = -1;
	for (int i = 0; i < dags; i++) {
	  new_depth = max(new_depth, (int)dag[i].tiny_node[newi[i]].depth);
	}

	int cost = dag[0].funcs.cost[fi];

	if (new_depth >= depth+cost) {
	  add(depth+cost, newi);
	}
      }
    }
  }
  piece_time.stop();
  piece_time.print("Piece loop time");
  set_time.print("Set time");
  child_time.print("Child looking time");


  auto lookFor = [&](vector<string> name_list) {
    vector<Image> look_imgs;
    vector<int> look_v;

    int di = 0;
    for (DAG&d : dag) {
      int p = 0;
      for (string name : name_list) {
	int fi = d.funcs.findfi(name);

	//auto&child = d.node[p].child;
	//auto it = lower_bound(child.begin(), child.end(), make_pair(fi,-1));
	//assert(it != child.end());
	int ret = d.tiny_node.getChild(p, fi);
	assert(ret >= 0);
	  //auto [fi_, ret] = *it;
	if (0) {//fi != fi_) {
	  cout << p << ' ' << di << " / " << train.size() << endl;
	  /*for (auto [fi,nxti] : child) {
	    string name = d.funcs.getName(fi);
	    if (name.substr(0,4) == "Move") {
	      cout << name << ' ';
	    }
	  }
	  cout << endl;*/
	}
	//assert(fi == fi_);
	p = ret;
	assert(p != -1);
      }
      look_v.push_back(p);
      look_imgs.push_back(d.getImg(p));
      /*if (n.isvec || n.img[0].sz != given_sizes[di][1]) {
	cout << "Bad" << endl;
	}*/
      di++;
    }
    if (!seen.insert(hashVec(look_v),0).second) cout << "Found indices" << endl;
    //exit(0);
  };

  /*if (out_sizes.size()) {
    lookFor({"compress", "toOrigin"});
    }*/
    /*
    lookFor({"repeat 0 1", "colShape 2", "Move -1 -1", "embed 1"});
    lookFor({"repeat 0 1", "colShape 2", "Move 1 1", "embed 1"});
    lookFor({"repeat 0 1", "smear 4", "colShape 1"});
    }*/
  /*lookFor({"filterCol 1", "interior", "colShape 2"});
    lookFor({"cut", "pickNotMaxes 11", "composeGrowing", "colShape 3", "embed 1"});
    lookFor({"filterCol 5", "makeBorder", "colShape 1", "embed 1"});
    lookFor({});
  */

  if (out_sizes.size() && print_nodes) {
    int nodes = 0;
    for (DAG&d : dag) nodes += d.tiny_node.size();
    cout << "Nodes:  " << nodes << endl;
    cout << "Pieces: " << pieces.piece.size() << endl;
  }
  pieces.dag = move(dag);
  //pieces.seen = move(seen);

  for (Piece3&p : pieces.piece) {
    for (int i = 0; i < pieces.dag.size(); i++) {
      int*ind = &mem[p.memi];
      assert(ind[i] >= 0 && ind[i] < pieces.dag[i].tiny_node.size());
    }
  }

  return pieces;
}
/*#include <vector>
#include <regex>
#include <cassert>
#include <iostream>*/
#include "precompiled_stl.hpp"
#include <experimental/filesystem>

using namespace std;
#include "utils.hpp"
#include "read.hpp"

Sample::Sample(string filename) {
  vector<Image> train_input, train_output;
  vector<Image> test_input, test_output;

  regex re(".*/([a-z0-9]{8}).json");
  smatch match;
  assert(std::regex_search(filename, match, re) && match.size() == 2);
  id = match.str(1);

  fp = fopen(filename.c_str(), "r");
  assert(fp);
  expect('{');
  while (1) {
    string train_test = getQuote();
    expect('[');
    while (1) {
      expect('{');
      while (1) {
	string input_output = getQuote();
	Image img = readImage();
	if (train_test == "train" && input_output == "input") {
	  train_input.push_back(img);
	} else if (train_test == "train" && input_output == "output") {
	  train_output.push_back(img);
	} else if (train_test == "test" && input_output == "input") {
	  test_input.push_back(img);
	} else if (train_test == "test" && input_output == "output") {
	  test_output.push_back(img);
	} else {
	  cerr << train_test << ' ' << input_output << endl;
	  assert(!"Unexpected tag");
	}
	if (end('}')) break;
      }
      if (end(']')) break;
    }
    if (end('}')) break;
  }
  assert(mygetc() == -1);
  assert(feof(fp));
  fclose(fp);

  assert(train_input.size() == train_output.size());
  for (int i = 0; i < (int)train_input.size(); i++) {
    train.emplace_back(train_input[i], train_output[i]);
  }
  if (test_input.size() == test_output.size()) {
    for (int i = 0; i < (int)test_input.size(); i++) {
      test.emplace_back(test_input[i], test_output[i]);
    }
  } else {
    assert(test_output.empty());
    for (int i = 0; i < (int)test_input.size(); i++) {
      test.emplace_back(test_input[i], badImg);
    }
  }
}

vector<Sample> Sample::split() {
  vector<Sample> ret;
  for (int i = 0; i < test.size(); i++) {
    ret.push_back(*this);
    Sample&s = ret.back();
    tie(s.test_in, s.test_out) = test[i];
    s.test = {{s.test_in, s.test_out}};
    s.id_ind = i;
  }
  return ret;
}

char Sample::mygetc() {
  char c = fgetc(fp);
  while (c == ' ' || c == '\n') c = fgetc(fp);
  return c;
}
int Sample::end(char endc) {
  char c = mygetc();
  if (c == ',') {
    return 0;
  } else {
    if (c != endc)
      cerr << "|"<<c<<"|" << " != " << "|"<<endc<<"|" << endl;
    assert(c == endc);
    return 1;
  }
}
void Sample::expect(char c) {
  char got = mygetc();
  if (got != c)
    cerr << "|"<<got<<"|" << " != " << "|"<<c<<"|" << endl;
  assert(got == c);
}
string Sample::getQuote() {
  expect('\"');
  char str[200];
  assert(fscanf(fp, "%[^\"]", str));
  expect('\"');
  expect(':');
  return string(str);
}
vector<int> Sample::readRow() {
  vector<int> ret;
  expect('[');
  while (1) {
    int v;
    assert(fscanf(fp, "%d", &v) == 1);
    ret.push_back(v);
    if (end(']')) break;
  }
  return ret;
}
Image Sample::readImage() {
  Image ret;
  ret.p = {0,0};
  expect('[');
  vector<int> widths;
  while (1) {
    vector<int> row = readRow();
    widths.push_back(row.size());
    for (int col : row)
      ret.mask.push_back(col);
    if (end(']')) break;
  }
  ret.h = widths.size();
  assert(ret.h >= 1 && ret.h <= 30);
  ret.w = widths[0];
  for (int i = 0; i < ret.h; i++)
    assert(widths[i] == ret.w);
  assert(ret.w >= 1 && ret.w <= 30);
  return ret;
}


vector<Sample> readAll(string path, int maxn) { //maxn = -1
  const string base_path[2] = {"/kaggle/input/abstraction-and-reasoning-challenge/", "./dataset/"};

  int base_pathi = 0;
  while (!experimental::filesystem::exists(base_path[base_pathi]+path)) {
    base_pathi++;
    assert(base_pathi < 2);
  }

  vector<string> files;
  for (auto magic_file_type : experimental::filesystem::directory_iterator(base_path[base_pathi]+path)) {
    string name = magic_file_type.path().u8string();
    if (name.size() >= 5 && name.substr(name.size()-5,5) == ".json")
      files.push_back(name);
  }
  if (maxn >= 0) files.resize(maxn);
  vector<Sample> sample;
  for (string sid : files) {
    //sample.push_back(Sample(sid));
    for (Sample s : Sample(sid).split()) {
      //if (maxn < 0 || sample.size() < maxn)
      sample.push_back(s);
    }
  }
  return sample;
}


Writer::Writer(string filename) { //filename = "submission_part.csv"
  filename = filename;
  fp = fopen(filename.c_str(), "w");
  assert(fp);
  fprintf(fp, "output_id,output\n");
}

void Writer::operator()(const Sample& s, vector<Image> imgs) {
  //int cnt = seen[s.id]++;
  //assert(cnt == s.id_ind);
  fprintf(fp, "%s_%d,", s.id.c_str(), s.id_ind);
  //TODO: is empty output allowed? Looks like no!
  if (imgs.empty()) imgs = {dummyImg};
  assert(imgs.size() <= 3);
  int notfirst = 0;
  for (Image_ img : imgs) {
    assert(img.p == point({0,0}));
    assert(img.w >= 1 && img.w <= 30 && img.h >= 1 && img.h <= 30);
    if (notfirst++) fprintf(fp, " ");
    fprintf(fp, "|");
    for (int i = 0; i < img.h; i++) {
      for (int j = 0; j < img.w; j++) {
	int c = img(i,j);
	assert(c >= 0 && c <= 9);
	fprintf(fp, "%d", c);
      }
      fprintf(fp, "|");
    }
  }
  fprintf(fp, "\n");
}

Writer::~Writer() {
  fclose(fp);
}


void writeAnswersWithScores(const Sample&s, string fn, vector<Image> imgs, vector<double> scores) {
  FILE*fp = fopen(fn.c_str(), "w");
  assert(fp);
  fprintf(fp, "%s_%d\n", s.id.c_str(), s.id_ind);
  assert(imgs.size() == scores.size());
  if (imgs.empty()) imgs = {dummyImg}, scores = {-1};
  assert(imgs.size() <= 3);

  for (int i = 0; i < imgs.size(); i++) {
    Image_ img = imgs[i];
    double score = scores[i];
    assert(img.p == point({0,0}));
    assert(img.w >= 1 && img.w <= 30 && img.h >= 1 && img.h <= 30);
    fprintf(fp, "|");
    for (int i = 0; i < img.h; i++) {
      for (int j = 0; j < img.w; j++) {
	int c = img(i,j);
	assert(c >= 0 && c <= 9);
	fprintf(fp, "%d", c);
      }
      fprintf(fp, "|");
    }
    fprintf(fp, " %.20f", score);
    fprintf(fp, "\n");
  }
  fclose(fp);
}
#include "precompiled_stl.hpp"

using namespace std;

#include "utils.hpp"
#include "core_functions.hpp"
#include "image_functions.hpp"
#include "visu.hpp"
#include "read.hpp"
#include "normalize.hpp"
#include "tasks.hpp"
#include "evals.hpp"

#include "brute2.hpp"

#include "score.hpp"
#include "load.hpp"

#include "deduce_op.hpp"
#include "pieces.hpp"
#include "compose2.hpp"

#include "brute_size.hpp"

#include <thread>

string green(string s) {
  return ("\033[1;32m"+s+"\033[0m");
}
string blue(string s) {
  return ("\033[1;34m"+s+"\033[0m");
}
string yellow(string s) {
  return ("\033[1;33m"+s+"\033[0m");
}
string red(string s) {
  return ("\033[1;31m"+s+"\033[0m");
}


void writeVerdict(int si, string sid, int verdict) {
  printf("Task #%2d (%s): ", si, sid.c_str());
  switch (verdict) {
  case 3: cout << green("Correct") << endl; break;
  case 2: cout << yellow("Candidate") << endl; break;
  case 1: cout << blue("Dimensions") << endl; break;
  case 0: cout << red("Nothing") << endl; break;
  default: assert(0);
  }
}


int MAXDEPTH = -1; //Argument

int MAXSIDE = 100, MAXAREA = 40*40, MAXPIXELS = 40*40*5; //Just default values

int print_times = 1, print_mem = 1, print_nodes = 1;

void run(int only_sid = -1, int arg = -1) {
  //rankFeatures();
  //evalNormalizeRigid();
  //evalTasks();
  //bruteSubmission();
  //bruteSolve();
  //evalEvals(1);
  //deduceEvals();

  int no_norm   = (arg >= 10 && arg < 20);
  int add_flips = (arg >= 20 && arg < 40);
  int add_flip_id = (arg >= 30 && arg < 40 ? 7 : 6);

  if (arg == -1) arg = 2;
  MAXDEPTH = arg % 10 * 10;

  int eval = 0;

  int skips = 0;

  string sample_dir = "evaluation";
  int samples = -1;
  if (eval) {
    sample_dir = "test";
    samples = -1;
  }

  /*vector<Sample> sample = readAll("evaluation", -1);
  samples = sample.size();
  sample = vector<Sample>(sample.begin()+samples-100,sample.end());*/
  vector<Sample> sample = readAll(sample_dir, samples);
  //sample = vector<Sample>(sample.begin()+200, sample.begin()+300);

  int scores[4] = {};

  Visu visu;

  vector<int> verdict(sample.size());

  int dones = 0;
  Loader load(sample.size());


  assert(only_sid < sample.size());
  //Remember to fix Timers before running parallel

  for (int si = 0; si < sample.size(); si++) {
    if (only_sid != -1 && si != only_sid) continue;

    //if (si == 30) assert(0);

    if (eval) load();
    else if (++dones % 10 == 0) {
      cout << dones << " / " << sample.size() << endl;
    }

    const Sample&s = sample[si];

    //Normalize sample
    Simplifier sim = normalizeCols(s.train);
    if (no_norm) sim = normalizeDummy(s.train);

    vector<pair<Image,Image>> train;
    for (auto&[in,out] : s.train) {
      train.push_back(sim(in,out));
    }
    //auto base_train = train;
    if (add_flips) {
      for (auto&[in,out] : s.train) {
	auto [rin,rout] = sim(in,out);
	train.push_back({rigid(rin,add_flip_id),rigid(rout,add_flip_id)});
      }
    }
    auto [test_in,test_out] = sim(s.test_in, s.test_out);

    {
      int insumsz = 0, outsumsz = 0, macols = 0;
      int maxside = 0, maxarea = 0;
      for (auto&[in,out] : s.train) {
	maxside = max({maxside, in.w, in.h, out.w, out.h});
	maxarea = max({maxarea, in.w*in.h, out.w*out.h});
	insumsz += in.w*in.h;
	outsumsz += out.w*out.h;
	macols = max(macols, __builtin_popcount(core::colMask(in)));
      }
      int sumsz = max(insumsz, outsumsz);
      cerr << "Features: " << insumsz << ' ' << outsumsz << ' ' << macols << endl;

      double w[4] = {1.2772523019346949, 0.00655104, 0.70820414, 0.00194519};
      double expect_time3 = w[0]+w[1]*sumsz+w[2]*macols+w[1]*w[2]*sumsz*macols;
      //MAXDEPTH = 2;//(expect_time3 < 30 ? 4 : 3);//sumsz < 20*20*3 ? 3 : 2;
      cerr << "MAXDEPTH: " << MAXDEPTH << endl;


      MAXSIDE = 100;
      MAXAREA = maxarea*2;
      MAXPIXELS = MAXAREA*5;
    }
    //#warning Only 1 training example
    //train.resize(1);

    vector<point> out_sizes = bruteSize(test_in, train);
    /*if (add_flips) {
      point predsz = out_sizes.back();
      out_sizes.clear();
      for (auto [in,out] : train)
	out_sizes.push_back(out.sz);
      out_sizes.push_back(predsz);
      assert(out_sizes.size() == train.size()+1);
      }*/
    //vector<point> out_sizes = cheatSize(test_out, train); assert(!eval);

    /*verdict[si] = (out_sizes.back() == test_out.sz ? 3 : 0);
    scores[verdict[si]]++;
    writeVerdict(si, s.id, verdict[si]);
    continue;*/
    //Generate candidate pieces
    Pieces pieces;
    {
      double start_time = now();
      vector<DAG> dags = brutePieces2(test_in, train, out_sizes);

      if (print_times) cout << "brutePieces time: " << now()-start_time << endl;
      start_time = now();
      pieces = makePieces2(dags, train, out_sizes);
      if (print_times) cout << "makePieces time: " << now()-start_time << endl;
    }

    if (print_mem) {
      double size = 0, child = 0, other = 0, inds = 0, maps = 0;
      for (DAG&d : pieces.dag) {
	/*for (Node&n : d.node) {
	  for (Image_ img : n.state.vimg) {
	    size += img.mask.size();
	  }
	  child += n.child.size()*8;
	  other += sizeof(Node);
	  }*/
	other += sizeof(TinyNode)*d.tiny_node.size();
	size += 4*d.tiny_node.bank.mem.size();
	for (TinyNode&n : d.tiny_node.node) {
	  if (n.child.sz < TinyChildren::dense_thres)
	    child += n.child.cap*8;
	  else
	    child += n.child.cap*4;
	}
	maps += 16*d.hashi.data.size()+4*d.hashi.table.size();
	//maps += (d.hashi.bucket_count()*32+d.hashi.size()*16);
      }
      for (Piece3&p : pieces.piece) {
	inds += sizeof(p);
      }
      inds += sizeof(pieces.mem[0])*pieces.mem.size();
      printf("Memory: %.1f + %.1f + %.1f + %.1f + %.1f MB\n", size/1e6, child/1e6, other/1e6, maps/1e6, inds/1e6);
      //break;
    }

    for (DAG&d : pieces.dag) {
      d.hashi.clear();
      for (TinyNode&n : d.tiny_node.node) {
	n.child.clear();
      }
    }

    int s1 = 0;
    if (!eval) s1 = (out_sizes.back() == test_out.sz);

    //Assemble pieces into candidates
    vector<Candidate> cands;
    {
      double start_time = now();
      cands = composePieces2(pieces, train, out_sizes);
      if (print_times) cout << "composePieces time: " << now()-start_time << endl;
    }
    addDeduceOuterProduct(pieces, train, cands);

    cands = evaluateCands(cands, train);

    int s2 = 0;
    if (!eval) s2 = scoreCands(cands, test_in, test_out);


    //Pick best candidates
    vector<Candidate> answers = cands;

    {
      sort(cands.begin(), cands.end());
      vector<Candidate> filtered;
      set<ull> seen;
      for (const Candidate&cand : cands) {

	//printf("%.20f\n", cand.score);

	ull h = hashImage(cand.imgs.back());
	if (seen.insert(h).second) {
	  filtered.push_back(cand);
	  if (filtered.size() == 3+skips*3) break;
	}
      }
      for (int i = 0; i < skips*3 && filtered.size(); i++)
	filtered.erase(filtered.begin());
      answers = move(filtered);
    }

    //Reconstruct answers
    vector<Image> rec_answers;
    vector<double> answer_scores;
    for (Candidate&cand : answers) {
      rec_answers.push_back(sim.rec(s.test_in, cand.imgs.back()));
      double score = cand.score;
      if (add_flips) {
	score /= 2-1e-5;
      }
      answer_scores.push_back(score);
    }

    int s3 = 0;
    if (!eval) s3 = scoreAnswers(rec_answers, s.test_in, s.test_out);

    if (!eval) {//!eval && s1 && !s2) {
      {
	visu.next(to_string(si) + " - test");
	for (auto&[in,out] : train) visu.add(in,out);
	visu.next(to_string(si) + " - test");
	visu.add(test_in,test_out);
	visu.next(to_string(si) + " - cands");
	for (int i = 0; i < min((int)answers.size(), 5); i++) {
	  visu.add(test_in, answers[i].imgs.back());
	}
      }
    }

    /*if (!eval && (s2 || s3) && !s1) {
      cout << si << endl;
      }*/

    if (!eval) {
      if (s3) verdict[si] = 3;
      else if (s2) verdict[si] = 2;
      else if (s1) verdict[si] = 1;
      else verdict[si] = 0;
      scores[verdict[si]]++;

      writeVerdict(si, s.id, verdict[si]);
    }
    {
      string fn = "output/answer_"+to_string(only_sid)+"_"+to_string(arg)+".csv";
      //Writer writer(fn);
      //writer(s, rec_answers);
      writeAnswersWithScores(s, fn, rec_answers, answer_scores);
    }
  }


  //auto now = []() {return chrono::steady_clock::now().time_since_epoch().count()/1e9;};
  //for (double start = now(); now() < start+10;);

  if (!eval && only_sid == -1) {
    for (int si = 0; si < sample.size(); si++) {
      Sample&s = sample[si];
      writeVerdict(si, s.id, verdict[si]);
    }
    for (int si = 0; si < sample.size(); si++) cout << verdict[si];
    cout << endl;

    for (int i = 3; i; i--) scores[i-1] += scores[i];
    printf("\n");
    printf("Total: % 4d\n", scores[0]);
    printf("Pieces:% 4d\n", scores[1]);
    printf("Cands: % 4d\n", scores[2]);
    printf("Correct:% 3d\n", scores[3]);
  }
}
#include "precompiled_stl.hpp"
using namespace std;
#include "utils.hpp"
#include "core_functions.hpp"
#include "image_functions.hpp"

#include "brute2.hpp"
#include "pieces.hpp"
#include "compose2.hpp"

int scoreCands(const vector<Candidate>&cands, Image_ test_in, Image_ test_out) {
  for (const Candidate&cand : cands)
    if (cand.imgs.back() == test_out) return 1;
  return 0;
}

int scoreAnswers(vImage_ answers, Image_ test_in, Image_ test_out) {
  assert(answers.size() <= 3);
  for (Image_ answer : answers)
    if (answer.sz == test_out.sz && answer.mask == test_out.mask) return 1;
  return 0;
}
import json

def read(fn):
    f = open(fn)
    t = f.read()
    f.close()
    return t

t = read('submission_part.csv')

total, correct = 0, 0
for line in t.strip().split('\n')[1:]:
    id, imgs = line.split(',')
    hash, test_ind = id.split('_')
    fn = 'dataset/evaluation/'+hash+'.json'
    with open(fn) as f:
        truth = json.load(f)['test'][int(test_ind)]['output']
        truth = '|'+'|'.join(''.join(str(j) for j in line) for line in truth)+'|'
        total += 1
        correct += truth in imgs.split()

print(correct, '/', total)
#include "precompiled_stl.hpp"
using namespace std;

#include "utils.hpp"
#include "core_functions.hpp"
#include "image_functions.hpp"
#include "image_functions2.hpp"
#include "visu.hpp"
#include "read.hpp"
#include "normalize.hpp"
#include "spec.hpp"

Image solveTask(const Image& img, const vector<pair<Image,Image>>&train, int taski) {
  if (taski == 0) {
    return toOrigin(compress(filterCol(img, 1)));
  } else if (taski == 1) {
    Image cols = cutPickMax(img, 4);
    Image shape = cutPickMax(img, 0);
    return toOrigin(colShape(cols, shape));
  } else if (taski == 2 || taski == 3) {
    return interior(filterCol(img, 2));
  } else if (taski == 4) {
    return outerProductIS(img, count(img, 1, 0));
  } else if (taski == 5) {
    return train[0].second;
  } else if (taski == 6) {
    Image cutter = filterCol(train[0].second, 1);
    Image yellow = compose(cutter, filterCol(img, 4));
    Image part = cutPickMax(yellow, 0);
    Image mask = hull(part);
    Image chosen = toOrigin(compose(img, mask, 2));
    Image positioned = outerProductSI(toOrigin(part), myStack(chosen, Square(1), 2));
    return compose(positioned, cutter, 1);
  } else if (taski == 7) {
    Image blue = filterCol(img, 1);
    Image red = compress(filterCol(img, 2));
    return compose(blue, Move(red, Pos(2,0)), 0);
  } else if (taski == 8) {
    return rigid(img, 2);
  } else if (taski == 9 || taski == 10) {
    Image a = compress(filterCol(img, 2));
    Image b = toOrigin(compress(filterCol(img, 3)));
    return colShape(compose(a, b), 1);
  } else if (taski == 11) {
    return train[0].second;
  } else if (taski == 12) {
    return compose(Square(3), splitPickMax(img, 0));
  } else if (taski == 13) {
    return train[0].second;

  } else if (taski == 15) {
    return toOrigin(compress(cutIndex(img, 0)));
  } else if (taski == 16) {
    Image good = filterCol(img, train[0].second);
    Image a = compose(good, rigid(good, 6));
    Image b = compose(a, Move(rigid(a, 2), Pos(2,2)));
    return embed(b, img);
  } else if (taski == 17) {
    Image a = filterCol(img, 2);
    Image b = filterCol(img, 3);
    return compose(a, colShape(b, 1));
  } else if (taski == 18) {
    Image cross = filterCol(rigid(img,4), 1);
    Image sq = count(filterCol(img, 2), 0, 0);
    return rigid(extend(compress(myStack(invert(sq), cross, 2)), img), 4);
  } else if (taski == 19) {
    Image a = cutIndex(img, 0);
    Image b = cutIndex(img, 1);
    return compose(colShape(compose(hull(compress(a)), hull(compress(b))), 1), img);

  } else if (taski == 21) {
    return rigid(img, 6);
  } else if (taski == 22) {
    Image shape = compress(cutPickMax(img, 8));
    Image points = filterCol(img, 1);
    Image a = cutIndex(points, 0);
    Image b = cutIndex(points, 1);
    Image c = cutIndex(points, 2);
    Image d = cutIndex(points, 3);
    Image ans = img;
    ans = compose(ans, replaceCols(align(outerProductIS(shape, a), a), img));
    ans = compose(ans, replaceCols(align(outerProductIS(shape, b), b), img));
    ans = compose(ans, replaceCols(align(outerProductIS(shape, c), c), img));
    ans = compose(ans, replaceCols(align(outerProductIS(shape, d), d), img));
    return ans;
  } else if (taski == 23) {
    Image a = smear(filterCol(img,2), 6);
    Image b = smear(filterCol(img,3), 6);
    return compose(compose(a, b), colShape(compose(a, b, 2), 1));

  } else if (taski == 25) {
    return embed(Move(toOrigin(compress(train[0].second)), compress(filterCol(img,1))), img);
  } else if (taski == 26) {
    Image dup = colShape(toOrigin(cutPickMax(img, 0)), 1);
    return compose(outerProductSI(Square(3), myStack(dup, Col(1), 2)), img, 1);
  } else if (taski == 27) {
    return broadcast(img, Square(3));

  } else if (taski == 29) {
    return compose(img, invert(interior2(img)), 2);

  } else if (taski == 31) {
    Image a = cutIndex(img, 0);
    Image b = cutIndex(img, 1);
    Image c = cutIndex(img, 2);
    Image d = cutIndex(img, 3);
    Image ans = compose(invert(Square(3)), Pos(1,1));
    ans = compose(ans, align(a, ans));
    ans = compose(ans, align(b, ans));
    ans = compose(ans, align(c, ans));
    ans = compose(ans, align(d, ans));
    return ans;
  } else if (taski == 32) {
    return toOrigin(compress(filterCol(cutPickMax(img, 4), 1)));
  } else if (taski == 33) {
    Image a = toOrigin(compress(filterCol(img, 3)));
    Image b = toOrigin(compress(filterCol(img, 4)));
    Image both = compose(a, b, 2);
    Image one = compose(a, b);
    return colShape(compose(one, invert(both), 2), 1);
  } else if (taski == 34) {
    return count(img, 0, 1);
  } else if (taski == 35) {
    Image a = smear(filterCol(img, 2), 5);
    Image b = smear(filterCol(img, 3), 4);
    return compose(compose(a, b), colShape(compose(a,b,2), 1));
  } else if (taski == 36) {
    Image a = toOrigin(compress(cutPickMax(img, 0)));
    Image b = toOrigin(compress(cutPickMax(img, 1)));
    return colShape(invert(compose(a, b)), 1);

  } else if (taski == 38) {
    Image gray = colShape(filterCol(img, 5), 1);
    return compose(img, gray);
  } else if (taski == 39) {
    return broadcast(rigid(img,4), Square(4));
  } else if (taski == 40) {
    Image a = smear(filterCol(img, 1), 4);
    Image b = filterCol(img, 2);
    return compose(b, a);
  } else if (taski == 41) {
    Image cutter = filterCol(img, 3);
    Image ret = colShape(getSize(img), 2);
    ret = compose(ret, colShape(cutPickMaxes(invert(img), 3), 1));
    ret = compose(ret, cutter);
    return ret;
  } else if (taski == 42) {
    return compose(train[0].second, compose(img, rigid(img, 6)));
  } else if (taski == 43) {
    Image a = smear(filterCol(img, 1), 4);
    Image b = smear(filterCol(img, 3), 4);
    Image c = smear(filterCol(img, 2), 5);
    return compose(c, compose(a, b));

  } else if (taski == 47) {
    Image fill = colShape(invert(compress(filterCol(img, 2))), 1);
    return compose(smear(fill, invert(img), 6), img);

  } else if (taski == 49) {
    Image base = colShape(cutPickMax(img, 4), 1);
    base = myStack(base, rigid(base, 4), 0);
    base = myStack(base, rigid(base, 5), 1);
    return base;

  } else if (taski == 51) {
    return rigid(compose(rigid(train[0].second, 5), getSize(img), 2), 5);

  } else if (taski == 52) {
    return embed(toOrigin(compress(img)), Square(3));
  } else if (taski == 53) {
    Image takes = splitPickMax(img, 0);
    return toOrigin(cutIndex(takes, 0));
  } else if (taski == 54) {
    Image a = compress(cutIndex(img,0));
    Image b = compress(cutIndex(img,1));
    Image ca = align(Col(1), a, 2, 2);
    Image cb = align(Col(1), b, 2, 2);
    ca = smear(ca, getSize(img), 6);
    cb = smear(cb, getSize(img), 6);
    return compose(compose(compose(compose(img, ca), cb), a), b);
  } else if (taski == 55) {
    Image twos = cutPickMaxes(img, 0);
    return colShape(broadcast(twos, Square(3), 0), 1);
  } else if (taski == 56) {
    Image base = compress(train[0].second);
    Image pos = compress(img);
    return embed(align(base, pos, 2, 2), img);

  } else if (taski == 58) {
    Image blues = count(img, 0, 1);
    Image blacks = count(invert(img), 0, 0);
    blues = outerProductSI(blues, img);
    blacks = outerProductSI(blacks, img);
    return wrap(blues, blacks);
  } else if (taski == 59) {
    return toOrigin(compress(filterCol(img, 1)));
  } else if (taski == 60) {
    return compose(img, rigid(img, 5));

  } else if (taski == 62) {
    Image reds = cutPickMaxes(img, 11);
    return compose(colShape(img, 1), reds);
  } else if (taski == 63) {
    Image blues = colShape(cutPickMaxes(img, 0), 1);
    Image greens = colShape(cutPickMaxes(img, 1), 3);
    return compose(compose(colShape(img, 2), blues), greens);
  } else if (taski == 64) {
    return colShape(filterCol(img, 1), 2);

  } else if (taski == 66) {
    return broadcast(compress(img), Square(3)); //Cheating
  } else if (taski == 67) {
    return compose(smear(filterCol(img, 2), 3), filterCol(img, 1)); //Needs good normalize_rigid

  } else if (taski == 69) {
    Image a = invert(toOrigin(compress(filterCol(img, 2))));
    Image b = invert(toOrigin(compress(filterCol(img, 3))));
    return colShape(compose(a,b,2), 1);
  } else if (taski == 70) {
    Image shape = compress(filterCol(train[0].second, 1));
    Image a = cutIndex(img, 0);
    Image b = cutIndex(img, 1);
    Image c = cutIndex(img, 2);
    a = replaceCols(align(shape, a, 2, 5), img);
    b = replaceCols(align(shape, b, 2, 5), img);
    c = replaceCols(align(shape, c, 2, 5), img);
    return embed(compose(compose(a,b), c), img);
  } else if (taski == 71) {
    Image shape = hull(compress(img));
    shape = compose(shape, colShape(interior2(shape), 2));
    return embed(shape, img);
  } else if (taski == 72) {
    return myStack(img, rigid(img,5), 1);
  } else if (taski == 73) {
    Image base = broadcast(compress(img), Square(3));
    return outerProductIS(base, base);

  } else if (taski == 77) {
    return myStack(img, rigid(img,5), 1);
  } else if (taski == 78) {
    Image a = compose(img, rigid(img, 6));
    Image b = compose(a, Move(rigid(a, 2), Pos(2,2)));
    return embed(b, img);
  } else if (taski == 79) {
    Image ship = filterCol(img, 2);
    Image cover = smear(ship, 2);
    return compose(compose(smear(filterCol(img, 1), 5), cover, 2), img);
  } else if (taski == 82) {
    Image mask = compress(interior(smear(filterCol(img, 2), 6)));
    return toOrigin(colShape(embed(img,mask), 2));
  } else if (taski == 83) {
    Image ret = myStack(img,rigid(img,3), 0);
    return myStack(ret,rigid(ret,2),1);
  } else if (taski == 84) {
    return embed(count(img, 0, 1), Square(3));
  } else if (taski == 85) {
    return train.back().second;
  } else if (taski == 86) {
    Image a = cutIndex(img,0);
    Image b = cutIndex(img,1);
    Image c = cutIndex(img,2);
    a = align(a,a,5,0);
    b = align(b,b,5,0);
    c = align(c,c,5,0);
    return embed(compose(compose(a,b),c), img);
  } else if (taski == 88) {
    Image cutter = filterCol(img, 3);
    Image mi = colShape(cutPickMaxes(invert(img),1), 2);
    Image ma = colShape(cutPickMaxes(invert(img),0), 1);
    return compose(compose(mi, ma), cutter);
  } else if (taski == 89) {
    Image dots = filterCol(img, 3);
    Image lines = smear(dots, 1);
    Image red = filterCol(img, 2);
    Image top = cutIndex(red,0);
    Image bot = cutIndex(red,1);

    Image overlap = compose(lines,top,2);
    overlap = align(overlap, bot, 1, 1);
    Image lines2 = smear(overlap,getSize(img),1);
    dots = colShape(dots, 1);
    return compose(compose(compose(lines, lines2), red), dots);
  } else if (taski == 90) {
    Image a = cutIndex(img,0);
    Image b = cutIndex(img,1);
    Image c = cutIndex(img,2);
    Image d = cutIndex(img,3);
    Image whole = getSize(img);
    Image blue = img;
    blue = compose(smear(a,whole,2), blue);
    blue = compose(smear(b,whole,2), blue);
    blue = compose(smear(c,whole,2), blue);
    blue = compose(smear(d,whole,2), blue);
    blue = colShape(blue, 1);
    Image red = img;
    red = compose(align(outerProductIS(a,Square(2)),a,2,2), red);
    red = compose(align(outerProductIS(b,Square(2)),b,2,2), red);
    red = compose(align(outerProductIS(c,Square(2)),c,2,2), red);
    red = compose(align(outerProductIS(d,Square(2)),d,2,2), red);
    red = colShape(red, 2);
    return compose(compose(blue,red),img);
    /*} else if (taski == 88) {
    //Needs better align
    Image cutter = filterCol(img,Col(1));
    Image ret = cutPickMax(img,cutter,0);
    Image a = cutIndex(img,cutter,0);
    Image b = cutIndex(img,cutter,1);
    Image c = cutIndex(img,cutter,2);
    Image d = cutIndex(img,cutter,3);
    ret = compose(ret, align(a,ret), 3);
    ret = compose(ret, align(b,ret), 3);
    ret = compose(ret, align(c,ret), 3);
    ret = compose(ret, align(d,ret), 3);
    return toOrigin(ret);*/
  } else if (taski == 93) {
    Image blue = compress(filterCol(img,1));
    return embed(compose(align(colShape(Square(3), 3), blue, 2, 2), blue), img);
  } else if (taski == 95) {
    Image cols = smear(splitPickMaxes(img, 1), 2);
    return replaceCols(img, cols);
  } else if (taski == 96) {
    Image base = compress(img);
    return toOrigin(outerProductIS(base,base));
  } else if (taski == 97) {
    return compose(img, colShape(interior2(img), 1));
  } else if (taski == 101) {
    return colShape(broadcast(compress(filterCol(img,2)), Square(3)), 1);
  }
  return img;
}





void evalTasks() {
  vector<Sample> sample = readAll("training", 100);//evaluation
  Visu visu;

  int corrects = 0;
  int place_count[11] = {};
  //#pragma omp parallel for
  for (int si = 0; si < sample.size(); si++) {
    Sample&s = sample[si];
    {
      vector<Simplifier> sims;
      remapCols(s.train, sims);
      for (auto&[in,out] : s.train)
	tie(in,out) = make_pair(sims[0].in(in), sims[0].out(in,out));
      tie(s.test_in,s.test_out) = make_pair(sims[0].in(s.test_in), sims[0].out(s.test_in,s.test_out));
      s.test = {{s.test_in, s.test_out}};
    }
    /*{
      vector<Simplifier> sims;
      normalizeRigid(s.train, sims);
      for (auto&[in,out] : s.train)
	tie(in,out) = make_pair(sims[0].in(in), sims[0].out(in,out));
      for (auto&[in,out] : s.test)
	tie(in,out) = make_pair(sims[0].in(in), sims[0].out(in,out));
	}*/

    /*visu.next(s.id);
    for (auto [in,out] : s.train) {
      visu.add(in,out);
      }*/

    Image pred = solveTask(s.test[0].first, s.train, si);
    cout << "Task " << si << ": " << (pred == s.test[0].second ? "OK" : "Failed") << endl;
    corrects += (pred == s.test[0].second);
    if (pred != s.test_out) {// && pred != s.test_in) {
      visu.next(to_string(si));//s.id);
      visu.add(s.test_in, pred);
      visu.add(s.test_in, s.test_out);
      for (auto [in,out] : s.train)
	visu.add(in, out);
    }
  }
  cout << corrects << " / " << sample.size() << endl;
  exit(0);
}
/*#include "stdio.h"
#include <vector>
#include <string>
#include <tuple>
#include <cassert>
#include <functional>*/
#include "precompiled_stl.hpp"
using namespace std;
#include "utils.hpp"
#include "visu.hpp"

Visu::Visu() {
  fp = fopen("visu.txt", "w");
}
Visu::~Visu() {
  fclose(fp);
}
void Visu::next(string s) {
  fprintf(fp, "Task %s\n", s.c_str());
}
void Visu::add(Image in, Image out) {
  fprintf(fp, "Pair\n");
  for (Image_ img : {in,out}) {
    fprintf(fp, "Image %d %d\n", img.w, img.h);
    for (int i = 0; i < img.h; i++) {
      for (int j = 0; j < img.w; j++) {
	int col = img(i,j);
	fprintf(fp, "%d", col);
      }
      fprintf(fp, "\n");
    }
  }
}


void plot(const vector<vector<int>>&inp, const char*filename) { //filename = out.ppm
  int h = inp.size();
  int w = inp[0].size();

  int tw = 512/max(w,h);
  int bw = max(1, 10/max(w,h));

  int W = (tw+bw)*w+bw, H = (tw+bw)*h+bw;
  FILE*fp = fopen(filename, "w");
  fprintf(fp, "P6\n%d %d\n255\n", W, H);
  vector<int> cols = {0x000000, 0x0074D9, 0xFF4136, 0x2ECC40, 0xFFDC00, 0xAAAAAA, 0xF012BE, 0xFF851B, 0x7FDBFF, 0x870C25};

  vector<unsigned char> output(W*H*3, 0x60);
  for (int i = 0; i < h; i++) {
    for (int j = 0; j < w; j++) {
      for (int k = 0; k < tw; k++) {
	for (int l = 0; l < tw; l++) {
	  for (int c = 0; c < 3; c++) {
	    output[((i*(tw+bw)+bw+k)*W+
		    (j*(tw+bw)+bw+l))*3 + c] = cols[inp[i][j]] >> (2-c)*8 & 255;
	  }
	}
      }
    }
  }
  fwrite(output.data(), 1, W*H*3, fp);
  fclose(fp);
}

void print(Image img) {
  printf("[%d %d %d %d]\n", img.p.x, img.p.y, img.w, img.h);
  for (int i = 0; i < img.h; i++) {
    for (int j = 0; j < img.w; j++) {
      int col = img(i,j);
      if (col)
	printf("%d", col);
      else printf(".");
    }
    printf("\n");
  }
}
