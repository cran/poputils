#include "cpp11.hpp"
#include "cpp11/integers.hpp"

extern "C" {
#include <R_ext/Random.h>
}

using namespace cpp11;
using namespace std;
namespace writable = cpp11::writable;

// r-style modulo
inline int mod3(int x) { 
  int r = x % 3;
  return (r < 0) ? r + 3 : r;
}

// HAS_TESTS
[[cpp11::register]]
writable::integers rr3_inner(integers x, bool is_rvec) {
  int n = x.size();
  writable::integers out(n);
  GetRNGstate();
  for (int i = 0; i < n; ++i) {
    int xi = x[i];
    if (xi == NA_INTEGER) {
      out[i] = NA_INTEGER;
      continue;
    }
    int rem = mod3(xi);
    double p = unif_rand();
    if (rem == 1) {
      out[i] = (p < 2.0 / 3.0) ? xi - 1 : xi + 2;
    } else if (rem == 2) {
      out[i] = (p < 1.0 / 3.0) ? xi - 2 : xi + 1;
    } else {
      out[i] = xi; // rem == 0
    }
  }
  PutRNGstate();
  return out;
}
