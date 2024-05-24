
#include "cpp11.hpp"
#include "cpp11/doubles.hpp"
#include "cpp11/matrix.hpp"
using namespace cpp11;
using namespace std;
namespace writable = cpp11::writable;


// HAS_TESTS
[[cpp11::register]]
writable::doubles logit_inner(doubles p) {
  int n = p.size();
  writable::doubles ans(n);
  for (int i = 0; i < n; i++) {
    double val = p[i];
    if (isnan(val))
      ans[i] = NA_REAL;
    else if (val > 1)
      stop("'p' has value greater than 1.");
    else if (val < 0)
      stop("'p' has value less than 0.");
    else
      ans[i] = log(val) - log1p(-val);
  }
  return ans;
}

// HAS_TESTS
[[cpp11::register]]
writable::doubles invlogit_inner(doubles x) {
  int n = x.size();
  writable::doubles ans(n);
  for (int i = 0; i < n; i++) {
    double val = x[i];
    if (isnan(val))
      ans[i] = NA_REAL;
    else if (val < 0)
      ans[i] = exp(val) / (1 + exp(val));
    else
      ans[i] = 1 / (1 + exp(-val));
  }
  return ans;
}

	
	
