#include <Rcpp.h>
using namespace Rcpp;

NumericVector ret_simple(NumericVector x) {
  NumericVector vec_diff = diff(x);
  NumericVector res(x.size());

  // pad the front with an NA
  res[0] = NA_REAL;
  for(int i = 1; i < res.size(); i++) {
    res[i] = vec_diff[i-1] / x[i-1];
  }
  return res;
}
