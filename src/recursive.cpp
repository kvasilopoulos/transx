#include <Rcpp.h>
#include <numeric>   	// for std::partial_sum
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector rec_mean(NumericVector x){
  double acc = 0;
  NumericVector res(x.size());

  for(int i = 0; i < x.size(); i++){
    acc += x[i];
    res[i] = acc/i;
  }
  return res;
}


