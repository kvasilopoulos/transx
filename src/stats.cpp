// https://stackoverflow.com/questions/55212746/rcpp-fast-statistical-mode-function-with-vector-input-of-any-type

#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::plugins(cpp11)]]
#include <unordered_map>

// [[Rcpp::export]]
int fastIntMode(IntegerVector x, bool narm = false) {
  if (narm) x = x[!is_na(x)];
  int myMax = 1;
  int myMode = 0;
  std::unordered_map<int, int> modeMap;
  modeMap.reserve(x.size());

  for (std::size_t i = 0, len = x.size(); i < len; ++i) {
    auto it = modeMap.find(x[i]);

    if (it != modeMap.end()) {
      ++(it->second);
      if (it->second > myMax) {
        myMax = it->second;
        myMode = x[i];
      }
    } else {
      modeMap.insert({x[i], 1});
    }
  }

  return myMode;
}
