// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;

// rec_mean
NumericVector rec_mean(NumericVector x);
RcppExport SEXP _transx_rec_mean(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(rec_mean(x));
    return rcpp_result_gen;
END_RCPP
}
// fastIntMode
int fastIntMode(IntegerVector x, bool narm);
RcppExport SEXP _transx_fastIntMode(SEXP xSEXP, SEXP narmSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< IntegerVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< bool >::type narm(narmSEXP);
    rcpp_result_gen = Rcpp::wrap(fastIntMode(x, narm));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_transx_rec_mean", (DL_FUNC) &_transx_rec_mean, 1},
    {"_transx_fastIntMode", (DL_FUNC) &_transx_fastIntMode, 2},
    {NULL, NULL, 0}
};

RcppExport void R_init_transx(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
