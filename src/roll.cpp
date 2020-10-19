// SEXP roll_mean_impl(SEXP x, int n, String align, bool na_rm) {
//   roll_vector_with(mean)
//
//     if (na_rm) {
//       return RcppRoll::roll_vector_with(
//         RcppRoll::mean_f<true>(), NumericVector(x), n, weights, by, fill, partial, align, normalize);
//     } else {
//       return RcppRoll::roll_vector_with(
//         RcppRoll::mean_f<false>(), NumericVector(x), n, weights, by, fill, partial, align, normalize);
//     }
// }
//
// struct mean_f;
//
// template <>
// struct mean_f<true> {
//   inline double operator()(NumericVector const& x,  int n) {
//     double result = 0.0;
//     int num = 0;
//     for (int i = 0; i < n; ++i) {
//       if (!ISNAN(x[i])) {
//         result += x[i];
//         ++num;
//       }
//     }
//     return result / num;
//   }
//
// };
//
// template <typename Callable, typename T>
// T roll_matrix_with(Callable f,
//                    T const& x,
//                    int n) {
//
//   int nrow = x.nrow();
//   int ncol = x.ncol();
//
//   T output;
//   if (fill.filled()) {
//     output = T(nrow, ncol);
//   } else {
//     output = T(nrow - n + 1, ncol);
//   }
//
//   for (int i = 0; i < ncol; ++i) {
//     output(_, i) = roll_vector_with(
//       f, static_cast<NumericVector>(x(_, i)), n, weights, by,
//       fill, partial, align, normalize);
//   }
//
//   return output;
// }
