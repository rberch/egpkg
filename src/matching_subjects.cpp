#include <Rcpp.h>
using namespace Rcpp;

//' Match subjects Rcpp
//' @return A list with two vectors (`match_id`, `match_x`).
//' @param x NumericVector. This is the input
//' @examples
//' # Testing the function
//' library(Rcpp)
//' x = c(1,2,3,4,5,6)
//' matching_subjects(x)
//' @export
// [[Rcpp::export]]
List matching_subjects(const NumericVector & x) {

  int n = static_cast<int>(x.size());

  IntegerVector indices(n);
  NumericVector values(n);
  values.fill(std::numeric_limits< double >::max());

  for (int i = 0; i < n; ++i) {

    // Instead of allocating new memory, we can point by reference
    // (saves operations)
    double & cur_best = values[i];
    auto & cur_i    = indices[i];

    for (int j = 0; j < i; ++j) {

      // If it is lower, then update
      double d = std::abs(x[i] - x[j]);
      if (d < cur_best) {

        cur_best = d;
        cur_i    = j;

      }

      if (d < values[j]) {

        values[j] = d;
        indices[j] = i;

      }

    }

  }

  for (int i = 0; i < n; ++i)
    values[i] = x[indices[i]];

  return List::create(
    _["match_id"] = indices + 1, // We add one to match R's indices
    _["match_x"]  = values
  );
}
