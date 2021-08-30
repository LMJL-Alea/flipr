#include <Rcpp.h>

// [[Rcpp::export]]
double getElement(
    const Rcpp::NumericVector &distObject,
    const unsigned int rowIndex,
    const unsigned int colIndex
);
