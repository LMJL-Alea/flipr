#include <Rcpp.h>

double getElement(
    const Rcpp::NumericVector &distObject,
    const unsigned int rowIndex,
    const unsigned int colIndex
);
