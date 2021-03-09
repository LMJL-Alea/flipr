#include <Rcpp.h>

// [[Rcpp::export]]
double stat_student_impl(
    const Rcpp::NumericVector &distanceMatrix,
    const Rcpp::IntegerVector &firstGroupIndices,
    const Rcpp::IntegerVector &secondGroupIndices
);

// [[Rcpp::export]]
double stat_fisher_impl(
    const Rcpp::NumericVector &distanceMatrix,
    const Rcpp::IntegerVector &firstGroupIndices,
    const Rcpp::IntegerVector &secondGroupIndices
);

// [[Rcpp::export]]
double stat_bg_impl(
    const Rcpp::NumericVector &distanceMatrix,
    const Rcpp::IntegerVector &firstGroupIndices,
    const Rcpp::IntegerVector &secondGroupIndices
);

// [[Rcpp::export]]
double stat_energy_impl(
    const Rcpp::NumericVector &distanceMatrix,
    const Rcpp::IntegerVector &firstGroupIndices,
    const Rcpp::IntegerVector &secondGroupIndices,
    const unsigned int alphaValue = 1
);

// [[Rcpp::export]]
double stat_cq_impl(
    const Rcpp::NumericVector &similarityMatrix,
    const Rcpp::IntegerVector &firstGroupIndices,
    const Rcpp::IntegerVector &secondGroupIndices
);
