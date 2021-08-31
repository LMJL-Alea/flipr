#include "distUtils.h"

double getElement(const Rcpp::NumericVector &distObject,
                  const unsigned int rowIndex,
                  const unsigned int colIndex)
{
  if (rowIndex == colIndex) return 0.0;
  unsigned int i = rowIndex;
  unsigned int j = colIndex;
  if (i > j)
  {
    unsigned int k = i;
    i = j;
    j = k;
  }
  unsigned int numberOfDataPoints = distObject.attr("Size");
  unsigned int linearIndex = numberOfDataPoints * (i - 1) - i * (i - 1) / 2 + j - i - 1;
  return distObject(linearIndex);
}
