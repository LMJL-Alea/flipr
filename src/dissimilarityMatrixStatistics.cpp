#include "dissimilarityMatrixStatistics.h"
#include "distUtils.h"

double stat_student_impl(const Rcpp::NumericVector &distanceMatrix,
                         const Rcpp::IntegerVector &firstGroupIndices,
                         const Rcpp::IntegerVector &secondGroupIndices)
{
  double xyMean = 0.0, xxMean = 0.0, yyMean = 0.0;
  unsigned int n1 =  firstGroupIndices.size();
  unsigned int n2 = secondGroupIndices.size();
  unsigned int counter_xy = 0, counter_xx = 0, counter_yy = 0;
  for (unsigned int i = 0;i < n1;++i)
  {
    for (unsigned int j = 0;j < n2;++j)
    {
      xyMean *= (counter_xy / (counter_xy + 1.0));
      double distanceValue = getElement(distanceMatrix, firstGroupIndices[i], secondGroupIndices[j]);
      xyMean += distanceValue * distanceValue / (counter_xy + 1.0);
      ++counter_xy;

      if (i == 0)
      {
        for (unsigned int k = j + 1;k < n2;++k)
        {
          yyMean *= (counter_yy / (counter_yy + 1.0));
          double distanceValue = getElement(distanceMatrix, secondGroupIndices[j], secondGroupIndices[k]);
          yyMean += distanceValue * distanceValue / (counter_yy + 1.0);
          ++counter_yy;
        }
      }
    }

    for (unsigned int j = i + 1;j < n1;++j)
    {
      xxMean *= (counter_xx / (counter_xx + 1.0));
      double distanceValue = getElement(distanceMatrix, firstGroupIndices[i], firstGroupIndices[j]);
      xxMean += distanceValue * distanceValue / (counter_xx + 1.0);
      ++counter_xx;
    }
  }

  double var_x = xxMean / 2.0;
  double var_y = yyMean / 2.0;
  double var_c = var_x / n1 + var_y / n2;

  double resVal = xyMean - var_x - var_y;

  if (var_c < std::sqrt(std::numeric_limits<double>::epsilon()))
    return resVal;

  return resVal / var_c;
}

double stat_fisher_impl(const Rcpp::NumericVector &distanceMatrix,
                        const Rcpp::IntegerVector &firstGroupIndices,
                        const Rcpp::IntegerVector &secondGroupIndices)
{
  double xxMean = 0.0, yyMean = 0.0;
  unsigned int n1 =  firstGroupIndices.size();
  unsigned int n2 = secondGroupIndices.size();
  unsigned int counter_xx = 0, counter_yy = 0;
  for (unsigned int i = 0;i < n1;++i)
  {
    for (unsigned int j = 0;j < n2;++j)
    {
      if (i == 0)
      {
        for (unsigned int k = j + 1;k < n2;++k)
        {
          yyMean *= (counter_yy / (counter_yy + 1.0));
          double distanceValue = getElement(distanceMatrix, secondGroupIndices[j], secondGroupIndices[k]);
          yyMean += distanceValue * distanceValue / (counter_yy + 1.0);
          ++counter_yy;
        }
      }
    }

    for (unsigned int j = i + 1;j < n1;++j)
    {
      xxMean *= (counter_xx / (counter_xx + 1.0));
      double distanceValue = getElement(distanceMatrix, firstGroupIndices[i], firstGroupIndices[j]);
      xxMean += distanceValue * distanceValue / (counter_xx + 1.0);
      ++counter_xx;
    }
  }

  double var_x = xxMean / 2.0;
  double var_y = yyMean / 2.0;
  double var_c = std::min(var_x, var_y);

  double resVal = std::max(var_x, var_y);

  if (var_c < std::sqrt(std::numeric_limits<double>::epsilon()))
    return resVal;

  return resVal / var_c;
}

double stat_bg_impl(const Rcpp::NumericVector &distanceMatrix,
                    const Rcpp::IntegerVector &firstGroupIndices,
                    const Rcpp::IntegerVector &secondGroupIndices)
{
  double xyMean = 0.0, xxMean = 0.0, yyMean = 0.0;
  unsigned int n1 =  firstGroupIndices.size();
  unsigned int n2 = secondGroupIndices.size();
  unsigned int counter_xy = 0, counter_xx = 0, counter_yy = 0;
  for (unsigned int i = 0;i < n1;++i)
  {
    for (unsigned int j = 0;j < n2;++j)
    {
      xyMean *= (counter_xy / (counter_xy + 1.0));
      double distanceValue = getElement(distanceMatrix, firstGroupIndices[i], secondGroupIndices[j]);
      xyMean += distanceValue / (counter_xy + 1.0);
      ++counter_xy;

      if (i == 0)
      {
        for (unsigned int k = j + 1;k < n2;++k)
        {
          yyMean *= (counter_yy / (counter_yy + 1.0));
          double distanceValue = getElement(distanceMatrix, secondGroupIndices[j], secondGroupIndices[k]);
          yyMean += distanceValue / (counter_yy + 1.0);
          ++counter_yy;
        }
      }
    }

    for (unsigned int j = i + 1;j < n1;++j)
    {
      xxMean *= (counter_xx / (counter_xx + 1.0));
      double distanceValue = getElement(distanceMatrix, firstGroupIndices[i], firstGroupIndices[j]);
      xxMean += distanceValue / (counter_xx + 1.0);
      ++counter_xx;
    }
  }

  return (xxMean - xyMean) * (xxMean - xyMean) + (yyMean - xyMean) * (yyMean - xyMean);
}

double stat_energy_impl(const Rcpp::NumericVector &distanceMatrix,
                        const Rcpp::IntegerVector &firstGroupIndices,
                        const Rcpp::IntegerVector &secondGroupIndices,
                        const unsigned int alphaValue)
{
  double xyMean = 0.0, xxMean = 0.0, yyMean = 0.0;
  unsigned int n1 =  firstGroupIndices.size();
  unsigned int n2 = secondGroupIndices.size();
  unsigned int counter_xy = 0, counter_xx = 0, counter_yy = 0;
  for (unsigned int i = 0;i < n1;++i)
  {
    for (unsigned int j = 0;j < n2;++j)
    {
      xyMean *= (counter_xy / (counter_xy + 1.0));
      double distanceValue = getElement(distanceMatrix, firstGroupIndices[i], secondGroupIndices[j]);
      if (alphaValue != 1)
        distanceValue = std::pow(distanceValue, (double)alphaValue);
      xyMean += distanceValue / (counter_xy + 1.0);
      ++counter_xy;

      if (i == 0)
      {
        for (unsigned int k = 0;k < n2;++k)
        {
          yyMean *= (counter_yy / (counter_yy + 1.0));
          double distanceValue = getElement(distanceMatrix, secondGroupIndices[j], secondGroupIndices[k]);
          if (alphaValue != 1)
            distanceValue = std::pow(distanceValue, (double)alphaValue);
          yyMean += distanceValue / (counter_yy + 1.0);
          ++counter_yy;
        }
      }
    }

    for (unsigned int j = 0;j < n1;++j)
    {
      xxMean *= (counter_xx / (counter_xx + 1.0));
      double distanceValue = getElement(distanceMatrix, firstGroupIndices[i], firstGroupIndices[j]);
      if (alphaValue != 1)
        distanceValue = std::pow(distanceValue, (double)alphaValue);
      xxMean += distanceValue / (counter_xx + 1.0);
      ++counter_xx;
    }
  }

  return xyMean - (xxMean + yyMean) / 2.0;
}

double stat_cq_impl(const Rcpp::NumericVector &similarityMatrix,
                    const Rcpp::IntegerVector &firstGroupIndices,
                    const Rcpp::IntegerVector &secondGroupIndices)
{
  double xyMean = 0.0, xxMean = 0.0, yyMean = 0.0;
  unsigned int n1 =  firstGroupIndices.size();
  unsigned int n2 = secondGroupIndices.size();
  unsigned int counter_xy = 0, counter_xx = 0, counter_yy = 0;
  for (unsigned int i = 0;i < n1;++i)
  {
    for (unsigned int j = 0;j < n2;++j)
    {
      xyMean *= (counter_xy / (counter_xy + 1.0));
      double similarityValue = getElement(similarityMatrix, firstGroupIndices[i], secondGroupIndices[j]);
      xyMean += similarityValue / (counter_xy + 1.0);
      ++counter_xy;

      if (i == 0)
      {
        for (unsigned int k = 0;k < n2;++k)
        {
          if (j == k)
            continue;

          yyMean *= (counter_yy / (counter_yy + 1.0));
          double similarityValue = getElement(similarityMatrix, secondGroupIndices[j], secondGroupIndices[k]);
          yyMean += similarityValue / (counter_yy + 1.0);
          ++counter_yy;
        }
      }
    }

    for (unsigned int j = 0;j < n1;++j)
    {
      if (i == j)
        continue;

      xxMean *= (counter_xx / (counter_xx + 1.0));
      double similarityValue = getElement(similarityMatrix, firstGroupIndices[i], firstGroupIndices[j]);
      xxMean += similarityValue / (counter_xx + 1.0);
      ++counter_xx;
    }
  }

  return xxMean + yyMean - 2 * xyMean;
}
