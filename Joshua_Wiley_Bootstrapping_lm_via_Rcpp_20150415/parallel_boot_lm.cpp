// [[Rcpp::depends(RcppParallel)]]
// [[Rcpp::depends(RcppEigen)]]
#include <Rcpp.h>
#include <RcppEigen.h>
#include <RcppParallel.h>

using namespace std;
using namespace Rcpp;
using namespace RcppParallel;

using Eigen::Lower;
using Eigen::Map;
using Eigen::MatrixXd;
using Eigen::Upper;
using Eigen::VectorXd;
typedef Map<MatrixXd> MapMatd;
typedef Map<VectorXd> MapVecd;
typedef Eigen::ColPivHouseholderQR<MatrixXd> CPivQR;
typedef CPivQR::PermutationType Permutation;

inline MatrixXd orderMat(const MatrixXd X, const IntegerVector index)
{
  const int n(X.rows());
  const int k(X.cols());
  MatrixXd X2(n, k);
  for (int i = 0; i < n; i++) {
    for(int j = 0; j < k; j++) {
      X2(i, j) = X(index[i], j);
    }
  }
  return X2;
}

inline VectorXd orderVec(const VectorXd y, const IntegerVector index)
{
  const int n(y.size());
  VectorXd y2(n);
  for (int i = 0; i < n; i++) {
      y2[i] = y[index[i]];
  }
  return y2;
}


struct CVLM : public Worker
{
  // design matrix and outcome
  const Eigen::VectorXd y;
  const Eigen::MatrixXd X;
  const Rcpp::IntegerMatrix index;

  // some scratch objects
  Eigen::VectorXd yi;
  Eigen::MatrixXd Xi;
  Rcpp::IntegerVector tmpindex;


 // QR Decomposition
  CPivQR PQR;
  const Permutation Pmat;
  const int r;

  // output
  RMatrix<double> betamat;
  Eigen::VectorXd betahat;

  // initialize with input and output
  CVLM(const Eigen::MatrixXd X, const Eigen::VectorXd y, const Rcpp::IntegerMatrix index, Rcpp::NumericMatrix betamat)
    : y(y), X(X), index(index), PQR(X), Pmat(PQR.colsPermutation()), r(PQR.rank()), betamat(betamat) {}

  void operator()(std::size_t begin, std::size_t end) {

    for(int i = begin; i < end; i++) {
      Rcpp::IntegerVector tmpindex = index(_, i);
      Xi = orderMat(X, tmpindex);
      yi = orderVec(y, tmpindex);

      CPivQR PQR(Xi);
      betahat = PQR.solve(yi);

      for(int j = 0; j < X.cols(); j++) {
        betamat(j, i) = betahat(j);
      }
    }
  }
};

// [[Rcpp::export]]
NumericMatrix parallelFit(Eigen::MatrixXd x, Eigen::VectorXd dv, Rcpp::IntegerMatrix index) {

  // allocate the output matrix
  NumericMatrix betamat(x.cols(), index.ncol());

  // pass input and output
  CVLM cvLM(x, dv, index, betamat);

  // parallelFor to do it
  parallelFor(0, index.ncol(), cvLM);

  return betamat;
}
