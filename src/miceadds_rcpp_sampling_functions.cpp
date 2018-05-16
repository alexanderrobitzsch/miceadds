//// File Name: miceadds_rcpp_sampling_functions.cpp
//// File Version: 0.02


// [[Rcpp::depends(RcppArmadillo)]]

#include <RcppArmadillo.h>
// #include <Rcpp.h>


using namespace Rcpp;
using namespace arma;

// [[Rcpp::interfaces(r, cpp)]]

///********************************************************************
///** miceadds_rcpp_mvrnorm
// [[Rcpp::export]]
arma::colvec miceadds_rcpp_mvrnorm(arma::colvec mu, arma::mat sigma)
{
    int ncols = sigma.n_cols;
    arma::vec Y = arma::randn(ncols);
    arma::mat sigma_chol = arma::chol(sigma);
    arma::colvec samp = mu + sigma_chol * Y;
    return samp;
}
///********************************************************************

///********************************************************************
//*** copied from https://github.com/coatless/r-to-armadillo/blob/master/src/distributions.cpp
///** miceadds_rcpp_rwishart
// [[Rcpp::export]]
arma::mat miceadds_rcpp_rwishart(int df, arma::mat S)
{
    // Dimension of returned wishart
    int m = S.n_rows;
    // Z composition:
    // sqrt chisqs on diagonal
    // random normals below diagonal
    // misc above diagonal
    arma::mat Z(m,m);
    // Fill the diagonal
    for(int i = 0; i < m; i++){
        Z(i,i) = sqrt(R::rchisq(df-i));
    }
    // Fill the lower matrix with random guesses
    for(int j = 0; j < m; j++){
        for(int i = j+1; i < m; i++){
            Z(i,j) = R::rnorm(0,1);
        }
    }
    // Lower triangle * chol decomp
    arma::mat C = arma::trans( arma::trimatl(Z) ) * arma::chol(S);
    // Return random wishart
    arma::mat samp = arma::trans(C)*C;
    return samp;
}
///********************************************************************

///********************************************************************
//*** copied from https://github.com/coatless/r-to-armadillo/blob/master/src/distributions.cpp
///** miceadds_rcpp_riwishart
// [[Rcpp::export]]
arma::mat miceadds_rcpp_riwishart(int df, arma::mat S)
{
    arma::mat S_inv = arma::inv(S);
    arma::mat samp = miceadds_rcpp_rwishart(df, S_inv);
    samp = arma::inv(samp);
    return samp;
}
///********************************************************************

///********************************************************************
///** miceadds_rcpp_rtnorm
// [[Rcpp::export]]
arma::colvec miceadds_rcpp_rtnorm( Rcpp::IntegerVector y, arma::colvec mu,
            arma::colvec sigma, arma::colvec lower, arma::colvec upper )
{
    int N = y.size();
    arma::colvec z(N);
    Rcpp::NumericVector val = Rcpp::runif(N);
    double y1=0;
    double y2=0;
    double temp=0;
    for (int nn=0; nn<N; nn++){
        y1 = R::pnorm(lower(nn,0), mu(nn,0), sigma(nn,0), 1, 0);
        y2 = R::pnorm(upper(nn,0), mu(nn,0), sigma(nn,0), 1, 0);
        temp = y1 + val[nn] * ( y2 - y1 );
        z(nn,0) = R::qnorm(temp, mu(nn,0), sigma(nn,0), 1, 0 );
    }
    //--- output
    return z;
}
///********************************************************************
