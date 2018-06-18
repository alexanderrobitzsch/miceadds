//// File Name: miceadds_rcpp_sampling_functions.h
//// File Version: 0.45

#ifndef _MICEADDS_MICEADDS_RCPP_SAMPLING_FUNCTIONS_H
#define _MICEADDS_MICEADDS_RCPP_SAMPLING_FUNCTIONS_H
 
#include <RcppArmadillo.h>
using namespace Rcpp;
using namespace arma;

arma::mat miceadds_rcpp_arma_chol_ridge(arma::mat sigma0, double ridge);

arma::colvec miceadds_rcpp_mvrnorm(arma::colvec mu, arma::mat sigma);

arma::mat miceadds_rcpp_rwishart(int df, arma::mat S);

arma::mat miceadds_rcpp_riwishart(int df, arma::mat S);

double miceadds_rcpp_rtnorm_double( double mu, double sigma, double lower,
        double upper );

arma::colvec miceadds_rcpp_rtnorm( arma::colvec mu,
            arma::colvec sigma, arma::colvec lower, arma::colvec upper );

Rcpp::NumericVector miceadds_rcpp_arma2vec(arma::colvec x);

Rcpp::NumericVector miceadds_rcpp_pnorm( Rcpp::NumericVector x,
    Rcpp::NumericVector mu, double sigma);

Rcpp::NumericVector miceadds_rcpp_qnorm( Rcpp::NumericVector x,
    Rcpp::NumericVector mu, double sigma);

arma::colvec miceadds_rcpp_rtnorm2( arma::colvec mu,
            double sigma0, arma::colvec lower, arma::colvec upper,
            double minval, double maxval);

#endif // _MICEADDS_MICEADDS_RCPP_SAMPLING_FUNCTIONS_H
