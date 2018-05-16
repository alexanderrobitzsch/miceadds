//// File Name: miceadds_rcpp_sampling_functions.h
//// File Version: 0.02

#ifndef _MICEADDS_SAMPLING_FUNCTIONS_H
#define _MICEADDS_SAMPLING_FUNCTIONS_H
 
#include <RcppArmadillo.h>
using namespace Rcpp;
using namespace arma;

arma::colvec miceadds_rcpp_mvrnorm(arma::colvec mu, arma::mat sigma);

arma::mat miceadds_rcpp_rwishart(int df, arma::mat S);

arma::mat miceadds_rcpp_riwishart(int df, arma::mat S);

arma::colvec miceadds_rcpp_rtnorm( Rcpp::IntegerVector y, arma::colvec mu,
            arma::colvec sigma, arma::colvec lower, arma::colvec upper );

#endif
