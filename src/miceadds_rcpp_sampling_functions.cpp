//// File Name: miceadds_rcpp_sampling_functions.cpp
//// File Version: 0.45


// [[Rcpp::depends(RcppArmadillo)]]

#include <RcppArmadillo.h>
// #include <Rcpp.h>


using namespace Rcpp;
using namespace arma;

// [[Rcpp::interfaces(r, cpp)]]

///********************************************************************
///** miceadds_rcpp_arma_chol_ridge
// [[Rcpp::export]]
arma::mat miceadds_rcpp_arma_chol_ridge(arma::mat sigma0, double ridge)
{
    arma::mat sigma=sigma0;
    int np = sigma.n_rows;
    for (int pp=0; pp<np; pp++){
        sigma(pp,pp) = sigma0(pp,pp) * ( 1 + ridge );
        if (sigma(pp,pp)<ridge){
            sigma(pp,pp) = ridge;
        }
    }
    arma::mat sigma_chol = arma::chol(sigma);
    return sigma_chol;
}
///********************************************************************

///********************************************************************
///** miceadds_rcpp_mvrnorm
// [[Rcpp::export]]
arma::colvec miceadds_rcpp_mvrnorm(arma::colvec mu, arma::mat sigma)
{
    int ncols = sigma.n_cols;
    double ridge = 1e-6;
    arma::vec Y = arma::randn(ncols);
    arma::mat sigma_chol = miceadds_rcpp_arma_chol_ridge(sigma, ridge);
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
    double ridge = 1e-6;
    // Dimension of returned wishart
    int m = S.n_rows;
    // Z composition:
    // sqrt chisqs on diagonal
    // random normals below diagonal
    // misc above diagonal
    arma::mat Z(m,m);
    // Fill the diagonal
    for(int i = 0; i < m; i++){
        Z(i,i) = sqrt( ::Rf_rchisq(df-i) );
    }
    // Fill the lower matrix with random guesses
    for(int j = 0; j < m; j++){
        for(int i = j+1; i < m; i++){
            Z(i,j) = ::Rf_rnorm(0, 1);
        }
    }
    // Lower triangle * chol decomp    
    arma::mat S_chol = miceadds_rcpp_arma_chol_ridge(S, ridge);
    arma::mat C = arma::trans( arma::trimatl(Z) ) * S_chol;
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
///** miceadds_rcpp_rtnorm_double
// [[Rcpp::export]]
double miceadds_rcpp_rtnorm_double( double mu, double sigma, double lower,
        double upper )
{
    double y1=0;
    double y2=0;
    double temp=0;
    Rcpp::NumericVector val = Rcpp::runif(1);
    y1 = ::Rf_pnorm5(lower, mu, sigma, 1, 0);
    y2 = ::Rf_pnorm5(upper, mu, sigma, 1, 0);
    temp = y1 + val[0] * ( y2 - y1 );
    double z = ::Rf_qnorm5(temp, mu, sigma, 1, 0);
    //--- output
    return z;
}
///********************************************************************

///********************************************************************
///** miceadds_rcpp_rtnorm
// [[Rcpp::export]]
arma::colvec miceadds_rcpp_rtnorm( arma::colvec mu,
            arma::colvec sigma, arma::colvec lower, arma::colvec upper )
{
    int N = mu.size();
    arma::colvec z(N);
    for (int nn=0; nn<N; nn++){
        z(nn,0) = miceadds_rcpp_rtnorm_double(mu(nn,0), sigma(nn,0),
                        lower(nn,0), upper(nn,0) );
    }
    //--- output
    return z;
}
///********************************************************************

///********************************************************************
///** miceadds_rcpp_arma2vec
// [[Rcpp::export]]
Rcpp::NumericVector miceadds_rcpp_arma2vec(arma::colvec x)
{
    Rcpp::NumericVector y = Rcpp::NumericVector(x.begin(), x.end());
    return y;
}
///********************************************************************

///********************************************************************
///** miceadds_rcpp_pnorm
// [[Rcpp::export]]
Rcpp::NumericVector miceadds_rcpp_pnorm( Rcpp::NumericVector x,
    Rcpp::NumericVector mu, double sigma)
{
    Rcpp::NumericVector x1 = x - mu;
    Rcpp::NumericVector y = Rcpp::pnorm( x1, 0.0, sigma);
    //--- output
    return y;
}
///********************************************************************

///********************************************************************
///** miceadds_rcpp_qnorm
// [[Rcpp::export]]
Rcpp::NumericVector miceadds_rcpp_qnorm( Rcpp::NumericVector x,
    Rcpp::NumericVector mu, double sigma)
{
    Rcpp::NumericVector y = Rcpp::qnorm( x, 0.0, sigma);
    y = y + mu;
    //--- output
    return y;
}
///********************************************************************


///********************************************************************
///** miceadds_rcpp_rtnorm2
// [[Rcpp::export]]
arma::colvec miceadds_rcpp_rtnorm2( arma::colvec mu,
            double sigma0, arma::colvec lower, arma::colvec upper,
            double minval, double maxval)
{
    int N = mu.size();
    Rcpp::NumericVector val = Rcpp::runif(N);
    Rcpp::NumericVector lower1 = miceadds_rcpp_arma2vec(lower);
    Rcpp::NumericVector upper1 = miceadds_rcpp_arma2vec(upper);
    Rcpp::NumericVector mu1 = miceadds_rcpp_arma2vec(mu);
    Rcpp::NumericVector y1 = miceadds_rcpp_pnorm( lower1, mu1, sigma0);
    Rcpp::NumericVector y2 = miceadds_rcpp_pnorm( upper1, mu1, sigma0);    
    Rcpp::NumericVector temp(N);
    for (int nn=0; nn<N; nn++){
        if (y1[nn] < minval){ y1[nn] = minval; }
        if (y2[nn] > maxval){ y2[nn] = maxval; }
        temp[nn] = y1[nn] + val[nn] * ( y2[nn] - y1[nn] );
    }
    Rcpp::NumericVector z0 = miceadds_rcpp_qnorm( temp, mu1, sigma0);    
    arma::colvec z = Rcpp::as< arma::colvec >(z0);    
    //--- output
    return z;
}
///********************************************************************



// Rcpp::Rcout << "verbose" << ii << std::endl;
