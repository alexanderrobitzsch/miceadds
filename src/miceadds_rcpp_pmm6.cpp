//// File Name: miceadds_rcpp_pmm6.cpp
//// File Version: 1.122

// [[Rcpp::depends(RcppArmadillo)]]

#include <RcppArmadillo.h>
// #include <Rcpp.h>

using namespace Rcpp;
using namespace arma;


///********************************************************************
///** miceadds_rcpp_impute_pmm6
// [[Rcpp::export]]
Rcpp::NumericVector miceadds_rcpp_impute_pmm6( Rcpp::NumericVector y,
        Rcpp::NumericVector ry01, Rcpp::NumericMatrix x, double ridge,
        Rcpp::NumericVector coefu1, Rcpp::NumericVector donorsample )
{
    // y // varlist only one variable
    //  ry01(ry01_); // varlist only one variable
    //  x(x_);
    // double ridge = Rcpp::as<double>(ridge_);   // ridge parameter
    //  coefu1(coefu_); // sampling regression coefficients
    // donorsample(donorsample_); // sampling regression coefficients

    //********************************
    // fit linear model
    arma::colvec ry01A(ry01.begin(), ry01.size(), false);
    arma::uvec ind_obs = arma::find(ry01A==1);
    int n = x.nrow();
    int k = x.ncol();
    int nobs = ind_obs.size();
    int nmiss = n - nobs;
    arma::mat xA(x.begin(), n, k, false);
    arma::mat xobs=xA.rows(ind_obs);
    arma::colvec yA(y.begin(), y.size(), false);
    arma::colvec yobs = yA.rows(ind_obs);
    arma::mat xtx = arma::mat( arma::trans(xobs) * xobs );
    for (int ii=0;ii<k;ii++){
        xtx(ii,ii)=xtx(ii,ii)+ridge;
    }
    arma::mat xinv = arma::inv( xtx );
    arma::mat coef2 = arma::mat( xinv * arma::trans(xobs) * yobs );
    arma::colvec resid = arma::mat( yobs - xobs*coef2 );
    double sig2 = arma::as_scalar( arma::trans(resid)*resid/(n-k) );
    // sample new coefficient coef
    arma::mat vcoef = arma::mat( sig2 * xinv );
    arma::colvec coefu(coefu1.begin(), coefu1.size(), false);
    arma::mat coef = arma::mat( coef2 + arma::chol(vcoef) * coefu );

    //*********************************
    // prediction and matrix arrangement
    Rcpp::NumericVector yimp_ind(nmiss);
    Rcpp::NumericVector yimp(nmiss);
    Rcpp::NumericVector yimp_donors(nobs);

    double t1=0;
    double t2=0;
    arma::mat ypred_mis = arma::mat( xA * coef );
    Rcpp::NumericMatrix YSM1(n,5);
    arma::mat YSM(YSM1.begin(), n, 5, false);
    for (int nn=0;nn<n; nn++){
        YSM(nn,0) = ypred_mis(nn,0);
        YSM(nn,1) = ry01[nn];
        t1 ++;
        if ( ry01[nn] == 1 ){
            YSM(nn,2) = t1;
            YSM(nn,4) = y[nn];
        }
        if ( ry01[nn] == 0 ){
            t2 ++;
            YSM(nn,3) = t2;
        }
    }

    //********************************************
    // sorting
    // sort according to variable
    arma::colvec Mvv = YSM.col(0);
    arma::uvec indvv = arma::sort_index( Mvv );
    arma::mat YSM_sort=YSM.rows(indvv);
    t1=0;
    int zz=0;
    double g1 =0;
    for (int nn=0;nn<n; nn++){
        if ( YSM_sort(nn,1) == 1 ){
            t1 ++;
            yimp_donors[ t1-1 ] = YSM_sort(nn,4);
        }
        if ( YSM_sort(nn,1) == 0 ){
            g1 = t1 + donorsample[zz];
            if ( g1 <= 0 ){ g1 = 1; }
            if (g1 > nobs){ g1 = nobs; }
            zz ++;
            yimp_ind[ YSM_sort(nn,3) - 1 ] = g1;
        }
    }
    // allocation
    for (int zz=0;zz<nmiss;zz++){
        yimp[zz] = yimp_donors[ yimp_ind[zz] - 1 ];
    }
    return yimp;
}



