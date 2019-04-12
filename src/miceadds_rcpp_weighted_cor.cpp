//// File Name: miceadds_rcpp_weighted_cor.cpp
//// File Version: 0.288


// [[Rcpp::depends(RcppArmadillo)]]

#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;


///********************************************************************
///** miceadds_rcpp_weighted_sd
// [[Rcpp::export]]
Rcpp::List miceadds_rcpp_weighted_sd( Rcpp::NumericMatrix x,
        Rcpp::NumericVector w, double eps)
{
    int N = x.nrow();
    int NV = x.ncol();
    Rcpp::NumericVector M(NV);
    Rcpp::NumericVector SD(NV);
    double m1 = 0;
    double sd1 = 0;
    double xt = 0;
    for (int vv = 0; vv<NV; vv++){
        m1 = 0;
        sd1 = 0;
        for (int nn=0; nn<N; nn++){
            xt = w[nn] * x(nn,vv);
            m1 += xt;
            sd1 += xt * x(nn,vv);
        }
        M[vv] = m1;
        sd1 = sd1 - m1*m1;
        if (sd1 < eps){ sd1 = eps;}
        SD[vv] = std::sqrt(sd1);
    }
    //***** OUTPUT *******
    return Rcpp::List::create(
                Rcpp::Named("M") = M,
                Rcpp::Named("SD") = SD
            );
}
///********************************************************************

///********************************************************************
///** miceadds_rcpp_weighted_cor
// [[Rcpp::export]]
Rcpp::List miceadds_rcpp_weighted_cor( Rcpp::NumericMatrix x,
    Rcpp::NumericMatrix y, Rcpp::NumericVector wt, double eps)
{
    int N = x.nrow();
    Rcpp::NumericVector w(N);
    double W = Rcpp::sum(wt);
    for (int nn=0; nn<N; nn++){
        w[nn] = wt[nn] / W;
    }

    int NX = x.ncol();
    int NY = y.ncol();

    // statistics x
    Rcpp::List res_x = miceadds_rcpp_weighted_sd(x, w, eps);
    Rcpp::NumericVector M_x = Rcpp::as< Rcpp::NumericVector >(res_x["M"]);
    Rcpp::NumericVector SD_x = Rcpp::as< Rcpp::NumericVector >(res_x["SD"]);
    // statistics y
    Rcpp::List res_y = miceadds_rcpp_weighted_sd(y, w, eps);
    Rcpp::NumericVector M_y = Rcpp::as< Rcpp::NumericVector >(res_y["M"]);
    Rcpp::NumericVector SD_y = Rcpp::as< Rcpp::NumericVector >(res_y["SD"]);

    // covariance
    Rcpp::NumericMatrix Cov_xy(NX, NY);
    Rcpp::NumericMatrix Cor_xy(NX, NY);
    double c1=0;
    for (int vv=0; vv<NX; vv++){
        for (int ww=0; ww<NY; ww++){
            c1 = 0;
            for (int nn=0; nn<N; nn++){
                c1 += w[nn] * x(nn,vv) * y(nn, ww);
            }
            Cov_xy(vv,ww) = c1 - M_x[vv]*M_y[ww];
            Cor_xy(vv,ww) = Cov_xy(vv,ww) / SD_x[vv] / SD_y[ww];
        }
    }

    //***** OUTPUT *******
    return Rcpp::List::create(
                Rcpp::Named("M_x") = M_x,
                Rcpp::Named("SD_x") = SD_x,
                Rcpp::Named("M_y") = M_y,
                Rcpp::Named("SD_y") = SD_y,
                Rcpp::Named("Cov_xy") = Cov_xy,
                Rcpp::Named("Cor_xy") = Cor_xy,
                Rcpp::Named("w") = w
            );
}
///********************************************************************

