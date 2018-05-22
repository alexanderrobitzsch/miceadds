//// File Name: miceadds_rcpp_scale.cpp
//// File Version: 5.09


#include <Rcpp.h>

using namespace Rcpp;


///*******************************************
/// Rcpp implementation of base::scale


///********************************************************************
///** scale2_C
// [[Rcpp::export]]
Rcpp::NumericMatrix scale2_C( Rcpp::NumericMatrix x )
{
    int n = x.nrow();
    int p = x.ncol();
    Rcpp::NumericMatrix y(n,p);
    double mvv=0;
    double sdvv=0;
    double eps_add = 1e-10;
    for (int vv=0;vv<p;vv++){
        mvv=0;
        sdvv=0;
        for (int ii=0;ii<n;ii++){
            mvv += x(ii,vv);
            sdvv += std::pow( x(ii,vv), 2.0 );
        }
        mvv = mvv / n;
        sdvv = std::sqrt( ( sdvv - n * mvv*mvv )/(n-1.0));
        // define standardization
        y(_,vv) = ( x(_,vv) - mvv ) / ( sdvv + eps_add );
    }
    //--- output
    return y;
}
///////////////////////////////////////////////////////////////



///********************************************************************
///** scale2_NA_C
// [[Rcpp::export]]
Rcpp::NumericMatrix scale2_NA_C( Rcpp::NumericMatrix x )
{
    int n = x.nrow();
    int p = x.ncol();
    Rcpp::NumericMatrix y(n,p);
    double mvv=0;
    double sdvv=0;
    double nvv=0;
    double eps_add = 1e-10;
    for (int vv=0;vv<p;vv++){
        mvv=0;
        sdvv=0;
        nvv=0;
        for (int ii=0;ii<n;ii++){
            if (! R_IsNA(x(ii,vv)) ) {
                mvv += x(ii,vv);
                sdvv += std::pow( x(ii,vv), 2.0 );
                nvv ++;
            }
        }
        mvv = mvv / nvv;
        sdvv = std::sqrt( ( sdvv - nvv * mvv*mvv )/(nvv - 1.0 ) );
        // define standardization
        y(_,vv) = ( x(_,vv) - mvv ) / ( sdvv + eps_add );
    }
    //--- output
    return y;
}

