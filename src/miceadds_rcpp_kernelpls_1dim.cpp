//// File Name: miceadds_rcpp_kernelpls_1dim.cpp
//// File Version: 3.172


// [[Rcpp::depends(RcppArmadillo)]]

#include <RcppArmadillo.h>
// #include <Rcpp.h>

using namespace Rcpp;
using namespace arma;


///********************************************************************
///** kernelplsaux
// [[Rcpp::export]]
Rcpp::List kernelplsaux(Rcpp::NumericMatrix Yr, Rcpp::NumericMatrix Xr,
    Rcpp::NumericVector nc)
{
    int nobj = Xr.nrow();
    int npred = Xr.ncol();
    int nresp = Yr.ncol();
    int ncomp = nc[0];
    double eps_add = 1e-10;
    arma::mat X(Xr.begin(), nobj, npred, false);
                // reuses memory and avoids extra copy
    arma::mat Y(Yr.begin(), nobj, nresp, false);
    // define matrices
    arma::mat R = arma::zeros( npred, ncomp );
    arma::mat P = arma::zeros( npred, ncomp );
    arma::mat tQ = arma::zeros( ncomp, nresp );
    arma::mat B = arma::zeros( npred, ncomp );
    // auxiliary matrices;
    arma::mat wa;
    arma::mat ra;
    arma::mat ua;
    arma::mat pa;
    arma::mat qa;
    arma::mat ta;
    arma::mat tmp1;
    arma::mat tmp2 = P;
    arma::mat tmp3;
    arma::mat tsq;
    arma::mat tsq1;
    arma::mat tmp11;
    arma::mat tmp12;
    arma::mat W = P;
    arma::mat U = arma::zeros( nobj, ncomp );
    arma::mat TT = arma::zeros( nobj, ncomp );
    arma::mat tsqs = arma::ones( ncomp, 1 );
    arma::mat fitted = arma::zeros( nobj, ncomp );

    //    XtY <- crossprod(X, Y)
    arma::mat XtY = arma::mat( arma::trans(X) * Y );

    // set component index;
    for (int aa=0;aa<ncomp;++aa){
        //    ## 2.
        //    w.a <- XtY / sqrt(c(crossprod(XtY)))
        wa = arma::mat( XtY / arma::repmat( arma::sqrt(
                                arma::trans(XtY) * XtY + eps_add ), npred,1) );
        //        ## 3.
        //        r.a <- w.a
        //            for (j in 1:(a - 1))
        //                r.a <- r.a - (P[,j] %*% w.a) * R[,j]
        //        }
        ra = wa;
        for (int jj=0; jj < aa; ++jj ){
            tmp11 = arma::mat( arma::trans(P.col(jj))  * wa );
            tmp12 = arma::repmat( tmp11, npred, 1 );
            ra = arma::mat( ra -  tmp12 % R.col(jj) );
        }
        //        ## 4.
        //        t.a <- X %*% r.a
        //        tsq <- c(crossprod(t.a))
        //        p.a <- crossprod(X, t.a) / tsq
        //        q.a <- crossprod(XtY, r.a) / tsq
        ta = arma::mat( X * ra );
        tsq = arma::mat( arma::trans(ta) * ta );
        tsq1 = arma::repmat( tsq, npred, 1 );
        pa = arma::mat( ( arma::trans(X) * ta ) / tsq1);
        qa = arma::mat( ( arma::trans(XtY) * ra ) / tsq );
        //     ## 5.
        //     XtY <- XtY - (tsq * p.a) %*% t(q.a)
        tmp2 = arma::repmat( tsq, npred, 1 );
        XtY = arma::mat( XtY - ( tmp2 % pa ) * arma::trans(qa) );
        //     ## 6.-8.
        //     R[,a] <- r.a
        //     P[,a] <- p.a
        //     tQ[a,] <- q.a
        //     B[,a] <- R[,1:a, drop=FALSE] %*% tQ[1:a,, drop=FALSE]
        R.col(aa) = ra;
        P.col(aa) = pa;
        tQ.row(aa) = qa;
        B.col(aa) = arma::mat(
                R( arma::span(0,npred-1), arma::span(0,aa) ) *
                tQ( arma::span(0,aa), arma::span(0,0) )
            );
        // # !stripped
        // tsqs[a] <- tsq
        tsqs( aa, 0 ) = tsq(0,0);
        //     ## Extra step to calculate Y scores:
        //     u.a <- Y %*% q.a / c(crossprod(q.a)) # Ok for nresp == 1 ??
        //    ua = arma::mat( ( Y * qa ) /
        //        arma::repmat( trans(qa) * qa, nobj, 1 ) );
        ua = arma::mat( (Y*qa) / arma::repmat( arma::trans(qa)*qa + eps_add, nobj, 1) );
        //     ## make u orth to previous X scores:
        //     if (a > 1) u.a <- u.a - TT %*% (crossprod(TT, u.a) / tsqs)
        if (aa > 0 ){
            ua = ua - arma::mat( TT * ( (arma::trans(TT)*ua) / ( tsqs + eps_add ) ));
        }
        //     U[,a] <- u.a
        U.col(aa) = ua;
        //     TT[,a] <- t.a
        TT.col(aa) = ta;
        //     W[,a] <- w.a
        W.col(aa) = wa;
        //    fitted[,a] <- TT[,1:a] %*% tQ[1:a,, drop=FALSE]
        fitted.col(aa) = arma::mat(
            TT( arma::span(0,nobj-1), arma::span(0,aa) ) *
            tQ( arma::span(0,aa), arma::span(0,0) )
                    );
    }  // end loop components
    //*******************

    ////////////////////////////////////
    // OUTPUT:
    return Rcpp::List::create(
                Rcpp::Named("Y_used") = Y,
                Rcpp::Named("X_used") = X,
                Rcpp::Named("R") = R,
                Rcpp::Named("P") = P,
                Rcpp::Named("tQ") = tQ,
                Rcpp::Named("B")=B,
                Rcpp::Named("XtY") = XtY,
                Rcpp::Named("wa") = wa,
                Rcpp::Named("ra")=ra,
                Rcpp::Named("ta")=ta,
                Rcpp::Named("tsq") = tsq,
                Rcpp::Named("pa")=pa,
                Rcpp::Named("qa")=qa,
                Rcpp::Named("W")=W,
                Rcpp::Named("U")=U,
                Rcpp::Named("TT")=TT,
                Rcpp::Named("tsqs")=tsqs,
                Rcpp::Named("fitted")=fitted
            );
}

///********************************************************************
///** kernelpls_1dim_C
// [[Rcpp::export]]
Rcpp::List kernelpls_1dim_C( Rcpp::NumericMatrix Yr,
    Rcpp::NumericMatrix Xr, Rcpp::NumericVector nc )
{
    // run PLS algorithm 'kernelplsaux'
    Rcpp::List res = kernelplsaux(Yr, Xr,  nc);
    return res;
}

