## File Name: cor_avoid_zero.R
## File Version: 0.204

#--- computes correlation by avoiding division by zero
#--- due to zero standard deviations
cor_avoid_zero <- function( x, y, eps=1E-20, wt=NULL, use_Rcpp=TRUE)
{
    if ( ! use_Rcpp ){
        if (is.null(wt)){
            c1 <- stats::cov( x=x, y=y)
            sd1 <- sd0(x)
            sd2 <- sd0(y)
            cor1 <- c1 / outer( sd1 + eps, sd2 + eps )

        } else {
            x1 <- cbind(x,y)
            res <- stats::cov.wt(x=x1, wt=wt, cor=TRUE)
            cor1 <- res$cor[1,-1,drop=FALSE]
        }
    } else {
        if (is.null(wt)){
            n <- nrow(y)
            wt <- rep(1,n)
        }
        res <- miceadds_rcpp_weighted_cor( x=as.matrix(x), y=y, wt=wt, eps=eps^2)
        cor1 <- res$Cor_xy
    }
    return(cor1)
}

