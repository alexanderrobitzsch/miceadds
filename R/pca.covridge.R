## File Name: pca.covridge.R
## File Version: 0.211

pca.covridge <- function( x, ridge=1E-10, wt=NULL )
{
    use_Rcpp <- TRUE
    if (is.null(wt)){
        cx <- stats::cov(x)
    } else {
        if (! use_Rcpp){
            cx <- stats::cov.wt(x=x, wt=wt)
            cx <- cx$cov
        } else {
            res <- miceadds_rcpp_weighted_cor( x=x, y=x, wt=wt, eps=ridge^2)
            cx <- res$Cov_xy
        }
    }
    diag(cx) <- diag(cx) + ridge
    pcax <- stats::princomp( covmat=cx )
    L <- pcax$loadings
    sdev <- pcax$sdev
    D <- diag(pcax$sdev^2)
    scores <- x %*% L
    res <- list( loadings=L, scores=scores, sdev=sdev )
    return(res)
}
