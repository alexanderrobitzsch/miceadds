## File Name: pca.covridge.R
## File Version: 0.14

pca.covridge <- function( x, ridge=1E-10, wt=NULL )
{
    if (is.null(wt)){
        cx <- stats::cov(x)
    } else {
        cx <- stats::cov.wt(x=x, wt=wt)
        cx <- cx$cov
    }
    diag(cx) <- diag(cx) + ridge
    pcax <- stats::princomp( covmat=cx )
    L <- pcax$loadings
    sdev <- pcax$sdev
    D <- diag( pcax$sdev^2)
    scores <- x %*% L
    res <- list( "loadings"=L, "scores"=scores, "sdev"=sdev )
    return(res)
}
