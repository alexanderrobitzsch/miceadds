## File Name: pca.covridge.R
## File Version: 0.11

pca.covridge <- function( x, ridge=1E-10 )
{
    cx <- stats::cov(x)
    diag(cx) <- diag(cx) + ridge
    pcax <- stats::princomp( covmat=cx )
    L <- pcax$loadings
    sdev <- pcax$sdev
    D <- diag( pcax$sdev^2)
    scores <- x %*% L
    res <- list( "loadings"=L, "scores"=scores, "sdev"=sdev )
    return(res)
}
