## File Name: pca.covridge.R
## File Version: 0.05
## File Last Change: 2017-02-06 11:05:50
pca.covridge <- function( x , ridge = 1E-10 ){
    cx <- stats::cov(x)
    diag(cx) <- diag(cx) + ridge
    pcax <- stats::princomp( covmat=cx )
    L <- pcax$loadings
    sdev <- pcax$sdev
    D <- diag( pcax$sdev^2)
    # scores <- t( t(L) %*%  t( x ) )  # = x %*% L
	scores <- x %*% L
    res <- list( "loadings" = L , "scores" = scores , 
                "sdev" = sdev )
    return(res)            
}
