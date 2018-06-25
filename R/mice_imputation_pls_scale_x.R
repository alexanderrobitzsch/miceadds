## File Name: mice_imputation_pls_scale_x.R
## File Version: 0.06

mice_imputation_pls_scale_x <- function( x, imputationWeights, use_weights)
{
    #*** standardize x
    if ( use_weights ){ # with weights
        n <- length(imputationWeights)
        iW <- outer( imputationWeights, rep(1,ncol(x) ) )
        Mx <- colSums( iW * x )  / colSums( iW )
        SDx <- sqrt( colSums( iW * x^2 ) / colSums(iW) - Mx^2  )
        x0 <- x <- ( x - outer( rep(1,n), t(Mx) ) ) / outer( rep(1,n), t(SDx) )
    } else {  # without weights
        x0 <- x <- ma.scale2( x )
    }
    return(x)
}
