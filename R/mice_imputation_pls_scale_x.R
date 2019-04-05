## File Name: mice_imputation_pls_scale_x.R
## File Version: 0.095

mice_imputation_pls_scale_x <- function( x, imputationWeights, use_weights)
{
    #*** standardize x
    if (use_weights){ # with weights
        n <- length(imputationWeights)
        p <- ncol(x)
        iW <- matrix(imputationWeights, nrow=n, ncol=p)
        Mx <- colSums( iW * x ) / colSums( iW )
        SDx <- sqrt( colSums( iW * x^2 ) / colSums(iW) - Mx^2  )
        M_mat <- matrix(Mx, nrow=n, ncol=p, byrow=TRUE)
        eps <- 1e-10
        SD_mat <- matrix(SDx+eps, nrow=n, ncol=p, byrow=TRUE)
        x0 <- x <- ( x - M_mat ) / SD_mat
    } else {  # without weights
        x0 <- x <- ma.scale2(x)
    }
    return(x)
}
