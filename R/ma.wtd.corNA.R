## File Name: ma.wtd.corNA.R
## File Version: 0.15


#--- weighted correlation using cov2cor function
ma.wtd.corNA <- function( data, weights=NULL, vars=NULL, method="unbiased" )
{
    #*** pre-processing
    res <- ma_wtd_stat_prepare_data(data=data, weights=weights, vars=vars )
    data <- res$data
    weights <- res$weights
    M <- length(data)
    #*** weighted covariance
    V <- ncol(data[[1]])
    res <- array( NA, dim=c(M,V,V) )
    dimnames(res)[[2]] <- colnames(data[[1]] )
    dimnames(res)[[3]] <- colnames(data[[1]] )
    for (ii in 1:M){
        data1 <- data[[ii]]
        covXY <- ma_wtd_cov_NA_compute_wtd_cov( data1=data1, vars=vars,
                        weights=weights, method=method )
        res[ii,,] <- stats::cov2cor(V=covXY)
    }
    res <- colMeans(res)
    return(res)
}

