## File Name: ma_wtd_cov_NA_compute_wtd_cov.R
## File Version: 0.02

ma_wtd_cov_NA_compute_wtd_cov <- function(data1, vars, weights, method)
{
    if ( ! is.null(vars) ){
        data1 <- data1[, vars, drop=FALSE ]
    }
    dataResp <- 1 - is.na(data1)
    data1[ is.na(data1) ] <- 0
    data1 <- as.matrix(data1)
    # calculate means
    sumweight <- colSums( dataResp * weights )
    M_vars <- colSums( data1 * weights ) / sumweight
    M_varsM <- matrix( M_vars, nrow=nrow(data1), ncol=length(M_vars), byrow=TRUE )
    data1adj <- ( data1 - M_varsM ) * dataResp # take care of missings
    sqrtweights <- sqrt(weights)
    # calculate weighted covariance: cross-products
    covXY <- crossprod( data1adj * sqrtweights )
    covWXY <- crossprod( dataResp * sqrtweights )
    covXY <- covXY / covWXY
    # adjustment of covariance
    if (method=="unbiased" ){
        wgtadj <- crossprod( dataResp * weights )
        wgtadj <- ( covWXY^2 - crossprod( dataResp * weights ) ) / covWXY^2
        wgtadj <- 1 / wgtadj
        covXY <- wgtadj * covXY
    }
    return(covXY)
}
