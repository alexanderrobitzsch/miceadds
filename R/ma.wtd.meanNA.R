## File Name: ma.wtd.meanNA.R
## File Version: 0.14


#**** weighted mean
ma.wtd.meanNA <- function( data, weights=NULL, vars=NULL )
{
    #*** pre-processing
    res <- ma_wtd_stat_prepare_data(data=data, weights=weights, vars=vars )
    data <- res$data
    weights <- res$weights
    M <- length(data)
    #*** weighted means
    res <- matrix( NA, nrow=M, ncol=ncol(data[[1]]) )
    for (ii in 1:M){
        data1 <- data[[ii]]
        dataResp <- 1 - is.na( data1 )
        data1[ is.na(data1) ] <- 0
        data1 <- as.matrix( data1 )
        # calculate means
        sumweight <- colSums( dataResp * weights )
        M_vars <- colSums( data1 *  weights ) / sumweight
        res[ii,] <- M_vars
    }
    res <- colMeans(res)
    names(res) <- colnames(data[[1]])
    return(res)
}
