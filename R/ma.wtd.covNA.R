## File Name: ma.wtd.covNA.R
## File Version: 0.03



###############################################################################
# weighted covariance
ma.wtd.covNA <- function( data , weights = NULL , vars = NULL ,  method="unbiased" ){
    #*** pre-processing
    res <- ma.wtd.aux.data(data=data , weights=weights , vars = vars )
    data <- res$data
    weights <- res$weights
    M <- length(data)
    #*** weighted covariance
    V <- ncol(data[[1]])
    res <- array( NA , dim=c(M,V,V) )
    dimnames(res)[[2]] <- colnames(data[[1]] )
    dimnames(res)[[3]] <- colnames(data[[1]] )
    for (ii in 1:M){
        data1 <- data[[ii]]
        if ( ! is.null(vars) ){
            data1 <- data1[ , vars , drop=FALSE ]
                                }
        dataResp <- 1 - is.na( data1 )
        data1[ is.na(data1) ] <- 0
        data1 <- as.matrix( data1 )
        # calculate means
        sumweight <- colSums( dataResp * weights )
        M_vars <- colSums( data1 *  weights ) / sumweight
        M_varsM <- matrix( M_vars , nrow= nrow(data1) , ncol=length(M_vars ) , byrow=TRUE )
        data1adj <- ( data1 - M_varsM ) * dataResp # take care of missings
        sqrtweights <- sqrt( weights )
        # calculate weighted covariance
        # cross-products
        covXY <- crossprod( data1adj * sqrtweights )
        w1 <- covWXY <- crossprod( dataResp * sqrtweights )
        covXY <- covXY / covWXY
        # adjustment of covariance
        if (method == "unbiased" ){
            # wgtadj <- w1 - colSums( dataResp * weights^2 ) / w1
            # wgtadj <- w1 / wgtadj
            wgtadj <- crossprod( dataResp * weights^2 )
            wgtadj <- ( covWXY^2 - crossprod( dataResp * weights^2 ) ) / covWXY^2
            wgtadj <- 1 / wgtadj
            covXY <- wgtadj * covXY
                                }
        res[ii,,] <- covXY
                    }
        res <- colMeans(res)
        return( res )
        }
