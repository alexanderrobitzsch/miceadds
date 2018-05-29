## File Name: mice_inits.R
## File Version: 0.04

mice_inits <- function(dat, ignore=NULL)
{
    varnames <- colnames(dat)
    NV <- length(varnames)
    pred <- matrix(1 , nrow=NV, ncol=NV)
    colnames(pred) <- rownames(pred) <- varnames
    diag(pred) <- 0
    miss_prop <- colMeans( is.na(dat) )
    ind <- which( miss_prop == 0 )    
    method <- rep("pmm", NV)
    names(method) <- varnames
    if (length(ind) > 0){
        pred[ ind, ] <- 0
        method[ind] <- ""    
    }
    if ( ! is.null(ignore) ){
        method[ ignore ] <- ""
        pred[ ignore, ] <- 0
        pred[, ignore] <- 0
    }    
    #--- output
    res <- list( method=method, predictorMatrix=pred, miss_prop=miss_prop,
                    variables=varnames)
    return(res)
}
