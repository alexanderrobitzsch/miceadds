## File Name: ma.wtd.quantileNA.R
## File Version: 0.14



#*** weighted quantile
ma.wtd.quantileNA <- function( data, weights=NULL, vars=NULL,
            type=7, probs=seq(0,1,.25) )
{
    require_namespace("TAM")
    #*** pre-processing
    res <- ma_wtd_stat_prepare_data(data=data, weights=weights, vars=vars )
    data <- res$data
    weights <- res$weights
    M <- length(data)
    #*** weighted quantile
    V <- ncol(data[[1]])
    PP <- length(probs)
    res <- matrix( NA, nrow=M, ncol=V*PP )
    for (ii in 1:M){
        data1 <- data[[ii]]
        for (vv in 1:V){
            M1 <- TAM::weighted_quantile(x=data1[,vv], w=weights, type=type,
                            probs=probs )
            res[ii, 1:PP + (vv-1)*PP ]    <- M1
        }
    }
    res <- colMeans(res)
    res <- matrix( res, nrow=PP, ncol=V, byrow=FALSE)
    colnames(res) <- colnames(data[[1]])
    rownames(res) <- paste0(100*probs,"%")
    return(res)
}
