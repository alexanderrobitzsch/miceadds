## File Name: lm_cluster_subset.R
## File Version: 0.02

lm_cluster_subset <- function(data, cluster, weights, subset, pos)
{
    if (! is.null(subset) ){
        data <- data[subset, ]
        cluster <- sirt_subset_object(x=cluster, subset=subset)
        weights <- sirt_subset_object(x=weights, subset=subset)
    }
    wgt__ <- weights
    assign(value=wgt__, x="wgt__", pos=pos)
    #--- output
    res <- list(data=data, cluster=cluster, wgt__=wgt__)
    return(res)
}
