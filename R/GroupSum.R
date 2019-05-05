## File Name: GroupSum.R
## File Version: 1.11


GroupSum <- function( data, group, weights=NULL, extend=FALSE)
{
    res <- GroupMean_group_indices(group=group)
    groups <- res$groups
    index.group <- res$index.group
    if ( is.null(weights) ){
        data1 <- rowsum( data, index.group, na.rm=TRUE)
    } else {
        data1 <- rowsum( data*weights, index.group, na.rm=TRUE)
    }
    colnames(data1) <- colnames(data)
    data1 <- data.frame( group=groups, data1 )
    data1 <- GroupMean_extend_data(data=data1, index.group=index.group, extend=extend)
    return(data1)
}
