## File Name: GroupMean.R
## File Version: 1.252

GroupMean  <- function( data, group, weights=NULL, extend=FALSE, elim=FALSE)
{
    res <- GroupMean_group_indices(group=group)
    groups <- res$groups
    index.group <- res$index.group

    if ( is.null(weights) ){
        Ngroup <- rowsum( 1 - is.na(data), index.group )
        data1 <- rowsum( data, index.group, na.rm=TRUE)
    } else {
        Ngroup <- rowsum( weights*(1 - is.na(data)), index.group )
        data1 <- rowsum( data*weights, index.group, na.rm=TRUE)
    }
    colnames(data1) <- colnames(data)
    data1 <- data1 / Ngroup
    data1 <- data.frame( group=groups, data1 )
    data1 <- GroupMean_extend_data(data=data1, index.group=index.group, extend=extend)

    if (elim){
        eps <- 1e-15
        Ngroup0 <- cbind(Ngroup, Ngroup)
        Ngroup1 <- GroupMean_extend_data(data=Ngroup0, index.group=index.group,
                                extend=extend)
        ng <- Ngroup1[,1]
        data2 <- ( ng*data1[,-1] - data ) / (ng-1+eps)
        data1[,-1] <- data2
    }
    return(data1)
}
