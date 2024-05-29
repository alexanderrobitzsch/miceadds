## File Name: mice.impute.2l.groupmean.elim.R
## File Version: 0.131

mice.impute.2l.groupmean.elim <- function (y, ry, x, type,  ...)
{
    if ( ncol(x) > 2){
        warning('\nOnly one variable is allowed to be aggregated.\n')
    }
    # aggregated mean of x
    clusterx <- paste( x[,type==-2] )
    a1 <- rowsum( x[, type %in% c(1,2) ], clusterx, na.rm=TRUE )
    a2 <- rowsum( 1+0*x[, type %in% c(1,2) ], clusterx, na.rm=TRUE )
    i1 <- match( clusterx, rownames(a1) )
    ximp <- ( a1[i1,] - x[, type %in% c(1,2), drop=FALSE] ) / ( a2[i1,] - 1 )
    ximp <- ximp[,1]
    ximp[ is.na( ximp) ] <- mean( ximp, na.rm=TRUE)
    return(ximp)
}
