## File Name: mice_multilevel_impute_groupmean.R
## File Version: 0.11



mice_multilevel_impute_groupmean <- function (y, ry, x, type, grmeanwarning=TRUE, ...)
{
    if ( ( ncol(x) > 2 ) & grmeanwarning ){
        warning('\nMore than one variable is requested to be aggregated.\n')
    }
    # calculate aggregated values
    a1 <- stats::aggregate( x[, type %in% c(1,2) ],
                            list( x[,type==-2] ), mean, na.rm=TRUE)
    i1 <- match( x[,type==-2], a1[,1] )
    ximp <- as.matrix(a1[i1,-1])
    colnames(ximp) <- paste( names(type)[ type %in% c(1,2) ],
                                names(type)[ type==-2 ], sep='.' )
    return(ximp)
}
