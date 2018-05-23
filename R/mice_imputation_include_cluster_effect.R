## File Name: mice_imputation_include_cluster_effect.R
## File Version: 0.06

mice_imputation_include_cluster_effect <- function( x, y, ry, type )
{
    #***********************
    # include cluster effect: group mean (eliminating the subject under study)
    if ( sum(type==-2) > 0 ){
        x1 <- cbind( y, x[, which( type==-2) ] )
        type1 <- c( 1,-2)
        ximp <- mice.impute.2l.groupmean.elim( y=y, ry=ry, x=x1, type=type1 )
        x <- as.matrix( cbind( x, ximp ) )
        newname <- "y_aggr"
        colnames(x)[ ncol(x) ] <- newname
        x10 <- x0 <- x
        type <- c( type, 1 )
        names(type)[ length(type) ] <- newname
        # check whether x10 and x0 are really needed here
    }
    res <- list( x=x, type=type )
    return(res)
}
