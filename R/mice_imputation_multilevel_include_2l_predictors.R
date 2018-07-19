## File Name: mice_imputation_multilevel_include_2l_predictors.R
## File Version: 0.09


#-----------------------------------------------------------------------------
# function for inclusion of group level predictors
mice_imputation_multilevel_include_2l_predictors <- function( y, x, ry, type, ... )
{
    X <- as.matrix( x[, type %in% c(1,2)] )
    X <- cbind( 1, X )
    # group level predictors
    if ( sum( type==-2 ) > 0 ){
        cluster <- x[, type==-2 ]
        if ( sum( type==2 ) > 0 ){
            x1a <- as.matrix( cbind( x[, type==- 2 ], x[, type==2 ]  ) )
            colnames(x1a) <- c( colnames(x)[ type==-2 ], colnames(x)[ type==2 ] )
            gm0 <- mice.impute.2l.groupmean(y=y, ry=FALSE * ry, x=x1a,
                            type=c( -2, rep(1, ncol(x1a)-1 ) ),
                            grmeanwarning=FALSE, ... )
            gm0 <- as.matrix(gm0)
            colnames(gm0) <- paste0("M.", colnames(x1a)[-1] )
            X <- as.matrix( cbind( X, gm0 ))
        }
    } else {
        cluster <- NULL
    }
    return(X)
}
#-----------------------------------------------------------------------------

.include.2l.predictors <- mice_imputation_multilevel_include_2l_predictors
