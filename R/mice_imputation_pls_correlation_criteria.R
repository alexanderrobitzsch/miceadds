## File Name: mice_imputation_pls_correlation_criteria.R
## File Version: 0.14


mice_imputation_pls_correlation_criteria <- function( y, x, ry, use.ymat, wt=NULL)
{
    if ( ! use.ymat ){
        c1 <- cor_avoid_zero( x=y[ry], y=x[ry,], wt=wt[ry] )
    } else {
        # look for correlations of all the dummy variables
        c1 <- cor_avoid_zero( x=y[ry], y=x[ry,], wt=wt[ry] )
        c1 <- apply( abs(c1), 2, mean, na.rm=TRUE )
    }
    return(c1)
}

