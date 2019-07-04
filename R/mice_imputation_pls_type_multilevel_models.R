## File Name: mice_imputation_pls_type_multilevel_models.R
## File Version: 0.08

mice_imputation_pls_type_multilevel_models <- function(x, x_, type, type_)
{
    type_ <- type_[ ! ( type_ %in% c(0,1) ) ]
    LT <- length(type_)
    if (LT>0){
        x <- x[, apply(x, 2, stats::sd) > 1e-10, drop=FALSE ]
        x <- cbind( x_[, names(type_)], x)
        colnames(x)[1:LT] <- names(type_)
        type <- rep(1, ncol(x))
        names(type) <- colnames(x)
        names(type)[1:LT] <- names(type_)
        type[1:LT] <- type_
    } else {
        type <- rep(1, ncol(x)-1)
        names(type) <- colnames(x)[-c(1)]
    }
    res <- list(x=x, type=type)
    return(res)
}
