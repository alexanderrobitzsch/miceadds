## File Name: mice_imputation_factor_pmm_prepare.R
## File Version: 0.085

mice_imputation_factor_pmm_prepare <- function(y)
{
    is_factor <- FALSE
    y_aggr <- NULL
    y0 <- y
    if (is.factor(y)){
        y <- as.integer(y)
        y_aggr <- stats::aggregate(y, list(y0), mean)
        is_factor <- TRUE
        # y <- as.factor(y-1)
    }
    #--- output
    res <- list( is_factor=is_factor, y_aggr=y_aggr, y=y)
    return(res)
}
