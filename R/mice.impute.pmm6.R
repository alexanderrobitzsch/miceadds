## File Name: mice.impute.pmm6.R
## File Version: 0.25


mice.impute.pmm6 <- function (y, ry, x, donors=3, noise=10^5, ridge=10^(-5), ...)
{
    res <- mice_imputation_factor_pmm_prepare(y=y)
    y <- res$y
    y_aggr <- res$y_aggr
    is_factor <- res$is_factor

    x <- cbind(1, as.matrix(x))
    # dummy response indicator
    ry01 <- 1*ry
    # sample regression coefficients
    coefu <- stats::rnorm( ncol(x) )
    # create donor samples
    donorsample <- sample( 1:(2*donors), sum( ! ry), replace=TRUE) - donors
    donorsample <- ifelse( donorsample <=0, donorsample - 1, donorsample )
    #----- Rcpp code
    imp <- miceadds_rcpp_impute_pmm6( y=y, ry01=ry01, x=x, ridge=ridge,
            coefu1=coefu, donorsample=donorsample )
    imp <- mice_imputation_factor_pmm_convert_factor(imp=imp,
                    is_factor=is_factor, y_aggr=y_aggr)
    return(imp)
}
