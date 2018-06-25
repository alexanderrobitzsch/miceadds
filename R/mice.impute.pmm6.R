## File Name: mice.impute.pmm6.R
## File Version: 0.21


mice.impute.pmm6 <- function (y, ry, x, donors=3, noise=10^5, ridge=10^(-5), ...)
{
    x <- cbind(1, as.matrix(x))
    # dummy response indicator
    ry01 <- 1*ry
    # sample regression coefficients
    coefu <- stats::rnorm( ncol(x) )
    # create donor samples
    donorsample <- sample( 1:(2*donors), sum( ! ry), replace=TRUE) - donors
    donorsample <- ifelse( donorsample <=0, donorsample - 1, donorsample )
    #----- Rcpp code
    ##*** change due to bug report SvB 2018-05-18
    imp <- miceadds_rcpp_impute_pmm6( y=y, ry01=ry01, x=x, ridge=ridge,
            coefu1=coefu, donorsample=donorsample )
    return(imp)
}
