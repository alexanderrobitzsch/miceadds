## File Name: mice.impute.pmm3.R
## File Version: 4.65


mice.impute.pmm3 <- function (y, ry, x, donors=3, noise=10^5, ridge=10^(-5), ...)
{
    require_namespace(pkg="sirt")
    res <- mice_imputation_factor_pmm_prepare(y=y)
    y <- res$y
    y_aggr <- res$y_aggr
    is_factor <- res$is_factor

    x <- cbind(1, as.matrix(x))
    res <- mice_imputation_norm_draw(y=y, ry=ry, x=x, ridge=ridge, ...)
    yhatobs <- res$yhatobs
    yhatmis <- res$yhatmis
    yobs <- res$yobs
    # define distance matrix
    M1 <- matrix( yhatobs[,1], nrow=sum(!ry), ncol=sum(ry), byrow=TRUE )
    M2 <- matrix( yhatmis[,1], nrow=sum(!ry), ncol=sum(ry) )
    disty <- abs( M2 - M1 )
    donor.ind <- sirt::rowKSmallest2.sirt(matr=disty, K=donors )$smallind
    N1 <- nrow(disty)
    # sampled indices
    ind.sample <- sample( 1:donors, N1, replace=TRUE )
    # select index
    res1 <- donor.ind[ cbind( 1:N1, ind.sample) ]
    # search for imputed values
    imp <- yobs[ res1 ]
    imp <- mice_imputation_factor_pmm_convert_factor(imp=imp,
                    is_factor=is_factor, y_aggr=y_aggr)
    return(imp)
}

