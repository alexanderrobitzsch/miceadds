## File Name: mice.impute.catpmm.R
## File Version: 0.389


mice.impute.catpmm <- function(y, ry, x, donors=5, ridge=10^(-5), ...)
{
    require_namespace(pkg='sirt')
    res <- mice_imputation_factor_pmm_prepare(y=y)
    y <- res$y
    y_aggr <- res$y_aggr
    is_factor <- res$is_factor

    ind1 <- which(ry)
    ind0 <- which(!ry)
    n <- nrow(x)
    dfr <- data.frame(y=y,x)
    #-- add noise to x variables
    vars_x <- colnames(x)
    dfr <- mice_impute_catpmm_add_noise_x(dfr=dfr, vars_x=vars_x, ridge=ridge)

    #-- create dummy variables
    res <- mice_impute_catpmm_create_dummies_y(y=y, dfr=dfr, ridge=ridge)
    y1 <- res$y1
    ny <- res$ny
    dfr <- res$dfr
    n1 <- length(ind1)
    n0 <- length(ind0)

    #- bootstrap sample
    boot_sample <- sample(ind1, size=n1, replace=TRUE)
    #- run linear model on bootstrap sample
    form1 <- paste0('cbind(',paste0(colnames(y1), collapse=','), ')')
    form2 <- paste0(vars_x, collapse=' + ')
    form <- as.formula(paste0(form1, ' ~ ', form2 ))
    mod <- stats::lm( formula=form, data=dfr[boot_sample, ])

    #-- predictions
    pred <- predict(mod, newdata=dfr )
    pred1 <- pred[ind1,,drop=FALSE]
    pred0 <- pred[ind0,,drop=FALSE]

    # define distance matrix
    distmat <- matrix(0, nrow=n0, ncol=n1)
    for (ii in 1L:ny){
        M1 <- matrix( pred1[,ii], nrow=n0, ncol=n1, byrow=TRUE )
        M0 <- matrix( pred0[,ii], nrow=n0, ncol=n1, byrow=FALSE )
        distmat <- distmat + abs(M0-M1)
    }
    donor_ind <- sirt::rowKSmallest2.sirt(matr=distmat, K=donors )$smallind
    # sampled indices
    ind_sample <- sample( 1L:donors, n0, replace=TRUE )
    # select indices
    res1 <- donor_ind[ cbind( 1L:n0, ind_sample) ]

    # search for imputed values
    imp <- y[ ind1[res1] ]
    imp <- mice_imputation_factor_pmm_convert_factor(imp=imp,
                    is_factor=is_factor, y_aggr=y_aggr)

    return(imp)
}
