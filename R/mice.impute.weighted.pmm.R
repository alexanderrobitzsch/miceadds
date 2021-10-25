## File Name: mice.impute.weighted.pmm.R
## File Version: 0.419

mice.impute.weighted.pmm <- function (y, ry, x, wy=NULL, imputationWeights=NULL,
        pls.facs=NULL, interactions=NULL, quadratics=NULL, donors=5, ...)
{
    # processing
    res <- mice_imputation_weighted_norm_prepare( x=x, ry=ry, y=y,
                imputationWeights=imputationWeights, interactions=interactions,
                quadratics=quadratics, pls.facs=pls.facs, pls.impMethod="pmm",    ... )
    yimp <- res$yimp
    pls.facs <- res$pls.facs
    yobs <- res$yobs
    xobs <- res$xobs
    weights.obs <- res$weights.obs
    x <- res$x

    #** weighted pmm
    if (is.null(pls.facs)){
        parm <- mice_imputation_weighted_norm_draw( yobs=yobs, xobs=xobs,
                        ry=ry, y=y, x=x, weights.obs=weights.obs, ... )
        wy <- mice_imputation_define_wy(wy=wy, ry=ry)
        yhatobs <- ( x[ry,] %*% parm$coef )[,1]
        yhatmis <- ( x[wy,] %*% parm$beta )[,1]
        yhatobs <- mice_imputation_add_jitter(x=yhatobs)
        yimp <- miceadds_rcpp_weighted_pmm_match( yhatmis=yhatmis,
                    yhatobs=yhatobs, yobs=y[ry], weights_obs=weights.obs,
                    donors=donors)

    }
    return(yimp)
}


# cat(" * pmm") ; a1 <- Sys.time(); print(a1-a0) ; a0 <- a1
