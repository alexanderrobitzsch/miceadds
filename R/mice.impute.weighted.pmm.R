## File Name: mice.impute.weighted.pmm.R
## File Version: 0.28

mice.impute.weighted.pmm <- function (y, ry, x,  imputationWeights=NULL,
        pls.facs=NULL, interactions=NULL, quadratics=NULL, ...)
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
        yhatobs <- x[ry,] %*% parm$coef
        yhatmis <- x[!ry,] %*% parm$beta
        yimp <- apply(as.array(yhatmis), 1, mice::.pmm.match, yhat=yhatobs, y=y[ry], ... )
    }
    return(yimp)
}
