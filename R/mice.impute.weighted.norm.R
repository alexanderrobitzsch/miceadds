## File Name: mice.impute.weighted.norm.R
## File Version: 0.402

mice.impute.weighted.norm <- function(y, ry, x, wy=NULL, ridge=.00001, pls.facs=NULL,
    imputationWeights=NULL, interactions=NULL, quadratics=NULL, ... )
{
    # processing
    res <- mice_imputation_weighted_norm_prepare( x=x, ry=ry, y=y,
                imputationWeights=imputationWeights, interactions=interactions,
                quadratics=quadratics, pls.facs=pls.facs, pls.impMethod="norm",    ... )
    yimp <- res$yimp
    pls.facs <- res$pls.facs
    yobs <- res$yobs
    xobs <- res$xobs
    weights.obs <- res$weights.obs
    x <- res$x

    #** norm draw
    if ( is.null(pls.facs) ){
        parm <- mice_imputation_weighted_norm_draw( yobs=yobs, xobs=xobs,
                        ry=ry, y=y, x=x, weights.obs=weights.obs, ... )
        wy <- mice_imputation_define_wy(wy=wy, ry=ry)
        yimp <- x[wy,  ] %*% parm$beta + stats::rnorm(sum(wy)) * parm$sigma
        yimp <- yimp[,1]
    }
    return(yimp)
}



