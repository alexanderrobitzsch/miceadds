## File Name: mice_imputation_weighted_norm_prepare.R
## File Version: 0.112

mice_imputation_weighted_norm_prepare <- function(x, ry, y, imputationWeights,
    interactions, quadratics, pls.facs, pls.impMethod, ... )
{
    x <- cbind(1, as.matrix(x))
    xobs <- x[ry,]
    yobs <- y[ry]
    if ( is.null( imputationWeights ) ){
        imputationWeights <- rep(1, length(y) )
    }
    weights.obs <- imputationWeights[ ry   ]
    # standardize all weights to one
    weights.obs <- length(weights.obs) * weights.obs / sum( weights.obs )

    # PLS interactions and quadratics
    res <- mice_imputation_get_states(pos=parent.frame(n=2) )
    newstate <- res$newstate
    vname <- res$vname

    plsout <- mice_imputation_pls_helper( newstate=newstate, vname=vname,
                    pls.impMethod=pls.impMethod,
                    x=x[,-1], y=y, ry=ry, imputationWeights=imputationWeights,
                    interactions=interactions, quadratics=quadratics, pls.facs=pls.facs,
                    ... )
    pls.facs <- plsout$pls.facs
    yimp <- plsout$yimp

    #---- output
    res <- list( yimp=yimp, pls.facs=pls.facs, yobs=yobs, xobs=xobs,
                weights.obs=weights.obs, x=x)
    return(res)
}
