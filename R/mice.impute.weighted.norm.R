## File Name: mice.impute.weighted.norm.R
## File Version: 0.19
mice.impute.weighted.norm <- function(y, ry, x, ridge=.00001, pls.facs=NULL,
                    imputationWeights=NULL,
                    interactions=NULL, quadratics=NULL,
                     ...){
    x <- cbind(1, as.matrix(x))
    xobs <- x[ry,]
    yobs <- y[ry]
    if ( is.null( imputationWeights ) ){
        imputationWeights <- rep(1, length(y) )
    }
    weights.obs <- imputationWeights[ ry   ]
    # standardize all weights to one
    weights.obs <- length(weights.obs) * weights.obs / sum( weights.obs )

    #.+.+.+.+.+.+.+.+.+.+.+.+
    # PLS interactions and quadratics
    newstate <- get( "newstate", pos=parent.frame() )
    vname <- get("vname", pos=parent.frame() ) # get variable name
    plsout <- mice_imputation_pls_helper( newstate=newstate, vname=vname, pls.impMethod="norm",
                    x=x[,-1], y=y, ry=ry, imputationWeights=imputationWeights,
                    interactions=interactions, quadratics=quadratics,  pls.facs=pls.facs,  ... )
    pls.facs <- plsout$pls.facs
    yimp <- plsout$yimp

    if ( is.null(pls.facs) ){
        parm <- mice_imputation_weighted_norm_draw( yobs=yobs, xobs=xobs,
                        ry=ry, y=y, x=x,
                        weights.obs=weights.obs, ... )
        yimp <- x[!ry,  ] %*% parm$beta + stats::rnorm(sum(!ry)) * parm$sigma
    }
    return(yimp)
}



