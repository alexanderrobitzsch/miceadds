## File Name: mice.impute.weighted.pmm.R
## File Version: 0.14
mice.impute.weighted.pmm <- function (y, ry, x,  imputationWeights=NULL,
                                    pls.facs=NULL,  interactions=NULL, quadratics=NULL,  ...){
    x <- cbind(1, as.matrix(x))
    # .weighted.norm.draw
    xobs <- x[ry,]
    yobs <- y[ry]
    if ( is.null( imputationWeights ) ){ imputationWeights <- rep(1, length(y) ) }
    weights.obs <- imputationWeights[ ry   ]
    # standardize all weights to one
    weights.obs <- length(weights.obs) * weights.obs / sum( weights.obs )
    #.+.+.+.+.+.+.+.+.+.+.+.+
    # PLS interactions and quadratics
    res <- mice_imputation_get_states(pos=parent.frame(n=2) )
    newstate <- res$newstate
    vname <- res$vname    
    plsout <- mice_imputation_pls_helper( newstate=newstate, vname=vname, pls.impMethod="pmm",
                    x=x[,-1], y=y, ry=ry, imputationWeights=imputationWeights,
                    interactions=interactions, quadratics=quadratics,  pls.facs=pls.facs,  ... )
    # save PLS result
    pls.facs <- plsout$pls.facs
    yimp <- plsout$yimp
    if (is.null(pls.facs)){
            parm <- .weighted.norm.draw( yobs=yobs, xobs=xobs, ry=ry, y=y, x=x,
                                weights.obs=weights.obs, ... )
            yhatobs <- x[ry,] %*% parm$coef
            yhatmis <- x[!ry,] %*% parm$beta
            yimp <- apply(as.array(yhatmis), 1, .pmm.match, yhat=yhatobs,
                                y=y[ry], ... )
                            }
    return(yimp)

    }
