## File Name: lm.cluster.R
## File Version: 0.422



#-- linear model for clustered data
lm.cluster <- function( data, formula, cluster, weights=NULL, subset=NULL )
{
    #*** to be included in future versions
    if (FALSE){
        X <- stats::model.matrix( object=formula, data=data )
        t1 <- stats::terms(formula)
        resp_var <- paste( t1[[2]] )
        sel_X <- as.numeric( dimnames(X)[[1]] )
        y <- data[ sel_X, resp_var ]
        weights <- weights[ sel_X ]
        sel <- which( ! is.na(y) )
        weights <- weights[sel]
        X <- X[sel,, drop=FALSE]
        y <- y[sel]
        mod <- stats::lm.wfit(x=X, y=y, w=weights)
    }

    #- handle subset
    pos <- parent.frame()
    res <- lm_cluster_subset(data=data, cluster=cluster, weights=weights,
                    subset=subset, pos=pos)
    data <- res$data
    cluster <- res$cluster
    wgt__ <- res$wgt__

    #-- fit linear model
    mod <- stats::lm( data=data, formula=formula, weights=wgt__)

    #-- compute standard errors
    vcov2 <- lm_cluster_compute_vcov(mod=mod, cluster=cluster, data=data)

    #-- output
    res <- list( lm_res=mod, vcov=vcov2 )
    class(res) <- "lm.cluster"
    return(res)
}

coef.lm.cluster <- function( object, ... )
{
    return( coef(object$lm_res) )
}

vcov.lm.cluster <- function( object, ... )
{
    return(object$vcov)
}

summary.lm.cluster <- function( object, ... )
{
    smod <- summary( object$lm_res )
    csmod <- smod$coefficients
    csmod[,"Std. Error"] <- sqrt( diag( vcov(object) ))
    csmod[,"t value"] <-  csmod[,"Estimate"] / csmod[,"Std. Error"]
    csmod[,"Pr(>|t|)"] <- stats::pnorm( - abs( csmod[,"t value"] ) )*2
    R2 <- smod$r.squared
    cat("R^2=", round(R2, 5),"\n\n" )
    print(csmod)
    invisible(csmod)
}
