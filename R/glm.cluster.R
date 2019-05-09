## File Name: glm.cluster.R
## File Version: 0.297



#-- linear model for clustered data
glm.cluster <- function( data, formula, cluster, weights=NULL, subset=NULL,
        family="gaussian")
{

    #- handle subset
    pos <- parent.frame()
    res <- lm_cluster_subset(data=data, cluster=cluster, weights=weights,
                    subset=subset, pos=pos)
    data <- res$data
    cluster <- res$cluster
    wgt__ <- res$wgt__

    #-- fit generalized linear model
    mod <- stats::glm( data=data, formula=formula, weights=wgt__, family=family)

    #-- adjust standard errors
    vcov2 <- lm_cluster_compute_vcov(mod=mod, cluster=cluster, data=data)

    #-- output
    res <- list( glm_res=mod, vcov=vcov2 )
    class(res) <- "glm.cluster"
    return(res)
}

coef.glm.cluster <- function( object, ... )
{
    return( coef(object$glm_res) )
}

vcov.glm.cluster <- function( object, ... )
{
    return(object$vcov)
}

summary.glm.cluster <- function( object, ... )
{
    smod <- summary(object$glm_res )
    csmod <- smod$coefficients
    csmod[,2] <- sqrt( diag(object$vcov) )
    csmod[,3] <- csmod[,1] / csmod[,2]
    csmod[,4] <- stats::pnorm( - abs( csmod[,3] ) )*2
    print(csmod)
    invisible(csmod)
}
