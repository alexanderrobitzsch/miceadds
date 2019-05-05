## File Name: glm.cluster.R
## File Version: 0.26



#-- linear model for clustered data
glm.cluster <- function( data, formula, cluster, ... )
{
    require_namespace("multiwayvcov")
    mod <- stats::glm( data=data, formula=formula,  ... )
    if ( length(cluster) > 1 ){
        v1 <- cluster
    } else {
        v1 <- data[,cluster]
    }
    dfr <- data.frame( cluster=v1 )
    vcov2 <- multiwayvcov::cluster.vcov( model=mod, cluster=dfr)
    res <- list( "glm_res"=mod, "vcov"=vcov2 )
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
