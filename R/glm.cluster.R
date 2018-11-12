## File Name: glm.cluster.R
## File Version: 0.25


##################################################
# linear model for clustered data
glm.cluster <- function( data, formula, cluster, ... )
{
    TAM::require_namespace_msg("multiwayvcov")
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
###################################################
coef.glm.cluster <- function( object, ... )
{
    return( coef(object$glm_res) )
}
####################################################
vcov.glm.cluster <- function( object, ... )
{
    return(object$vcov)
}
####################################################
summary.glm.cluster <- function( object, ... )
{
    smod <- summary(object$glm_res )
    csmod <- smod$coefficients
    csmod[,2] <- sqrt( diag(object$vcov) )
    csmod[,3] <- csmod[,1] / csmod[,2]
    csmod[,4] <- stats::pnorm( - abs( csmod[,3] ) )*2
    # R2 <- smod$r.squared
    # cat("R^2=", round(R2, 5),"\n\n" )
    print(csmod)
    invisible(csmod)
}
#######################################################
