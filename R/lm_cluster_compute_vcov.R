## File Name: lm_cluster_compute_vcov.R
## File Version: 0.01

lm_cluster_compute_vcov <- function(mod, cluster, data)
{
    require_namespace("sandwich")
    if ( length(cluster) > 1 ){
        v1 <- cluster
    } else {
        v1 <- data[,cluster, drop=TRUE]
    }
    dfr <- data.frame( cluster=v1 )
    vcov2 <- sandwich::vcovCL( x=mod, cluster=dfr$cluster)
    return(vcov2)
}
