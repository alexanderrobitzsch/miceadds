## File Name: ma_rhat.R
## File Version: 0.03

ma_rhat <- function(x, n_chains=3)
{
    nv <- ncol(x)
    rhat <- rep(NA,nv)
    NC <- n_chains
    NV <- nrow(x)
    NV0 <- NV - ( ( NV ) %% ( NC ) )
    for (vv in 1:nv){
        v1 <- x[,vv]
        mat <- matrix( v1[1:NV0], ncol=NC )
        rhat[vv] <- Rhat1(mat)
    }
    return(rhat)
}
