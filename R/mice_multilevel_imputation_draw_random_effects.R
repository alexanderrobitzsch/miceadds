## File Name: mice_multilevel_imputation_draw_random_effects.R
## File Version: 0.135

mice_multilevel_imputation_draw_random_effects <- function( mu, Sigma, ridge=1E-20 )
{
    dim_Sigma <- dim(Sigma)
    ngr <- dim_Sigma[3]
    NR <- dim_Sigma[1]
    # draw random effects
    u <- matrix(0, nrow=ngr, ncol=NR)
    if (NR==1){
        u[,1] <- stats::rnorm(ngr, mean=mu[,1], sd=sqrt(Sigma[1,1,]) )
    } else {  # NR > 1
        for(i in 1L:ngr){
            #-- compute covariance matrix with ridge
            Sigma1 <- Sigma[,,i] + diag(ridge,NR)
            # Cholesky matrix of Sigma1
            Sigma_chol <- chol(Sigma1)
            # simulation
            rn <- stats::rnorm(NR, mean=0, sd=1)
            u[i,] <- mu[i,] + as.vector( crossprod( Sigma_chol, rn) )
        }
    }
    return(u)
}
