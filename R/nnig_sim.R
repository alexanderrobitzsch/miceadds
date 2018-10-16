## File Name: nnig_sim.R
## File Version: 0.20

nnig_sim <- function(N, coef)
{
    p <- coef$p
    X <- matrix(NA, nrow=N, ncol=p)
    for (ii in 1:p){
        X[,ii] <- fleishman_sim(N=N, coef=coef$fcoef[ii,] )
    }
    A <- coef$A
    Y <- matrix(0, nrow=N, ncol=p)
    colnames(Y) <- paste0("Y", 1:coef$p)
    for (ii in 1:p){
        for (hh in 1:p){
            Y[,ii] <- Y[,ii] + A[ii,hh] * X[,hh]
        }
    }
    #- output
    return(Y)
}
