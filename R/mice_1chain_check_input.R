## File Name: mice_1chain_check_input.R
## File Version: 0.01

mice_1chain_check_input <- function(burnin, iter, Nimp)
{
    if (burnin <= 0){
        burnin <- 1
    }
    if (iter <= burnin + Nimp ){
        iter <- burnin + Nimp
    }    
    #--- output
    res <- list( burnin=burnin, iter=iter, Nimp=Nimp)
    return(res)
}
