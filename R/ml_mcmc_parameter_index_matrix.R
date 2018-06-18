## File Name: ml_mcmc_parameter_index_matrix.R
## File Version: 0.01

ml_mcmc_parameter_index_matrix <- function(NM)
{
    m2 <- matrix(NA, nrow=NM, ncol=NM)
    hh <- 0
    for (mm in 1:NM){
        m2[mm,1:mm] <- hh + ( 1:mm )
        hh <- hh + mm
    }
    #--- output
    res <- list( np=hh, matr=m2)
    return(res)
}
