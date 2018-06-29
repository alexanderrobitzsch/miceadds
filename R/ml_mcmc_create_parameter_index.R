## File Name: ml_mcmc_create_parameter_index.R
## File Version: 0.08

ml_mcmc_create_parameter_index <- function(beta, Psi_list, est_sigma2, est_thresh, K)
{
    parameter_index <- list()
    est_parameter <- list()

    #--- beta
    npar <- length(beta)
    parameter_index$beta <- 1:npar - 1
    est_parameter$beta <- TRUE

    #--- Psi
    NR <- length(Psi_list)
    psi_parameter_index <- list()
    for (rr in seq_len(NR) ){
        res <- ml_mcmc_parameter_index_matrix(NM=nrow(Psi_list[[rr]]))
        matr <- res$matr + ( npar - 1 )
        matr[ is.na(matr) ] <- -9
        psi_parameter_index[[rr]] <- matr
        npar <- npar + res$np
    }
    parameter_index$Psi <- psi_parameter_index
    est_parameter$Psi <- TRUE

    #--- sigma2
    parameter_index$sigma2 <- NULL
    if ( est_sigma2 ){
        parameter_index$sigma2 <- npar
        npar <- npar + 1
    }
    est_parameter$sigma2 <- est_sigma2

    #--- thresh
    if ( est_thresh ){
        parameter_index$thresh <- npar - 1 + seq_len( K - 1 )
        npar <- npar + K - 1
    }
    est_parameter$thresh <- est_thresh

    #*** output
    res <- list( npar=npar, parameter_index=parameter_index,
                    est_parameter=est_parameter)
    return(res)
}
