## File Name: ml_mcmc_proc_data.R
## File Version: 0.17

ml_mcmc_proc_data <- function( formula, data, outcome, verbose=TRUE)
{
    #*** formula processing
    res <- ma_lme4_formula_design_matrices(formula=formula, data=data, start_index=0)
    res$verbose <- verbose
    #*** settings depending on outcome
    est_normal <- FALSE
    est_probit <- FALSE
    est_sigma2 <- TRUE
    K <- 9999
    est_thresh <- FALSE

    #- probit outcome
    if (outcome=="probit"){
        est_probit <- TRUE
        est_sigma2 <- FALSE
        K <- max(res$y)
        if (K>1){
            est_thresh <- TRUE
            res$parnames$thresh <- paste0("thresh",2:K)
        }
    }
    #- normal outcome
    if (outcome=="normal"){
        est_normal <- TRUE
    }
    #- attach to list
    res$est_probit <- est_probit
    res$est_sigma2 <- est_sigma2
    res$K <- K
    res$est_thresh <- est_thresh
    res$est_normal <- est_normal

    if (! est_sigma2){
        res$parnames$sigma2 <- NULL
    }

    #--- output
    return(res)
}

