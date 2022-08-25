## File Name: ml_mcmc_summary_print_descriptives.R
## File Version: 0.04

ml_mcmc_summary_print_descriptives <- function(object)
{
    cat("Outcome model:", object$outcome, "\n")
    cat("Number of observations", "=", object$N, "\n")
    ncluster_list <- object$ncluster_list
    NCL <- length(ncluster_list)
    for (cc in seq_len(NCL)){
        cat( paste0("Number of clusters (", names(ncluster_list)[cc],")"), "=",
                        ncluster_list[[cc]], "\n")
    }
    cat("\n")
}
