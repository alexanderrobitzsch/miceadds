## File Name: summary.NMIwaldtest.R
## File Version: 0.02


##############################################################
summary.NMIwaldtest <- function(object, digits=4,...)
{
    cat("Wald Test\n")
    obji <- object$stat
    CDM::cdm_print_summary_data_frame(obji, from=1, digits=digits)
    cat("\nLinear Hypotheses\n")
    summary(object$linear_hyp, digits=digits)
}
###############################################################
