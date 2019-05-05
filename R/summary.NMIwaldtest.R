## File Name: summary.NMIwaldtest.R
## File Version: 0.04



summary.NMIwaldtest <- function(object, digits=4,...)
{
    require_namespace(pkg="CDM")
    cat("Wald Test\n")
    obji <- object$stat
    CDM::cdm_print_summary_data_frame(obji, from=1, digits=digits)
    cat("\nLinear Hypotheses\n")
    summary(object$linear_hyp, digits=digits)
}
