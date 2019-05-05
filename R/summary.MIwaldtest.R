## File Name: summary.MIwaldtest.R
## File Version: 0.06


summary.MIwaldtest <- function(object, digits=4,...)
{
    require_namespace(pkg="CDM")
    cat("Wald Test\n")
    obji <- object$stat
    CDM::cdm_print_summary_data_frame(obji, from=1, digits=digits)
    cat("\nLinear Hypotheses\n")
    summaryMIwaldtest_linear_hyp(object$linear_hyp, digits=digits)
}
