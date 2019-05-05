## File Name: summary.lmer_vcov.R
## File Version: 0.07


summary.lmer_vcov <- function( object, digits=4, file=NULL, ...)
{
    require_namespace(pkg="CDM")
    # open sink
    CDM::osink( file=file, suffix=paste0( "__SUMMARY.Rout") )

    cat("Inference for Fitted Model in lme4\n\n")
    obji <- object$par_summary
    CDM::cdm_print_summary_data_frame(obji, digits=digits, from=2)

    # close sink
    CDM::csink( file=file )
}
