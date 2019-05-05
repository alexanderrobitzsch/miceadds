## File Name: summary.ml_mcmc.R
## File Version: 0.15


summary.ml_mcmc <- function( object, digits=4, file=NULL, ...)
{
    require_namespace(pkg="sirt")
    require_namespace(pkg="CDM")

    # open sink
    CDM::osink( file=file, suffix=paste0( "__SUMMARY.Rout") )

    cat("-----------------------------------------------------------------\n")
    #- package and R session
    sirt::sirt_summary_print_package_rsession(pack="miceadds")

    cat( "Date of Analysis:", paste(object$s2), "\n" )
    cat("Computation Time:", print(object$s2 - object$s1), "\n\n")

    #- print call
    sirt::sirt_summary_print_call(CALL=object$CALL)

    cat("-----------------------------------------------------------------\n")
    cat( "Number of iterations", "=", object$iter, "\n" )
    cat( "Number of burnin iterations", "=", object$burnin, "\n\n" )

    #-- some descriptive informations
    ml_mcmc_summary_print_descriptives(object=object)

    cat("-----------------------------------------------------------------\n")
    cat("Model Parameters\n")
    obji <- object$par_summary
    CDM::cdm_print_summary_data_frame(obji, digits=digits, from=2)

    # close sink
    CDM::csink( file=file )
}
#*******************************************************
