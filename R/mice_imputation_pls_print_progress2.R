## File Name: mice_imputation_pls_print_progress2.R
## File Version: 0.06

mice_imputation_pls_print_progress2 <- function(pls.print.progress, imp.temp,
    pls.title, y, x)
{
    if( pls.print.progress){
        cat(" Imputation: ", imp.temp$im, ", Iteration:", imp.temp$it   )
        if ( ! is.null(pls.title)){
            cat("\n ",pls.title)
        }
        cat( "\n\nImputation using Partial Least Squares Regression\n")
        cat( substring(Sys.time(),1),"\n" )
        utils::flush.console()
        cat( "\n", paste( ncol(x), "Predictor Variables", names(y) ), "\n")
        cat("Used Variables ", paste(colnames(x),collapse=" "), "\n", sep="" )
    }
}

