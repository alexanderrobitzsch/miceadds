## File Name: datlist_create.R
## File Version: 0.341


### create datlist
datlist_create <- function(datasets)
{
    CALL <- match.call()
    if ( inherits( datasets, "mids") | inherits( datasets, "mids.1chain") ){
        datasets <- mids2datlist(datasets)
    }
    if ( inherits(datasets, "imputationList" ) ) {
        datasets <- datasets$imputations
    }
    class(datasets) <- "datlist"
    attr(datasets,"Nimp") <- length(datasets)
    attr(datasets,"call") <- CALL
    attr(datasets,"nobs") <- nrow(datasets[[1]])
    attr(datasets,"nobs_datasets") <-
            lapply( datasets, FUN=function(dd){ nrow(dd) } )
    attr(datasets,"nobs") <- round( mean( unlist(
                attr(datasets,"nobs_datasets") ) ), 2 )
    attr(datasets,"nvars") <- ncol(datasets[[1]])
    attr(datasets,"variables") <- colnames(datasets[[1]])
    return(datasets)
}
#**************** print method ***********************
print.datlist <- function(x,...)
{
    cat("Object of class 'datlist'\nCall: ")
    print( attr(x,"call"))
    cat("MI data with", attr(x,"Nimp"),"datasets\n")
    v1 <- paste0( attr(x,"nobs"), " cases and ",
    attr(x,"nvars"), " variables \n" )
    cat(v1)
}


