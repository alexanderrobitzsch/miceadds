## File Name: nested.datlist_create.R
## File Version: 1.131


#***** create nested.datlist
nested.datlist_create <- function(datasets)
{
    CALL <- match.call()
    if ( inherits(datasets, c("mids.nmi")) ){
        datasets <- mids2datlist(datasets)
    }
    if ( inherits(datasets, c("NestedImputationList")) ){
        datasets <- datasets$imputations
    }
    v1 <- c("between"=length(datasets), "within"=length(datasets[[1]]) )
    class(datasets) <- "nested.datlist"
    attr(datasets,"Nimp") <- v1
    attr(datasets,"call") <- CALL
    attr(datasets,"nobs") <- nrow(datasets[[1]][[1]])
    h1 <- lapply( datasets, FUN=function(dd){
         lapply( dd, FUN=function(ee){ nrow(ee) } )
        } )
    attr(datasets,"nobs_datasets") <- h1
    attr(datasets,"nobs") <- round( mean( unlist( attr(datasets,"nobs_datasets") ) ), 2 )
    attr(datasets,"nvars") <- ncol(datasets[[1]][[1]])
    attr(datasets,"variables") <- colnames(datasets[[1]][[1]])
    return(datasets)
}


#**************** print method ***********************
print.nested.datlist <- function(x,...)
{
    cat("Object of class 'nested.datlist'\nCall: ")
    print( attr(x,"call"))
    v1 <- paste0( "NMI data with ", attr(x,"Nimp")[1]," between datasets and ",
                attr(x,"Nimp")[2], " within datasets\n")
    cat(v1)
    v1 <- paste0( attr(x,"nobs"), " cases and ",
    attr(x,"nvars"), " variables \n" )
    cat(v1)
}


