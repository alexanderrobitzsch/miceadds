## File Name: datlist2Amelia.R
## File Version: 0.07

datlist2Amelia <- function(datlist)
{
    if (inherits(datlist,"mids")){
        datlist <- mids2datlist(midsobj=datlist)
    }
    # define results object
    m <- length(datlist)
    names(datlist) <- paste0("imp",1:m)
    res <- list(imputations=datlist)
    res$m <- m
    res$orig.vars <- colnames(datlist[[1]])
    # define missing indicator matrix
    res$missMatrix <- ! extract_response_matrix(datlist=datlist)
    #- output
    class(res) <- "amelia"
    return(res)
}
