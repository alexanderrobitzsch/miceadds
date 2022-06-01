## File Name: with.mids.nmi.R
## File Version: 0.11


#### with function for objects of class mids
with.mids.nmi <- function(data, expr, ...)
{
    # adapted from with.mids.1chain
    call <- match.call()
    if (! inherits(data,"mids.nmi")){
        stop("The data must have class mids.nmi")
    }
    Nimp <- data$Nimp
    # data <- data$imp
    anal <- as.list( 1:(Nimp["between"]) )
    for (bb in 1:(Nimp["between"]) ){
        analyses <- as.list( 1:(Nimp["within"]) )
        # do the repeated analysis, store the result.
        for (i in 1:(Nimp["within"])  ) {
            data.i <- complete.mids.nmi(data, c(bb,i) )
            analyses[[i]] <- eval( expr=substitute(expr), envir=data.i,
                        enclos=parent.frame())
            if (is.expression(analyses[[i]])){
                analyses[[i]] <- eval(expr=analyses[[i]],
                            envir=data.i, enclos=parent.frame())
            }
        }
        anal[[bb]] <- analyses
    }

    # return the complete data analyses as a list of length nimp
    object <- list(call=call, call1=data$imp$call, analyses=anal, Nimp=Nimp)
    class(object) <- "mira.nmi"
    return(object)
}
