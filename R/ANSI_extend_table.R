## File Name: ANSI_extend_table.R
## File Version: 0.19

###########################################
ANSI_extend_table <- function( data, vars, subset, varindex="varindex",
            varname="value")
{
    x <- data
    # subset of a dataset
    r <- if (missing(subset)){
        rep_len(TRUE, nrow(x))
    } else {
        e <- substitute(subset)
        r <- eval(e, x, parent.frame())
        if (!is.logical(r))
            stop("'subset' must be logical")
        r & !is.na(r)
    }
    x <- x[r,]
    data <- x

    NV <- length(vars)
    vars0 <- setdiff( colnames(data), vars )
    dfr <- NULL
    NC <- length(vars0)
    for (vv in 1:NV){
        # vv <- 1
        dfr0 <- data.frame( data[, vars0], vars[vv], data[, vars[vv] ] )
        colnames(dfr0)[ NC + 1:2 ] <- c(varindex, varname )
        dfr <- rbind( dfr, dfr0)
    }
    return(dfr)
}
############################################
