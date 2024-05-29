## File Name: VariableNames2String.R
## File Version: 0.131


VariableNames2String <- function( vars, breaks=80, sep=" ")
{
    vars2 <- vars
    # define breaks
    NS <- length(sep)
    V <- length(vars)
    dfr <- data.frame( index=1L:V, variable=vars, nchar=nchar(vars), nsep=NS )
    dfr$sum1 <- dfr$nchar + dfr$nsep
    dfr$sum2 <- cumsum(dfr$sum1)
    dfr$sum3 <- 0
    dfr$line <- 1

    ii <- 1
    cum_index <- dfr$sum2[ii]
    line_ii <- dfr$line[ii]
    vars2 <- paste0( vars[ii], sep )
    for (ii in 2L:V){
        cum_index <- cum_index + dfr$sum1[ii]
        if ( cum_index > breaks){
            line_ii <- line_ii + 1
            cum_index <- dfr$sum1[ii]
            vars2 <- paste0( vars2, '\n' )
        }
        dfr$line[ii] <- line_ii
        dfr$sum3[ii] <- cum_index
        vars2 <- paste0( vars2, vars[ii], sep )
    }
    return(vars2)
}
